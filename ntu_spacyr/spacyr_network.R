require(quanteda)
require(spacyr)
require(tidyverse)
require(Matrix)
require(igraph)

tsai_articles <- readRDS('tsai_nyt_articles.RDS')

parsed_text <- spacy_parse(tsai_articles$content, dependency = TRUE)

### extract all entities (with at least two occurences)

entity_extract(parsed_text) %>% filter(entity_type == "PERSON") %>% 
    count(entity, sort = TRUE) %>% filter(n >= 2) %>% print(n = 300)

## clean up Trump and Obama: should check errors / wrong extractions
entity_extract(parsed_text) %>% filter(entity_type == "PERSON") %>% 
    mutate(entity = ifelse(entity %in% c("Trump", "Donald_Trump"), 
                           "Donald_J._Trump", entity)) %>% 
    mutate(entity = ifelse(entity %in% c("Obama"), "Barack_Obama", entity)) %>% 
    mutate(entity = str_replace(entity, "_'s$", "")) %>% 
    filter(str_detect(entity, "_")) %>% 
    select(entity) %>% distinct


excluded_entities <- c("Anti_-_L.G.B.T.Q.", "Cloud_Gate", "Level_2", "Treating_Lee_Ming_-", "Troop_8341", "Wan_Chai", "Seeking_Truth", "Hong_Kongers", "Lamb_of_God", "Burkina_Faso", "Humpty_Dumpty", "Saõ_Tomé", "Sao_Tomé",  "Pro_-_Taiwan", "Hsiung_Feng_III")


entity_extract(parsed_text) %>% filter(entity_type == "PERSON") %>% 
    mutate(entity = ifelse(entity %in% c("Trump", "Donald_Trump"), "Donald_J._Trump", entity)) %>% 
    mutate(entity = ifelse(entity %in% c("Obama"), "Barack_Obama", entity)) %>% 
    mutate(entity = str_replace(entity, "_'s$", "")) %>% 
    filter(str_detect(entity, "_")) %>% 
    filter(str_detect(entity, "^Q\\.", negate = TRUE)) %>% 
    filter(!entity %in% excluded_entities) %>% 
    group_by(doc_id, entity) %>% tally %>% ungroup %>% 
    select(-n) -> affliation_matrix

## Two mode network / bipartite network

affliation_matrix

### Two-to-one transformation.

A <- spMatrix(nrow=length(unique(affliation_matrix$entity)),
              ncol=length(unique(affliation_matrix$doc_id)),
              i = as.numeric(factor(affliation_matrix$entity)),
              j = as.numeric(factor(affliation_matrix$doc_id)),
              x = rep(1, length(affliation_matrix$entity)))

row.names(A) <- levels(factor(affliation_matrix$entity))
colnames(A) <- levels(factor(affliation_matrix$doc_id))

### The transposed cross product of the afflication matrix is the co-occurence matrix.
tcrossprod(A)

tsai_graph <- graph.adjacency(tcrossprod(A), "undirected", weighted = TRUE, diag = FALSE)


V(tsai_graph)$betweenness <- betweenness(tsai_graph)

delete.vertices(tsai_graph, which(V(tsai_graph)$betweenness <= 200)) -> best

plot(best, vertex.size = log(V(best)$betweenness), 
     vertex.color = membership(cluster_walktrap(best)))


