require(quanteda)
require(spacyr)
require(tidyverse)
require(Matrix)
require(igraph)

tsai_articles <- readRDS('tsai_nyt_articles.RDS') %>% as_tibble

spacy_initialize()

parsed_text <- spacy_parse(tsai_articles$content, dependency = TRUE) %>% as_tibble

parsed_text %>% mutate(sentence_id = paste(doc_id, sentence_id, sep = "_")) -> parsed_text

tsai_sentences <- tokens(tsai_articles$content, what = 'sentence') %>% unlist %>% str_subset("Tsai") %>% str_subset("Tsai administration", negate = TRUE)

set.seed(12121)
tibble(tsai_sentences, wc = ntoken(tsai_sentences)) %>% arrange(desc(wc)) %>% sample_n(size = 50, weight = wc) -> tsai_random_sentences

tsai_random_sentences$tsai_sentences %>% corpus %>% dfm(dictionary = data_dictionary_LSD2015) %>% convert(to = 'data.frame') %>% mutate(wc = ntoken(recon)) %>% mutate(nettone = ((positive - neg_positive) / wc) - ((negative - neg_negative) / wc))

###tsai_random_sentences %>% rio::export('tsai_sample.csv')


#' parsed_str: parsed articles in a list of dataframes.
#' kw: keywords of the actor
#' sent: raw text of the article. For sent or parsed_str, only one of them should be null.
#' attributive: extract or not attrbutive words (e.g. "black" from "black cats") associated with the kw
extract_associated_word_sent <- function(sent = NULL, kw, parsed_str = NULL, attributive = FALSE) {
    if (!is.null(sent)) {
        parsed_str <- spacy_parse(sent, pos = TRUE, tag = TRUE, dependency = TRUE)
    }
    check <- parsed_str[, c("token_id", "lemma", "pos", "dep_rel")]
    parsed_str[,c("head_token_id", "token_id")] %>% left_join(check, by = c('head_token_id' = 'token_id')) %>% left_join(check, by = "token_id") -> rel_data
    rel_data %>% filter(str_detect(lemma.x, kw)) %>% pull(lemma.y) -> label_attached
    rel_data %>% filter(str_detect(lemma.y, kw) & dep_rel.y == "nsubj") %>% pull(lemma.x) -> active_verbs
    if (attributive) {
        words_associated <- c(label_attached, active_verbs)
    } else {
        words_associated <- c(active_verbs)
    }
    if (length(words_associated) == 0) {
        return(c(""))
    }
    return(words_associated)
}
spacy_initialize()

extract_associated_word_sent(sent = tsai_random_sentences$tsai_sentences[1], kw = "Tsai", attributive = FALSE)

tsai_verbs <- map(tsai_random_sentences$tsai_sentences, extract_associated_word_sent, kw = "Tsai")

tsai_verbs_att <- map(tsai_random_sentences$tsai_sentences, extract_associated_word_sent, kw = "Tsai", attributive = TRUE)

tsai_verbs %>% map_chr(paste, collapse = " ") %>% dfm(dictionary = data_dictionary_LSD2015) %>% convert(to = "data.frame")

tsai_verbs_att %>% map_chr(paste, collapse = " ") %>% dfm(dictionary = data_dictionary_LSD2015) %>% convert(to = "data.frame")

