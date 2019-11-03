require(quanteda)
require(stm)
require(tidyverse)

ted_raw <- rio::import("ted_main.csv")
ted_raw %>% select(views, comments) -> ted_meta_data

ted_dfm <- corpus(ted_raw$description, docvars = ted_meta_data) %>% dfm(remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("english")) %>% dfm_trim(min_docfreq = 5 / 2250, max_docfreq = 0.90, docfreq_type = "prop")

ted_stm_data <- quanteda::convert(ted_dfm, to = 'stm', docvars = docvars(ted_dfm))

### Naive one

set.seed(42)
ted_stm_naive <- stm(ted_stm_data$documents, ted_stm_data$vocab, K = 0, data = ted_stm_data$meta, init.type = "Spectral")

labelTopics(ted_stm_naive)

ted_stm_theta <- ted_stm_naive$theta

cor.test(ted_raw$views, ted_stm_theta[,65])

### let's do a better one.

ted_stm_better <- stm(ted_stm_data$documents, ted_stm_data$vocab, K = 20, data = ted_stm_data$meta, init.type = "Spectral", prevalence =~ log(views))

ted_labels <- labelTopics(ted_stm_better, n = 20)

ted_effect <- estimateEffect(formula = 1:20~log(views), stmobj = ted_stm_better, meta = ted_stm_data$meta, uncertainty = "Global")

summary(ted_effect)

## 11, 7, 5, 2
## 14, 8, 4

ted_labels$frex[4,]
