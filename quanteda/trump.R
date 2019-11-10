require(tidyverse)
require(quanteda)
require(rio)
require(lubridate)

## trump_tweets <- import('trump.json') %>% as_tibble

## ### preprocessing

## trump_tweets %>% mutate(is_retweet = is_retweet == "true", created_at = parse_date_time(created_at, orders = '%a %b %d %H:%M:%S %z %Y')) -> trump_tweets

## trump_tweets %>% group_by(source) %>% count

## trump_tweets %>% filter(str_detect(source, "iPhone|Android")) -> trump_tweets
## saveRDS(trump_tweets, "trump_tweets.RDS")
### quanteda

trump_tweets <- readRDS("trump_tweets.RDS")
trump_corpus <- corpus(trump_tweets$text)
docvars(trump_corpus, "source") <- str_detect(trump_tweets$source, "Android")
docvars(trump_corpus, "created_at") <- trump_tweets$created_at

## YOUR TURN: add one more docvars - retweet_count

summary(trump_corpus)

## KWIC keyword in context

kwic(trump_corpus, "bush")

kwic(trump_corpus, "cruz")

## YOUR TURN: kwic hillary

## Corpus -> dfm

trump_dfm <- dfm(trump_corpus)

topfeatures(trump_dfm, 100) ## WTF

?dfm
?tokens

trump_dfm <- dfm(trump_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
topfeatures(trump_dfm, 100)

stopwords("en")

trump_dfm <- dfm(trump_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove = stopwords('en'))

topfeatures(trump_dfm, 100)

textplot_wordcloud(trump_dfm, min_count = 300, random_order = FALSE)

## FYI: explore remove_twitter

## keyness

textstat_keyness(trump_dfm, docvars(trump_dfm, "source"), sort = TRUE) %>% head(n = 100)

textstat_keyness(trump_dfm, docvars(trump_dfm, "source"), sort = TRUE) %>% textplot_keyness


dfm(trump_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove = stopwords('en'), groups = "source") %>% textplot_wordcloud(comparison = TRUE)

## Dictionary-based method: introduction

kwic(trump_corpus, "me")

### because stopwords include pronouns...

trump_dfm2 <- dfm(trump_corpus, remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)

pronouns <- dictionary(list(
    first_singular = c("i", "me", "my", "mine", "myself"),
    second = c("you", "your", "yours", "yourself", "yourselves"),
    first_plural = c('we', 'us', 'our', 'ours', 'ourselves'),
    third_masculine = c('he', 'him', 'his', 'himself'),
    third_feminine = c('she', 'her', 'hers', 'herself'),
    other = c('it', 'its', 'itself', 'they', 'them', 'their', 'themselves', 'themself')
))

trump_pronouns <- dfm_lookup(trump_dfm2, dictionary = pronouns)

trump_corpus[1]
trump_pronouns[1,]

textstat_keyness(trump_pronouns, docvars(trump_pronouns, "source"), sort = TRUE)

## Your Turn: Create a dictionary of
## 1. populism (Rooduijn & Pauwels 2011) with these words:
## c('elit*', 'consensus*', 'undemocratic*', 'referend*', 'corrupt*', 'propagand*', 'politici*', '*deceit*', '*deceiv*', '*betray*', 'shame*', 'scandal*', 'truth*', 'dishonest*', 'establishm*', 'ruling*')
## 2. Terrorism
## c('terror*')
## 3. Tax
## c('tax*')
## And study the difference of these words in Trump's tweets from Android and iPhone


## off-the-shelf dictionary
## validate

afinn <- readRDS('afinn.RDS')

## STEP 1: randomly select some tweets
## trump_tweets %>% sample_n(50) %>% select(text) %>% saveRDS('./data/validation.RDS')
## readRDS('./data/validation.RDS') %>% mutate(tid = seq_along(text)) %>% rio::export('./data/validation.csv')
## STEP 2: read those tweets. Assess the sentiment

## STEP 3: study the correlation between human-coding and machine judgement (e.g. the sentiment score)
validation <- readRDS('validation.RDS')

gold_standard <- rio::import()

corpus(validation$text) %>% dfm %>% dfm_lookup(dictionary = afinn) -> val_dfm

val_dfm %>% quanteda::convert(to = 'data.frame') %>%
  mutate(afinn_score = (neg5 * -5) + (neg4 * -4) + (neg3 * -3) + (neg2 * -2) +
           (neg1 * -1) + (zero * 0) + (pos1 * 1) + (pos2 * 2) + (pos3 * 3) +
           (pos4 * 4) + (pos5 * 5)) %>% select(afinn_score) -> afinn_score
plot(gold_standard, afinn_score$afinn_score)
### If the sentiment dictionary is reasonably good, use it for analysis.

trump_afinn <- dfm_lookup(trump_dfm2, dictionary = afinn)

quanteda::convert(trump_afinn, to = 'data.frame') %>%
    mutate(afinn_score = (neg5 * -5) + (neg4 * -4) + (neg3 * -3) + (neg2 * -2) +
               (neg1 * -1) + (zero * 0) + (pos1 * 1) + (pos2 * 2) + (pos3 * 3) +
               (pos4 * 4) + (pos5 * 5)) %>% select(afinn_score) %>%
        mutate(android = docvars(trump_afinn, "source")) -> afinn_score

afinn_score %>% group_by(android) %>% summarize(mean_afinn_score = mean(afinn_score), sd_afinn_score = sd(afinn_score))

cor(afinn_score$afinn_score, trump_tweets$retweet_count, method = 'spearman')


### Your turn: Apply the affin to the entire data and then compare the affinn score of tweets from iPhone and from Android.


## References:
## Rooduijn, M., & Pauwels, T. (2011). Measuring populism: Comparing two methods of content analysis. West European Politics, 34(6), 1272-1283.
## Hansen, L. K., Arvidsson, A., Nielsen, F. Å., Colleoni, E., & Etter, M. (2011). Good friends, bad news-affect and virality in twitter. In Future information technology (pp. 34-43). Springer, Berlin, Heidelberg.

