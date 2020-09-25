## ----setup, include = FALSE, cache = FALSE--------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, cache = TRUE,
                      dev = "svg")

library(ggplot2)
theme_set(theme_bw())


## -------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(purrr)
library(twitteR)


## ----eval = FALSE---------------------------------------------------------------------------------------------------------------------------
## # You'd need to set global options with an authenticated app
## setup_twitter_oauth(getOption("twitter_consumer_key"),
##                     getOption("twitter_consumer_secret"),
##                     getOption("twitter_access_token"),
##                     getOption("twitter_access_token_secret"))
## 
## # We can request only 3200 tweets at a time; it will return fewer
## # depending on the API
## trump_tweets <- userTimeline("realDonaldTrump", n = 3200)
## trump_tweets_df <- tbl_df(map_df(trump_tweets, as.data.frame))


## ----trump_tweets_df------------------------------------------------------------------------------------------------------------------------
# if you want to follow along without setting up Twitter authentication,
# just use my dataset:
load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))


## ----tweets, dependson = "trump_tweets_df"--------------------------------------------------------------------------------------------------
library(tidyr)

tweets <- trump_tweets_df %>%
  select(id, statusSource, text, created) %>%
  extract(statusSource, "source", "Twitter for (.*?)<") %>% #Extract a character column  and rename as "source"
  filter(source %in% c("iPhone", "Android")) # only keep iPhone and android rather than  web client or iPad


## ----dependson = "tweets"-------------------------------------------------------------------------------------------------------------------
library(lubridate)
library(scales)

tweets %>%
  count(source, hour = hour(with_tz(created, "EST"))) %>% #Get date-time in a different time zone
  mutate(percent = n / sum(n)) %>% # as of total tweets = 1390
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")


## ----dependson = "tweets", echo = FALSE-----------------------------------------------------------------------------------------------------
library(stringr)

tweets %>%
  count(source,
        quoted = ifelse(str_detect(text, '^"'), "Quoted", "Not quoted")) %>% #extrat with " 
  ggplot(aes(source, n, fill = quoted)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "") +
  ggtitle('Whether tweets start with a quotation mark (")')


## ---- dependson = "tweets"------------------------------------------------------------------------------------------------------------------
tweet_picture_counts <- tweets %>%
  filter(!str_detect(text, '^"')) %>%
  count(source,
        picture = ifelse(str_detect(text, "t.co"),
                         "Picture/link", "No picture/link"))

ggplot(tweet_picture_counts, aes(source, n, fill = picture)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "")


## ----echo = FALSE---------------------------------------------------------------------------------------------------------------------------
spr <- tweet_picture_counts %>%
  spread(source, n) %>%
  mutate_each(funs(. / sum(.)), Android, iPhone)

rr <- spr$iPhone[2] / spr$Android[2]


## ----tweet_words, dependson = "tweets"------------------------------------------------------------------------------------------------------
library(tidytext)

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- tweets %>%
  filter(!str_detect(text, '^"')) %>% # filter our quoted tweets out (since they contain text from followers that may not be representative of Trump¡¯s own tweets
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

tweet_words


## ----tweet_words_plot, dependson = "tweet_words", fig.height = 6, fig.width = 8, echo = FALSE-----------------------------------------------
tweet_words %>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  ylab("Occurrences") +
  coord_flip()

#Which are the words most likely to be from Android and most likely from iPhone?
## ----android_iphone_ratios, dependson = "tweet_words"---------------------------------------------------------------------------------------
android_iphone_ratios <- tweet_words %>%
  count(word, source) %>%
  filter(sum(n) >= 5) %>%
  spread(source, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -word) %>% #log odds ratio
  mutate(logratio = log2(Android / iPhone)) %>%
  arrange(desc(logratio))


## ----android_iphone_ratios_plot, dependson = "android_iphone_ratios", fig.height = 6, fig.width = 8, echo = FALSE---------------------------
android_iphone_ratios %>%
  group_by(logratio > 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Android / iPhone log ratio") +
  scale_fill_manual(name = "", labels = c("Android", "iPhone"),
                    values = c("red", "lightblue"))


## ----nrc, message=FALSE---------------------------------------------------------------------------------------------------------------------
library(tidytext)
library(textdata)
nrc <- get_sentiments("nrc")



## ----by_source_sentiment, dependson = c("nrc", "tweet_words")-------------------------------------------------------------------------------
sources <- tweet_words %>%
  group_by(source) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(id, source, total_words)

by_source_sentiment <- tweet_words %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment, id) %>%
  ungroup() %>%
  complete(sentiment, id, fill = list(n = 0)) %>%
  inner_join(sources) %>%
  group_by(source, sentiment, total_words) %>%
  summarize(words = sum(n)) %>%
  ungroup()

head(by_source_sentiment)


## -------------------------------------------------------------------------------------------------------------------------------------------
library(broom)

sentiment_differences <- by_source_sentiment %>%
  group_by(sentiment) %>%
  do(tidy(poisson.test(.$words, .$total_words)))

sentiment_differences


## ----dependson = "sentiment_differences", echo = FALSE, fig.height = 8, fig.width = 8-------------------------------------------------------
library(scales)

sentiment_differences %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, estimate)) %>%
  mutate_each(funs(. - 1), estimate, conf.low, conf.high) %>%
  ggplot(aes(estimate, sentiment)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  scale_x_continuous(labels = percent_format()) +
  labs(x = "% increase in Android relative to iPhone",
       y = "Sentiment")


## ----dependson = "android_iphone_ratios", echo = FALSE, fig.height = 6, fig.width = 8-------------------------------------------------------
android_iphone_ratios %>%
  inner_join(nrc, by = "word") %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  mutate(sentiment = reorder(sentiment, -logratio),
         word = reorder(word, -logratio)) %>%
  group_by(sentiment) %>%
  top_n(10, abs(logratio)) %>%
  ungroup() %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  facet_wrap(~ sentiment, scales = "free", nrow = 2) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "Android / iPhone log ratio") +
  scale_fill_manual(name = "", labels = c("Android", "iPhone"),
                    values = c("red", "lightblue"))

