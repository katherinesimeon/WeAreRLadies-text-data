##### @WeAreRLadies Twitter Text Analysis
### Code looking at @WeAreRLadies tweets using tidytext package
### Written for R-Ladies Chicago August Meetup

# Load packages
library(tidyverse)
library(tidytext)
library(scales)
library(textdata)
library(wordcloud)
library(reshape2)

### Obtain our data 
## Load Data from .csv file
tweets <- read_csv('rocur_tweets_thru_June2019.csv')

## Remove any tweets where there wasn't a curator
tweets <- tweets %>% 
  filter(Curator!="BREAK") %>% 
  filter(!is.na(Curator))

## Make a tibble that only has what we want
rocur_tweets <- tweets %>% 
  select(Twitter,
         Tweet.text,
         date,
         time_only,
         engagement.rate,
         Student)

View(rocur_tweets) # Look at data

#### TOKENIZATION using tidytext ----
## breaking up texts into tokens
## unnest_tokens divides each tweet into smaller units

## Saving tokenization to words as its own tibble
words <- rocur_tweets %>% 
  unnest_tokens(word,Tweet.text)

## Sort words by frequency
words %>% 
  count(word, sort = TRUE)

## Remove stopwords
data("stop_words") # tidytext dataset of common words such as "a" and "the"
words <- words %>% 
  anti_join(stop_words) %>% # stop words leave!
  filter(!word %in% c('t.co', 'https',str_detect(word,pattern = '^[0-9]'))) # words that could be considered stopwords for this dataset

## Sort words by frequency again
words %>% 
  count(word, sort = TRUE)

## Look at frequency of words overall
words %>%
  count(word, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "#88398A") +
  xlab(NULL) +
  ylab('n') +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_flip()



#### SENTIMENT ANALYSIS ----
## Looking at whether text is positive, negative, or neutral with the Bing Sentiment lexicon

## Sentiment graphed the way the tidytext book does it
words %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(word, sentiment, colour=sentiment)) +
  geom_col() +
  xlab("") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c("none"),
        legend.background = element_rect(size=0.5, linetype="solid", 
                                         colour ="#4D4D4D")) +
  scale_colour_gradient2(low = "#FFF0F5", mid = muted("#88398A"), high = "#562457") 


## Graph sentiment for only the very positive and very negative tokens
words %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  filter(sentiment < -3 | sentiment > 8) %>% # Filter to remove middle sentiment scores 
  mutate(word = reorder(word, sentiment)) %>% 
  ggplot(aes(word, sentiment, fill=sentiment)) +
  geom_col() +
  xlab("") +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c("none"),
        legend.background = element_rect(size=0.5, linetype="solid", 
                                         colour ="#4D4D4D")) +
  coord_flip() +
  scale_fill_gradient2(low = "#FFF0F5", mid = muted("#88398A"), high = "#562457")



#### BIGRAMS ---- 
bigrams <- rocur_tweets %>% 
  unnest_tokens(bigram, Tweet.text, token = "ngrams", n = 2)


## Look at most common bigrams
bigrams %>%  
  count(bigram, sort = TRUE)

## Separate bigrams so that each word is in its own column
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

## Filter out stop words
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% c(stop_words$word,'t.co', 'https')) %>%
  filter(!word2 %in% c(stop_words$word,'t.co', 'https')) 

## Look at most common bigrams after filtering out stop words
bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

## Put the bigrams back together in a single column
bigrams_together <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

## Look at the words before and after 'plot'
bigrams_separated %>%
  filter(word1 == "plot") %>%
  count(word1, word2, sort = TRUE)

bigrams_separated %>%
  filter(word2 == "plot") %>%
  count(word1, word2, sort = TRUE)


#### Calculating tf-idf ----
## Calculate tf-idf info for each curator
## First, we need to obtain term frequency

# Word Count by Curator
word_count_curator <- words %>%
  count(Twitter, word, sort = TRUE) 

word_count_curator

# Term frequency of words by curator
total_words_curator <- word_count_curator %>% group_by(Twitter) %>% summarize(total = sum(n))
curator_tf <- left_join(word_count_curator, total_words_curator)
curator_tf

# Obtain tf_idf
curator_tf_idf <- curator_tf %>% 
  bind_tf_idf(word, Twitter, n)

# Sort by tf_idf
curator_tf_idf %>% 
  arrange(desc(tf_idf,n))

# tf_idf for each curator
curator_tf_idf %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(Twitter) %>% 
  top_n(15) %>% 
  ungroup %>% 
  ggplot(aes(word, tf_idf, fill= Twitter)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "td-idf") +
  facet_wrap(~ Twitter, ncol = 8, scales = "free") +
  coord_flip()

# tf_idf for cbirunda (NYC R Conf live-tweets) and littlemissdata (rstudioconf live-tweets)
curator_tf_idf %>% 
  filter(Twitter %in% c("@littlemissdata","@cbirunda")) %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(Twitter) %>% 
  top_n(15) %>% 
  ungroup %>% 
  ggplot(aes(word, tf_idf, fill= Twitter)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "td-idf") +
  facet_wrap(~ Twitter, ncol = 2, scales = "free") +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_flip() +
  scale_fill_manual(values = c("#88398A","#562457"))
  
## Compare the NYC R Conf and rstudioconf term frequencies
words %>%
  filter(Twitter %in% c("@littlemissdata","@cbirunda")) %>% 
  count(word, Twitter, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n)) %>%
  group_by(Twitter) %>% 
  top_n(15) %>% 
  ggplot(aes(x=word, y=n, fill = Twitter)) +
  geom_col(show.legend = FALSE) +
  xlab(NULL) + ylab('Word Frequency') +
  facet_wrap(~ Twitter, scales = "free") +
  theme_bw(base_size = 14) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_flip() +
  scale_fill_manual(values = c("#88398A","#562457"))


#### Other stuff ----
## Word Cloud student vs. non student
# uses wordcloud package
words %>% 
  count(word, Student, sort = TRUE) %>%
  filter(n > 5) %>% 
  acast(word ~ Student, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#88398A","#562457"),
                   max.words = 200)

