##### TWITTER DATA ANALYSIS
### Some quick analyses using tidytext package
## Written to look at data for rstudio::conf(2020)

library(tidyverse)
library(tidytext)
library(textdata)
library(scales)

### Obtain our data 
## Load Data from .csv file
tweets <- read_csv('data/rocur_tweets_thru_Dec2019.csv')

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
         impressions,
         engagements,
         engagement.rate,
         retweets,
         replies,
         likes,
         R_Experience,
         Student)

# Look at most liked tweets
rocur_tweets %>% 
  filter(likes > 800) %>%   
  arrange(desc(likes))

#### TOKENIZATION using tidytext ----
## breaking up texts into tokens
## unnest_tokens divides each tweet into smaller units

## Saving tokenization to words as its own tibble
words <- rocur_tweets %>% 
  unnest_tokens(word,Tweet.text)

words %>% 
  count(word, sort = TRUE)

## Remove stopwords
data("stop_words") # tidytext dataset of common words such as "a" and "the"
words <- words %>% 
  anti_join(stop_words) %>% # stop words leave!
  filter(!word %in% c('t.co', 'https','1','2','amp','gqv69bd9qi','It',str_detect(word,pattern = '^[0-9]'))) # words that could be considered stopwords for this dataset

## Just get a word count for all words in the dataset
words %>% 
  count(word, sort = TRUE)

## Look at frequency of words overall
freq_words <- words %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>% 
  add_row(word="package",n=328)

freq_words %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill="#88398A") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip()


#### SENTIMENT ANALYSIS ----
## Using sentimentr

clean_text <- rocur_tweets %>% # Remove unicode from text 
  mutate_at(vars(Tweet.text), function(x){gsub('[^ -~]', '', x)})

## Sentiment by word
clean_words <- clean_text %>% 
  unnest_tokens(word,Tweet.text)

words <- clean_words %>% 
  anti_join(stop_words) %>% # stop words leave!
  filter(!word %in% c('t.co', 'https','1','2','amp','gqv69bd9qi','It',str_detect(word,pattern = '^[0-9]'))) # words that could be considered stopwords for this dataset

# Sentiment first pass
words %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  filter(sentiment < -3| sentiment >8) %>% 
  mutate(word = reorder(word, sentiment)) %>% 
  ggplot(aes(word, sentiment, fill=sentiment)) +
  geom_col() +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c("none"),
        legend.background = element_rect(size=0.5, linetype="solid", 
                                         colour ="#4D4D4D")) +
  xlab("") +
  coord_flip()

## Split into negative and positive words
words %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>%
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ggplot(aes(word, n, fill=n)) +
  geom_col() +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c("none"),
        legend.background = element_rect(size=0.5, linetype="solid", 
                                         colour ="#4D4D4D")) +
  xlab(NULL) + ylab(NULL) +
  coord_flip() +
  facet_wrap(~ sentiment, scales = 'free_y') +
  scale_fill_gradient2(low = "#FFF0F5", mid = muted("#88398A"), high = "#562457")


### Look at questions vs. exclamations
questions_index <- grep("\\?",clean_text$Tweet.text)
exclaim_index <- grep("\\!",clean_text$Tweet.text)

question_tweets <- clean_text[c(questions_index),]
exclaim_tweets <- clean_text[c(exclaim_index),]

dim(clean_text)
dim(question_tweets)
dim(exclaim_tweets)

View(question_tweets)