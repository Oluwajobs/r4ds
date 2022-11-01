# Name: 'Joba Adisa
# Date: October 24, 2022


## Setup

library(tidyverse)
library(lubridate)
library(janitor)
library(tidytext)
library(aRxiv)
library(wordcloud)
library(tm)
library(rvest)
library(skimr)

## Data Scraping

# 1. API Example using aRxiv

DataSciencePapers <- arxiv_search( 
  query = '"Data Science"', 
  limit = 20000, 
  batchsize = 100 )

# view the data
glimpse(DataSciencePapers)

# To make sure R understands these variables as dates, 
# we will once again use the lubridate package. 
# After this conversion, R can deal with these two columns as measurements of time

DataSciencePapers <- DataSciencePapers %>% 
  mutate(submitted = lubridate::ymd_hms(submitted),
         updated = lubridate::ymd_hms(updated))

# viewing the data again
glimpse(DataSciencePapers)


# We begin by examining the distribution of submission years. 
# How has interest grown in data science?

DataSciencePapers %>% 
  group_by(year(submitted)) %>% 
  summarise(n = n())

## Analysis

# 1. Counting Words
DataSciencePapers %>% 
  # `unnest_tokens` function helps prepare data for text analysis.
  # It uses a tokenizer to split the text.
  unnest_tokens(word, abstract) %>%
  # Count word frequency of the data
  count(id, word, sort = TRUE) %>% 
  head()

# the word “the” is the most common word in many abstracts.
# a common practice to exclude stop words such as “a,” “the,” and “you”.
  

arxiv_words <- DataSciencePapers %>% 
  unnest_tokens(word, abstract) %>%
  # Using the `get_stopwords()` function to get a column of the stop words
  # and excluding it from the dataset using `anti_join()`
  anti_join(get_stopwords(), by = "word")

# Counting the occurrences of words again
arxiv_words %>% 
  count(id, word, sort = TRUE) %>% 
  head()

# We now see that the word “data” is, not surprisingly, 
# the most common non-stop word in many of the abstracts.
arxiv_abstracts <- arxiv_words %>% 
  group_by(id) %>% 
  summarize(abstract_clean = 
              # concatenate all the rows of each group together separated by a space
              paste(word, collapse = " "))


# Joining with the DataSciencePaper with a column for the abstract that is without stop words
arxiv_papers <- DataSciencePapers %>% 
  left_join(arxiv_abstracts, by = "id")


# Word Clouds
#' A wrod cloud is a kind of multivariate histogram for words. 
#' The wordcloud package generates a graphical depictions of word frequencies.

set.seed(10242022)

arxiv_papers %>% 
  pull(abstract_clean) %>% 
  wordcloud( max.words = 40, 
             scale = c(5, 1), 
             colors = topo.colors(n = 30), 
             random.color = TRUE )


# Sentiment Analysis

# A lexicon is a word list with 
# associated sentiments (e.g., positivity, negativity) that have been labeled.


# using `afinn()` for our sentiment analysis
# it associates each word with an integer value, ranging from -5 to 5.
afinn <- get_sentiments("afinn") 

afinn %>% 
  slice_sample(n = 15) %>% 
  arrange(desc(value))


arxiv_sentiments <- arxiv_words %>% 
  left_join(afinn, by = "word") %>% 
  group_by(id) %>% 
  summarize( num_words = n(), 
             sentiment = sum(value, na.rm = TRUE), 
             .groups = "drop" ) %>% 
  mutate(sentiment_per_word = sentiment / num_words) %>% 
  arrange(desc(sentiment))

# We can now add this new variable to our dataset of papers.
arxiv_papers <- arxiv_papers %>% 
  left_join(arxiv_sentiments, by = "id") 

# 
arxiv_papers %>% 
  skim(sentiment, sentiment_per_word)

#' The average sentiment score of these papers is 4, but they range from -26 to 39. 
#' The paper with the highest sentiment score per word had a score of 0.333. 


# Let’s take a closer look at the most positive abstract.

most_positive <- arxiv_papers %>% 
  filter(sentiment_per_word == max(sentiment_per_word)) %>% 
  pull(abstract)

# wraps it into a nicely formatted paragraph
str_wrap(most_positive)


# We can explore if there are time trends or differences between different 
# disciplines.We can use a regular expression to extract only the primary field 
# (the part before the period in the primary_category variable), 
# which may contain a dash (-), but otherwise is all lowercase characters.

arxiv_papers <- arxiv_papers %>% 
  mutate(field = str_extract(primary_category, "^[a-z,-]+")) 

ggplot( arxiv_papers, 
        aes(x = submitted, y = sentiment_per_word, color = field == "cs")) + 
  geom_smooth(method = stats::loess, se = TRUE) + 
  scale_color_brewer("Computer Science?", palette = "Set2") + 
  labs(x = "Date submitted", y = "Sentiment score per word")

# Interpretation
#' There’s mild evidence for a downward trend over time. 
#' Computer science papers have slightly higher sentiment, but the difference is modest.



# Exercise 1
# Use the group_by and summarize functions to determine the mean sentiment score per word 
# by field. Arrange the output in descending order. 
# Which fields have the highest and lowest scores? Use the arxiv_cats function of the aRxiv package to see the description of each subject classification abbreviation.

arxiv_papers %>% 
  group_by(field) %>% 
  summarise(
    mean_sentiment = mean(sentiment_per_word, na.rm = TRUE)
  ) %>% 
  arrange(desc(mean_sentiment))

# for this run:

# High Energy Physics-Theory had the highest sentiment score in this data: 0.0547
# Nuclear Theory (nucl-th) had the lowest sentiment score in this dataset: 0.00763 

arxiv_cats  # outputs the description of each subject classification abbrevaition


# HTML Table Example

# url of a wiki page we intend to scrap
url <- "http://en.wikipedia.org/wiki/List_of_songs_recorded_by_the_Beatles" 

# getting the list of tables in the site
tables <- url %>% 
  read_html() %>% 
  html_nodes("table")

# Using the `pluck()` function from the purr package to access the third table
# pluck(table_name, 3)
Beatles_songs <- tables %>% 
  purrr::pluck(3) %>% 
  html_table() %>% 
  clean_names() %>% 
  select(song, lead_vocal_s_d) 

# viewing pour data
glimpse(Beatles_songs)

# We need to clean the data a bit. Note that the song variable 
# contains quotation marks. The lead_vocal_s_d variable would 
# benefit from being renamed.

Beatles_songs <- Beatles_songs %>% 
  mutate(song = str_remove_all(song, pattern = '\\"')) %>% 
  rename(vocals = lead_vocal_s_d)


# determining how many songs each person is credited with singing.
Beatles_songs %>% 
  group_by(vocals) %>% 
  count() %>% 
  arrange(desc(n))


#vocal credits include McCartney 
Beatles_songs %>% 
  pull(vocals) %>% 
  str_subset("McCartney") %>% 
  length()  # outputs 103


#vocal credits include Lennon 
Beatles_songs %>% 
  pull(vocals) %>% 
  str_subset("Lennon") %>% 
  length()  # outputs 111

# John was credited with singing on more songs than Paul.


# investigating the titles of their songs. 
# What were they singing about?

Beatles_words <- Beatles_songs %>% 
  unnest_tokens(word, song) %>% 
  anti_join(get_stopwords(), by = "word") 

Beatles_words %>% 
  count(word, sort = TRUE) %>% 
  arrange(desc(n)) %>% 
  head()

# “love” is the most common word in the title of Beatles songs.

# Exercise 2

# word Cloud
set.seed(10242022)

Beatles_words %>% 
  pull(word) %>% 
  wordcloud( max.words = 40, 
             scale = c(5, 1), 
             colors = topo.colors(n = 30), 
             random.color = TRUE )

