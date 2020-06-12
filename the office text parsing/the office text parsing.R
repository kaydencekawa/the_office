#Install Packages
library(dplyr)
install.packages("schrute")
library(schrute)
library(tidyr)
library(ggplot2)
library(tidytext)

# Get the Data
office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')
episode_text <- schrute::theoffice

episode_text_parse <-
episode_text %>%
  unnest_tokens(word, text)

episode_text_parse %>%
  count(word, sort = TRUE) %>%
  filter(n < 15000) %>%
  filter(n > 5000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

get_sentiments("nrc")
