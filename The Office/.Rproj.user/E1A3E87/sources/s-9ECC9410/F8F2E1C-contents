#Install Packages
library(dplyr)
install.packages("schrute")
library(schrute)
library(tidyr)
library(ggplot2)

# Get the Data
office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')
episode_text <- schrute::theoffice
episode_dir_writer <- episode_text[,2:6]

##Clean episode_dir_writer to get one record per episode
episode_dir_writer <-
  distinct(episode_dir_writer)

##What is in the office_ratings df but not the episode_dir_writer df?
episode_dir_writer$season_episode <- paste(episode_dir_writer$season, episode_dir_writer$episode)
office_ratings$season_episode <- paste(office_ratings$season, office_ratings$episode)
results1 = setdiff(office_ratings$season_episode, episode_dir_writer$season_episode)
results1
##For two part episodes, episode_dir_writer counts them as two episodes, office_ratings counts them as 1

##create a new episode count for episode_dir_writer where 2 part episodes are counted as one episode
episode_dir_writer$counter <- 1

episode_dir_writer <- 
  episode_dir_writer %>%
  group_by(season) %>%
  mutate(episode_reorder = cumsum(counter))

episode_dir_writer$season_episode <- paste(episode_dir_writer$season, episode_dir_writer$episode_reorder)

results2 = setdiff(office_ratings$season_episode, episode_dir_writer$season_episode)
results2
##Office ratings doesn't always treat two part episodes the same way (some are 1 record, some are 2)

##Create two rows for parts one and two episodes in season 6 of the episode_dir_writer df (rows 95 & 96, and 108 & 109)
##Which rows need to be duplicated?
episode_dir_writer %>%
  filter(grepl("Parts 1&2", episode_name)) %>%
  filter(season == 6)

##Duplicate rows
episode_dir_writer <- rbind(episode_dir_writer, episode_dir_writer[rep(95, 1),])
episode_dir_writer <- rbind(episode_dir_writer, episode_dir_writer[rep(107, 1),])

##Resort and rerun episode reorder
episode_dir_writer <-
  episode_dir_writer %>%
  arrange(season, episode) %>%
  group_by(season) %>%
  mutate(episode_reorder = cumsum(counter))

episode_dir_writer$season_episode <- paste(episode_dir_writer$season, episode_dir_writer$episode_reorder)

##Merge the tables
office_data <- merge(episode_dir_writer, office_ratings, by.x = 'season_episode', by.y = 'season_episode')

##Check which rows don't have the same title
different_titles <-
  office_data[office_data$episode_name != office_data$title,]
##The different names are just semantic 

##Look at frequency of writers
episodes_per_writer <- 
  office_data %>%
  select(writer) %>%
  group_by(writer) %>%
  mutate(count_writer = n()) %>%
  distinct()

ggplot(episodes_per_writer %>% filter(count_writer > 5)) +
  geom_bar(aes(reorder(writer, -count_writer), count_writer),
           alpha = 1, stat="identity") +
  labs(title="Most Frequent Writers") +
  labs(x="Writer", y="Number of Episodes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
##Mindy Kaling wrote the most episodes, followed by BJ Novak


##Writers with the highest rating
rating_per_writer <- 
  office_data %>%
  group_by(writer) %>%
  summarise_at(vars(imdb_rating), mean) 

ggplot(rating_per_writer %>% filter(imdb_rating > 8.5)) +
  geom_bar(aes(reorder(writer, -imdb_rating), imdb_rating),
           alpha = 1, stat="identity") +
  labs(title="Highest Rated Writers") +
  labs(x="Writer", y="IMDB Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
##Greg Daniels & Mindy Kaling have the highest average rating


##Directors with the highest rating
rating_per_director <- 
  office_data %>%
  group_by(director) %>%
  summarise_at(vars(imdb_rating), mean) 

ggplot(rating_per_director %>% filter(imdb_rating > 8.5)) +
  geom_bar(aes(reorder(director, -imdb_rating), imdb_rating),
           alpha = 1, stat="identity") +
  labs(title="Highest Rated Directors") +
  labs(x="Writer", y="IMDB Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
##Harold Rarris and Steve Carell have the highest average rating per episode

##Writer and director combos with the highest rating
office_data$director_and_writer <- paste(office_data$director, office_data$writer, sep = "&")

rating_per_dir_and_writer <- 
  office_data %>%
  group_by(director_and_writer) %>%
  summarise_at(vars(imdb_rating), mean) 

ggplot(rating_per_dir_and_writer %>% filter(imdb_rating > 9)) +
  geom_bar(aes(reorder(director_and_writer, -imdb_rating), imdb_rating),
           alpha = 1, stat="identity") +
  labs(title="Writer & Director Combo") +
  labs(x="Writer", y="IMDB Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
##Greg Daniels and Steve Carell are the dream team

##How many episodes did they write and direct together?
office_data %>%
  filter(director == 'Greg Daniels') %>%
  filter(writer == 'Steve Carell') %>%
  nrow()
##Only one, probably the finale

##Which episode did they write together?
office_data %>%
  filter(director == 'Greg Daniels') %>%
  filter(writer == 'Steve Carell')
##Not the finale! Casino night, not surprising in hindsight 

##Who were the best writer director pairs over time? For more than one episode?
episodes_per_writer_director <- 
  office_data %>%
  group_by(director_and_writer) %>%
  mutate(count_director_and_writer = n()) %>%
  distinct()

##The same director and group of writers don't work together that often
episodes_per_writer_director_filter <-
  episodes_per_writer_director %>%
  filter(count_director_and_writer > 2) %>%
  group_by(director_and_writer) %>%
  summarise_at(vars(imdb_rating), mean)

ggplot(episodes_per_writer_director_filter) +
  geom_bar(aes(reorder(director_and_writer, -imdb_rating), imdb_rating),
           alpha = 1, stat="identity") +
  labs(title="Highest Rated Writer & Director Combo") +
  labs(x="Writer & Director Combo", y="IMDB Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  y_lim(5, 10)

##Who has the most lines?
number_of_lines <-
  episode_text %>%
  select(character) %>%
  group_by(character) %>%
  mutate(num_lines = n()) %>%
  distinct()

ggplot(number_of_lines %>% filter(num_lines > 500)) +
  geom_bar(aes(reorder(character, -num_lines), num_lines),
           alpha=1, stat = 'identity') +
  labs(title="Characters with the Most Lines") +
  labs(x="Character", y="Number of Lines")  +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

##Which seasons were each character in?
seasons_per_character <- 
  episode_text %>%
  group_by(character) %>%
  mutate(first_season = min(season)) %>%
  mutate(last_season = max(season)) %>%
  select(character, first_season, last_season) %>%
  distinct()
##This isn't interesting because most everyone was in the pilot and finale

##Who the heck is Michel?
episode_text %>%
  filter(character == "Michel")
##This is suppsoed to be Michael

##Replace "Michel" with "Michael"



