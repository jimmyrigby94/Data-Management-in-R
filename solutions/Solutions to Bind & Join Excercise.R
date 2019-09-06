# Loading tidyverse into memory
library(tidyverse)

# 1. Reading Data ---------------------------------------------------------
# Reading csvs directly from github!
ratings<-read_csv("https://raw.githubusercontent.com/jimmyrigby94/Data-Management-in-R/master/suppl/ratings.csv")

#### This was the read_csv() that had issues ### Now corrected
title<-read_csv("https://raw.githubusercontent.com/jimmyrigby94/Data-Management-in-R/master/suppl/titles.csv")

principal_actors<-read_csv("https://raw.githubusercontent.com/jimmyrigby94/Data-Management-in-R/master/suppl/principal_actors.csv")

names<-read_csv("https://raw.githubusercontent.com/jimmyrigby94/Data-Management-in-R/master/suppl/names.csv")


# 2. Merge Data Retaining All Obserations ------------------------------------
# Joining principal_actors and names to create cinematic_pros
### This file contains the actors, directors and writers for each movie
cinematic_pros<-full_join(principal_actors, names)


# 3. Filtering observations so that only actors are included --------------

# Use filter to identify the cinematic_pros that are actors
actors<-cinematic_pros%>%
  filter(category == "actor")



# 4 and 5. Identifying Missing Observations ----------------------------------------
# Names stores actors names while principal_actors contains movie history
# Anti-joining the data identifies the observations in names that don't have 
# matching information in the movie history

missing_movie<- anti_join(names, principal_actors)

nrow(missing_movie)

# Alternatively, we could filter based on NA columns in principal_actors on the 
# movie history
cinematic_pros%>%
  filter(is.na(tconst))%>% 
  nrow(.)


# 6. Merge the title and ratings data -------------------------------------
# merges title and ratings data frame using left_join() retaining all observations 
# in title
# right_join(ratings, title) is also appropriate
movies<-left_join(title, ratings)


# 7. Merge all the data together! -----------------------------------------
# full_join() is a mutating join and my personal favorite (Any join would do)!
# All observations are retained
full_data<-full_join(movies, actors)


# 8. Summarising by Actor ----------------------------------------------------
# We can use a dplyr pipe to summarise the data frame
actor_summary<-full_data%>% # Using the information stored in full_data
  group_by(nconst,primaryName)%>% # group the data based on these categories (just using nconst is appropriate)
  summarise(num_movies = n(), # How many movies did each actor star in?
            mean_rate = mean(averageRating))%>% # What is the average rating of all an actors movies?
  mutate(cum_dist = cume_dist(mean_rate)) # What proprotion of actor's ratings is less than or equal to a given actor?


# 9. Plotting Data --------------------------------------------------------
# What does the distribution of actors look like?
ggplot(actor_summary, aes(x = mean_rate))+
  stat_ecdf(color = "black", fill = "blue", geom = "density")+
  labs(x = "Actor's Mean Rating", y = "P(X<=x)")


# 10. Creating dataframe that stores the names and ids for top actors --------

# Which actors were in the top 10%?
top_10_pct<-actor_summary%>%
  filter(cum_dist >=.90)

# 11. Using semi_join() to identify rows of movie-actor staring top actors --------

hit_actors_movies<-semi_join(full_data, top_10_pct)
