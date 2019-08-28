
ratings<-read_tsv("c://Users/jimmy/Downloads/title.ratings.tsv.gz", na = "\\N")
titles<-read_tsv("c://Users/jimmy/Downloads/title.basics.tsv.gz", na = "\\N")
principals<-read_tsv("c://Users/jimmy/Downloads/title.principals.tsv.gz", na = "\\N")
akas<-read_tsv("c://Users/jimmy/Downloads/title.akas.tsv.gz", na = "\\N")
episodes<-read_tsv("c://Users/jimmy/Downloads/title.episode.tsv.gz", na = "\\N")
names<-read_tsv("c://Users/jimmy/Downloads/name.basics.tsv.gz", na = "\\N")


titles_ratings<-left_join(titles, ratings)

top_5000<-titles_ratings%>%
  filter(titleType=="movie"|titleType =="tvSeries")%>%
  arrange(desc(numVotes), desc(averageRating))%>%
  .[1:5000,]


top_5000<-top_5000%>%
  left_join(episodes)%>%
  left_join(principals)%>%
  left_join(akas, by = c("tconst" = "titleId"))%>%
  left_join(names)

ratings_5000 <- top_5000[colnames(ratings)]%>%
  distinct()

titles_5000 <- top_5000[colnames(titles)]%>%
  distinct()

principals_5000<- top_5000[colnames(principals)[-2]]%>%
  distinct()

akas_5000<- top_5000[c(colnames(akas)[-(1:2)], "tconst")]%>%
  distinct()%>%
  rename()

episodes_5000<- top_5000[colnames(episodes)]%>%
  distinct()

names_5000 <- top_5000[colnames(names)]%>%
  distinct()%>%
  bind_rows(names[1:100,])

rm(list = c("ratings", "titles", "principals", "akas", "episodes", "names"))

# Merge the names & principles data sets retaining all observations from principals;

actors<-left_join(principals_5000, names_5000)

# Retain observations for which the primary name is considered an actor;
actors<-actors%>%
  filter(category == "actor")

# using the appropriate function, create a separate data frame for actors whom we don't have princial information
missing<-anti_join(names_5000, principals_5000)

## How many do we have?
nrow(missing)

# Merge the titles and ratings data frames retaining all observations title movies_tv
movies_tv<-full_join(titles_5000, ratings_5000)

# Create a separate object by merging the actors and movies_tv
full_data<- full_join(movies_tv, actors)

# Calculate summary statistics using the actors unique id 

### counts of the number of movies they were in
### the average rating of all their movies
### the cumulative density function for their rating

summary<-full_data%>%
  group_by(primaryName)%>%
  summarise(count = n(), 
            rating_m = mean(averageRating))%>%
  mutate(cdf = cume_dist(rating_m))

# Plot the rating CDF
ggplot(summary, aes(x = rating_m))+
  geom_density(stat = "ecdf", fill = "blue")


### Create a data set for the actors rated to be the top 10%

top_10 <- summary%>%
 filter(cdf>=90)

### Use an appropriate join to rejoin the summary table to the full data, only retaining movies with the top 10% actors

final<-anti_join(full_data, top_10)




write_csv(ratings_5000, "suppl/ratings.csv")
write_csv(titles_5000, "suppl/titles.csv")
write_csv(principals_5000, "suppl/principal_actors.csv")
write_csv(names_5000, "suppl/names.csv")


