#########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                       #                    title = as.character(title),
                         #                  genres = as.character(genres))

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1) # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Set working directory

setwd("C:/Users/nelson/projects/Proyecto final Harvard")

#Save data
save.image("project_movielens.Rdata")

#load data
load(file = "project_movielens.Rdata")

#How many rows and columns are there in the edx dataset?

dim(edx)

#How many zeros were given as ratings in the edx dataset?
sum(edx$rating==0.0)

edx %>% filter(rating == 0) %>% tally()

#How many threes were given as ratings in the edx dataset?
sum(edx$rating==3.0)

edx %>% filter(rating == 3) %>% tally()

#How many different movies are in the edx dataset?

length(unique(edx$movieId))
n_distinct(edx$movieId)

#How many different users are in the edx dataset?

length(unique(edx$userId))


#How many movie ratings are in each of the following genres in the edx dataset?

table(edx$genres)
sum(str_detect(edx$genres,"Drama"))
sum(str_detect(edx$genres,"Comedy"))
sum(str_detect(edx$genres,"Thriller"))
sum(str_detect(edx$genres,"Romance"))

genres = c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})


numbers = c("Forrest Gump (1994)", "Jurassic Park (1993)", "Pulp Fiction (1994)", "Shawshank Redemption, The (1994)", "Speed 2: Cruise Control (1997)")
sapply(numbers, function(f) {
  sum(edx$title==f)
})

edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


edx %>% group_by(rating) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()



# Empezamos el analisis

#cargamos paquetes
library(tidyverse)

#Comenzamos con Regularization con lambda arbitrario de 3

lambda <- 3
mu <- mean(edx$rating)
movie_reg_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

#Unimos las variables regularizadas a la base original

edx_reg <- edx %>%  left_join(movie_reg_avgs, by = "movieId") 


#Vemos el top 10 de peliculas regularizadas 

regulbase <-  join(edx, movie_reg_avgs, type="full")
  
  
library(dplyr)
semi_join(movie_reg_avgs, edx, by = "movieId") 
  
 
edx %>%
  count(movieId) %>%
  left_join(movie_reg_avgs, by = "movieId") %>%
  arrange(b_i) %>%
  select(title, b_i, n) %>%
  slice(1:10) %>%
  pull(title)


m2 <-
  edx_reg %>% 
  group_by(movieId) %>% 
  filter(row_number()==1)

m2 <- as.data.frame(m2)

slice_max(m2, order_by = b_i, n=10)
slice_min(m2, order_by = b_i, n=10)


#Do we improve our results?
predicted_ratings <- validation %>%
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
RMSE(predicted_ratings, validation$rating)

#Regularization inlcuding user effect and choosing best lambda
#The test set should never be used for tuning.

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <-
    edx %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, edx$rating))
})

qplot(lambdas, rmses)

lambda <- lambdas[which.min(rmses)]
lambda

#el modelo de regularizacion con efectos de peliculas y de usuarios y con lambda optimo 5.5


#efecto peliculas optimo


lambdas <- 0.5
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <-
    validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, validation$rating))
})
