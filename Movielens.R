
##title: "Movielens Report"
##author: "Lam Vu"

###################################
# Create edx set and validation set
###################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
#used caret pkg
set.seed(1)
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


#############################################################
###Data Analysis###
#exploring edx dataset
class(edx)
head(edx)

#number of rows and columns in edx dataset
dim(edx)

#number of ratings were given as zero(0) or three(3)
edx %>% filter(rating == 0) %>% tally()

edx %>% filter(rating == 3) %>% tally()


#number of distinct userIds and movieIds
edx %>% summarize(n_users = n_distinct(userId),
                  n_movies = n_distinct(movieId))

#table of number of ratings by genres after separating combined genres
library(tidyr)
g_ratings <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n(), avg = mean(rating)) %>%
  arrange(desc(count))
g_ratings

#top 10 movie titles with the greatest number of ratings
top_titles <- edx %>% 
  group_by(movieId) %>%
  summarize(n = n(),
            title = title[1],
            avg = mean(rating)) %>%
  top_n(10, n) %>%
  arrange(desc(n)) 
top_titles

#Distribution of Movies
edx %>% group_by(movieId) %>% 
  summarize(n = n())%>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of movies") +
  ggtitle("Movies")

#Distribution of Users
edx %>% group_by(userId) %>% summarize(n = n())%>%top_n(10, n) %>%
  arrange(desc(n)) 

edx %>% group_by(userId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(fill = "blue", color = "black", bins = 30) +
  scale_x_log10() + 
  xlab("Number of ratings") +
  ylab("Number of users") +
  ggtitle("Users")

#top 10 movie titles with the greatest average rating
top_avg <- edx %>% 
  group_by(movieId) %>%
  summarize(n = n(),
            title = title[1],
            avg = mean(rating)) %>%
  top_n(10, n) %>%
  arrange(desc(avg)) 
top_avg

#table of frequency of star ratings from most to least
rating_frequency <- edx %>% group_by(rating) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))

rating_frequency  
#In general, half star ratings are less common than whole star ratings

# Rating distribution
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, color = "black") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  ggtitle("Rating distribution")

#RMSE formula
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

####Models####
#build the simplest model: predict same rating for all movies across all users
#Yu,i = mu + Eu,i
mu <- mean(edx$rating)
mu

#Test results based on simple prediction
naive_rmse <- RMSE(validation$rating, mu)
naive_rmse

#create a table that's going to store the results 
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
rmse_results %>% knitr::kable()

#Yu,i = mu + b_i + Eu,i where b_i = the average rating for movie i or as "bias
movie_avgs <- edx %>%
  group_by(movieId)%>%
  summarize(b_i = mean(rating - mu))

movie_avgs %>%
  ggplot(aes(b_i )) +
  geom_histogram(binwidth = .25, color = "black") +
  ggtitle("Estimates for b_i")

#Movie Effect Model
predicted_ratings <- mu +  validation %>%
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()

#User Effect Model
user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

user_avgs %>%
  ggplot(aes(b_u )) +
  geom_histogram(binwidth = .25, color = "black") +
  ggtitle("Estimates for b_u")

predicted_ratings <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effect Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

#Regularization permits us to penalize large estimates that come from small sample sizes
# lambda is a tuning parameter
# Use cross-validation to find the lambda with lowest rmse 
lambdas <- seq(0, 10, 0.25)

# For each lambda,find b_i & b_u, followed by rating prediction & testing
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(predicted_ratings, validation$rating))
})

# Plot rmses vs lambdas to select the optimal lambda                                                             
qplot(lambdas, rmses)  

# The optimal lambda                                                             
lambda <- lambdas[which.min(rmses)]
lambda

# Test and save results                                                             
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized movie and user effect model",  
                                     RMSE = min(rmses)))

#table of results from different models
rmse_results %>% knitr::kable()

