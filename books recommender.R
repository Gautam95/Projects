#Packages
library(recommenderlab)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(DT)
library(knitr)
library(grid)
library(gridExtra)
library(methods)
library(Matrix)
library(corrplot)
library(ggraph)

#Loading_datasets
read.csv("C:\\Users\\91844\\Downloads\\ratings (1).csv") -> ratings
read.csv("C:\\Users\\91844\\Downloads\\tags (1).csv") -> tags
read.csv("C:\\Users\\91844\\Downloads\\book_tags.csv") -> books_tag
read.csv("C:\\Users\\91844\\Downloads\\books.csv") -> books
View(tags)
View(ratings)
View(books_tag)
View(books)

#Cleaning_data #finding duplicate ratings
ratings %>% group_by(user_id,book_id) %>% mutate(N=n()) ->ratings
table(ratings$N)
ratings %>% filter(N==1) -> ratings

#Removing users who rated fewer than 3 books
ratings %>% group_by(user_id) %>% mutate(Ratings_given = n()) -> ratings
ratings %>% filter(Ratings_given>2) -> ratings

#Selecting a sample from entire dataset
set.seed(1)
user.fraction <- 0.02
users <- unique(ratings$user_id)
sample.user <- sample(users, round(user.fraction*length(users)))
nrow(ratings)
ratings %>% filter(user_id %in% sample.user) ->ratings
nrow(ratings)

#Making a distribution of ratings
ratings %>% 
  ggplot(aes(x = rating,fill = factor(rating)))+
  geom_bar(color = "grey20")+scale_fill_brewer(palette = "YlGnBu")+guides(fill = FALSE)

#Number of ratings per books
ratings %>% 
  group_by(book_id) %>% 
  summarize(number_of_ratings_per_book = n()) %>%
  ggplot(aes(number_of_ratings_per_book)) + 
  geom_bar(fill = "orange",color = "grey20",width = 1)+ coord_cartesian(c(0,40))

#Find the count of different genres

str_to_lower(c("Art","Biography","Business","ChickList","Children","Poetry","horror","crime","history","suspense","classic","mystery","paranormal","spiritual","fantasy","romance","cookbooks","manga","sports","travel","religion")) -> genres
genres[str_to_lower(genres) %in% tags$tag_name ] -> available_genres
tags$tag_id[match(available_genres,tags$tag_name)] ->available_tags

#Plotting percentage of each genres
 tmp <- books_tag %>%
  filter(tag_id %in% available_tags) %>% 
  group_by(tag_id) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(sumN = sum(n),percentage = n/sumN) %>% 
  arrange(-percentage) %>% 
  left_join(tags,by = "tag_id") -> book_info
View(book_info)

book_info %>%
  ggplot(aes(reorder(tag_name,percentage),percentage,fill = percentage))+
  geom_bar(stat ="identity")+coord_flip()+scale_fill_distiller(palette = 'YlOrRd')+
  labs(y = 'percentage', x = 'Genres')

#Top10 Books with Highest Ratings
books %>% 
  arrange(-average_rating) %>% 
  top_n(10,wt = average_rating) %>% 
  select(title, ratings_count,average_rating)-> Top10
View(Top10)

#Top10 most popular books
books %>% 
  arrange(-ratings_count) %>% 
  top_n(10,wt = ratings_count) %>% 
  select(title,ratings_count,average_rating) ->Top_popular
View(Top_popular)

#Re-Structuring data to build collaborative filtering(Our data should be in form of matrix
# & rows should correspond to books)

dimension_names<- list(user_id = sort(unique(ratings$user_id)),book_id =sort(unique(ratings$book_id))) 
ratingmat<- spread(select(ratings,book_id,user_id,rating),book_id,rating) %>% 
select(-user_id)  
class(ratingmat)
as.matrix(ratingmat) -> ratingmat
class(ratingmat)
ratingmat[1:5,1:5]
ratingmat[,-1] ->ratingmat
ratingmat[1:5,1:5]
dimnames(ratingmat)->dimension_names
dim(ratingmat)

#Converting rating matrix into a real rating matrix
ratingmat0 <- ratingmat
dim(ratingmat0)
ratingmat0[is.na(ratingmat0)] <- 0
sparseMatrix<- as(ratingmat0,"sparseMatrix")
real_ratings <-new("realRatingMatrix",data = sparseMatrix)
real_ratings

#Splitting data into test and train

sample(x= c(T,F),size = nrow(real_ratings),replace = T,prob = c(0.8,0.2)) -> split
real_ratings[split,] -> recc_train
real_ratings[!split,] -> recc_test

#Building UBCF model
Recommender(data = recc_train,method = "UBCF") ->recc_model_UBCF
n_recc_model_UBCF <-6
predict(object = recc_model_UBCF, newdata = recc_test, n=n_recc_model_UBCF)-> recc_pred
predict(object=recc_model_UBCF,newdata=recc_test,n=n_recc_model_UBCF)->recc_pred

#Recommending books for user1

recc_pred@items[[1]] -> user1_books
recc_pred@itemlabels[user1_books]

books %>% filter(id==6343) %>% select(original_title,authors)
books %>% filter(id==7482) %>% select(original_title,authors)
books %>% filter(id==2750) %>% select(original_title,authors)

#Recommending books for user5

recc_predicted_ubcf@items[[5]]->user5_book_numbers
recc_predicted_ubcf@itemLabels[user5_book_numbers]

books %>% filter(id==4624) %>% select(original_title,authors)
books %>% filter(id==6867) %>% select(original_title,authors)
books %>% filter(id==7326) %>% select(original_title,authors)
