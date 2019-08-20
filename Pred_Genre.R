library('tidyverse')
install.packages('boot')
library('boot')

getwd()
setwd("c:/users/sislam/documents/PredictingMovieGenre-Master")
load('acting.Rdata')
load('genres.Rdata')
#clustering actors
data_long <- inner_join(acting, genres_readable, by = c('movie_id' = 'id','title'))

colnames(data_long) <- c('movie_id','title','actor_id','actor','genre_id','genre')
data_long %>% group_by(actor_id) %>% count() %>% filter(n>25)
data_long %>% group_by(actor_id) %>% count() %>% ggplot(.,aes(n))+geom_histogram(binwidth=1)+xlim(0,20)
data_long %>% group_by(actor_id) %>% count() %>% filter(n>25)%>% ggplot(.,aes(n))+geom_histogram(binwidth = 2)

data1 <- data_long %>% group_by(actor_id) %>% count() %>% filter(n>25) #actors that have been in 25 or more movies
data2 <- data_long[data_long$actor_id %in% data1$actor_id,] #mask by actors that that have been in 25 or more movies
nrow(data1)
#set by genre prop
data_cgenre <- data2 %>% group_by(actor_id,genre) %>% count() 
data4_cactor <- data2 %>% group_by(actor_id) %>% count()

data5 <- inner_join(data_cgenre,data4_cactor,by='actor_id')
colnames(data5)<-c('actor_id','genre','g_count','a_count')
data5$genre_prop<-data5$g_count/data5$a_count
data6 <- data5[,c('actor_id','genre','genre_prop')]
data_wide<- spread(data6,genre,genre_prop)
data_wide[is.na(data_wide)]<-0
nrow(data_wide)
set.seed(123)

# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- data_wide[,-1]
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=20 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

k.out1<-kmeans(data,4,nstart=20)
table(data_wide$actor_id,k.out1$cluster)
k.out2<-kmeans(data,5,nstart=20)
actor_k<-table(data_wide$actor_id,k.out2$cluster)
actor_k

#clustering directors
load('directors.Rdata')
data_long2 <- inner_join(data_long,directors, by =c("movie_id","title"))
data_long2 <- data_long2[,-8]
colnames(data_long2)[colnames(data_long2)=='id'|colnames(data_long2)=='name']<- c('director_id','director')
data_long2

# find genre prop by director
d_cgenre <- data_long2 %>% group_by(director_id,genre) %>% count() 
d_cdir <- data_long2 %>% group_by(director_id) %>% count()
d_cgenre
d_cdir
d_5 <- inner_join(d_cgenre,d_cdir,by='director_id')
colnames(d_5)<-c('director_id','genre','g_count','d_count')
d_5$genre_prop<-d_5$g_count/d_5$d_count
d_6 <- d_5[,c('director_id','genre','genre_prop')]
d_w<- spread(d_6,genre,genre_prop)
d_w[is.na(d_w)]<-0

set.seed(123)

#group directors by k-means
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data_d <- d_w[,-1]
wss <- sapply(1:k.max, 
              function(k){kmeans(data_d, k, nstart=20 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

k_dout1<-kmeans(data_d,5,nstart=20)
table(d_w$director_id,k_dout1$cluster)
k_dout2<-kmeans(data_d,6,nstart=20)
director_k<-table(d_w$director_id,k_dout2$cluster)
director_k <- as.data.frame.matrix(director_k)
director_k

#setting director group number
for(c in 1:6){
  for(r in 1:length(director_k$`1`)){
    director_k[r,c]<-ifelse(director_k[r,c]==1,c,0)
  }
}
as.matrix(director_k)
director_group <- matrix(t(director_k)[t(director_k)!=0], nrow=nrow(director_k), byrow=TRUE) 
director_id<-d_w$director_id
director_group <-data.frame(director_id,director_group)
director_group


#settting actor group number
actor_k <-table(data_wide$actor_id,k.out2$cluster)
actor_k <- as.data.frame.matrix(actor_k)
for(c in 1:5){
  for(r in 1:length(actor_k$`1`)){
    actor_k[r,c]<-ifelse(actor_k[r,c]==1,c,0)
  }
}
as.matrix(actor_k)
actor_group <- matrix(t(actor_k)[t(actor_k)!=0], nrow=nrow(actor_k), byrow=TRUE) 
actor_id<-data_wide$actor_id
actor_group <-data.frame(actor_id,actor_group)

#adding actor group and director group in movie
test_long<-inner_join(data_long2,actor_group,'actor_id')
test_long %>% group_by(movie_id,actor_group) %>% summarize()
test_2 <- test_long %>% group_by(movie_id,actor_group) %>% summarize(n()) #number of actors in each group by movie
colnames(test_2)<- c('movie_id','actor_group','count')
test_3 <- inner_join(test_long,test_2)
test_4 <- test_3[,c(1,6,7,9,10)]
test_5 <- unique(test_4)
test_6 <- test_5[,-2]
test_7 <- unique(test_6)
colnames(test_7) <- c('movie_id','director_id','actor_group','count')
test_7
test_8 <- spread(test_7,actor_group,count)
test_9 <- unique(test_5[,c(-4,-5)])

test_10 <- inner_join(test_9,test_8)
test_10[is.na(test_10)]<-0
colnames(test_10)<-c('movie_id','genre','director_id','g_1','g_2','g_3','g_4','g_5')

test_11 <- inner_join(test_10,director_group)
test_11
test_11 %>% group_by(genre) %>% summarise(n())
e_genre <- test_11 %>% group_by(genre) %>% summarise(n()) %>% filter(`n()`>100) #taking out genres w/ less than 100 data points
t11 <- test_11[test_11$genre %in% e_genre$genre,]
t11 %>% group_by(genre) %>% summarise(n())
#creating test and training set
test.sample <- sample(nrow(t11), floor(nrow(t11)*.2))
test <- t11[test.sample,]
train <- t11[-test.sample,]
test %>% group_by(genre) %>% summarise(n())
train %>% group_by(genre) %>% summarise(n())
correct <- c()
false_positives <- c()
random_correct <- c()
random_false_pos <- c()
c_correct <- c()
cfp <- c()
crc <- c()
crfp <- c()
c_row <- c()


#running logit regression to predict each genre of movies using #of actors in each group and director group and finding false positives and accurate positives
for(i in 1:length(unique(train$genre))){
  gen<-unique(train$genre)[i]
  tr2 <- train
  tr2$genre <- if_else(tr2$genre==gen,1,0)
  yes <- tr2[tr2$genre==1,]
  dedup <- duplicated(yes$movie_id)
  yes <- yes[!dedup,]
  
  no <- tr2[!tr2$movie_id %in% yes$movie_id,]
  dedup2 <- duplicated(no$movie_id)
  no <- no[!dedup2,]
  tr3 <- rbind(yes,no)
  logit.fit <- glm(as.factor(genre) ~ as.factor(director_group)+g_1+g_2+g_3+g_4+g_5, data =tr3,family = "binomial")
  
  t2 <- test
  t2$genre <- if_else(t2$genre==gen,1,0)
  yes <- t2[t2$genre==1,]
  dedup <- duplicated(yes$movie_id)
  yes <- yes[!dedup,]
  no <- t2[!t2$movie_id %in% yes$movie_id,]
  dedup2 <- duplicated(no$movie_id)
  no <- no[!dedup2,]
  t3 <- rbind(yes,no)

  logit.predict <- predict(logit.fit,t3,type='response')
  pred <- ifelse(logit.predict>=.5,1,0)
  t3$predict <- pred
  
  random <- sample(c(0,1),replace = TRUE,nrow(t3))
  t3$Random <- random
  
  c <- nrow(t3[t3$genre==t3$predict,])/nrow(t3)
  cc <- nrow(t3[t3$genre==t3$predict,])
  c_row <- c(c_row,nrow(t3))

  true_neg <- nrow(t3[t3$genre == 0,])
  pred_fp <- nrow(t3[t3$genre == 0 & t3$predict==1,])
  fp <- pred_fp/(true_neg+pred_fp)
 
  rc<- nrow(t3[t3$genre==t3$Random,])/nrow(t3)
  rand_fp <- nrow(t3[t3$genre == 0 & t3$Random==1,])
  rfp <- rand_fp/(true_neg+rand_fp)

  correct <- c(correct,c)
  false_positives <- c(false_positives,fp)
  random_correct <- c(random_correct,rc)
  random_false_pos <- c(random_false_pos,rfp)
  c_correct <- c(c_correct,nrow(t3[t3$genre==t3$predict,]))
  cfp <-c(cfp,pred_fp)
  crc <- c(crc,nrow(t3[t3$genre==t3$Random,]))
  crfp <- c(crfp,rand_fp)
  
}

#creating table of false positives and accurate predictions compared against random chance by genre
length(unique(train$genre))
genre_names<-unique(train$genre)
g_count <- test %>% group_by(genre) %>% summarise(n())
g<-data.frame(genre_names,correct,false_positives,random_correct,random_false_pos)
final_data <- inner_join(g,g_count,by=c('genre_names'='genre'))
colnames(final_data) <- c("Genre", "Pred Correct", "False +", "Rand Correct", "Random False +", "Genre Count")
g_count<-data.frame(c_correct,cfp,crc,crfp,c_row)
colnames(g_count) <- c("#Pred Correct", "#False +", "#Rand Correct", "#Random False +", "Total Movie Count")
write.csv(final_data,"final_data.csv")
write.csv(g_count,"final_data_count.csv")
