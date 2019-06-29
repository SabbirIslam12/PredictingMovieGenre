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
data_wide
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
test_8 <- spread(test_7,actor_group,count)
test_9 <- unique(test_5[,c(-4,-5)])

test_10 <- inner_join(test_9,test_8)
test_10[is.na(test_10)]<-0
colnames(test_10)<-c('movie_id','genre','director_id','g_1','g_2','g_3','g_4','g_5')

test_11 <- inner_join(test_10,director_group)
test_11
test_11 %>% group_by(genre) %>% summarise(n())
e_genre <- test_11 %>% group_by(genre) %>% summarise(n()) %>% filter(`n()`>30) #taking out genres w/ less than 30 data points
t11 <- test_11[test_11$genre %in% e_genre$genre,]
#creating test and training set
test.sample <- sample(nrow(test_11), floor(nrow(test_11)*.1))
test <- t11[test.sample,]
train <- t11[-test.sample,]

acc <- c()
fal <- c()

#running logit regression to predict each genre of movies using #of actors in each group and director group and finding false positives and accurate positives
for(i in 1:length(unique(train$genre))){
  gen<-unique(train$genre)[i]
  tr2 <- train
  tr2$genre<-if_else(tr2$genre==gen,1,0)
  neg <- unique(tr2[!tr2$movie_id %in% tr2[tr2$genre==1,]$movie_id,])
  pos<-tr2[tr2$movie_id %in% tr2[tr2$genre==1,]$movie_id&tr2$genre==1,]
  tr3 <- rbind(neg,pos)
  logit.fit <- glm(as.factor(genre) ~ as.factor(director_group)+g_1+g_2+g_3+g_4+g_5, data =tr3,family = "binomial")
  
  t2 <- test
  t2$genre<-if_else(t2$genre==gen,1,0)
  neg <- unique(t2[!t2$movie_id %in% t2[t2$genre==1,]$movie_id,])
  pos<-t2[t2$movie_id %in% t2[t2$genre==1,]$movie_id&t2$genre==1,]
  t3 <- rbind(neg,pos)
  
  logit.predict <- predict(logit.fit,t3,type='response')
  pred <- ifelse(logit.predict>=.5,1,0)
  t3$predict <- pred
  p <- nrow(t3[t3$genre==t3$predict,])/nrow(t3)
  negs <- t3[t3$movie_id %in% neg$movie_id,]
  fp <- nrow(negs[negs$genre!=negs$predict,])/nrow(negs)
  acc <- c(acc,p)
  fal <- c(fal,fp)
  
}

#creating table of false positives and accurate predictions by genre
length(unique(train$genre))
genre_names<-unique(train$genre)
g_count <- test_11 %>% group_by(genre) %>% summarise(n()) %>% filter(`n()`>30)
g<-data.frame(genre_names,acc,fal)
inner_join(g,g_count,by=c('genre_names'='genre'))
g_count