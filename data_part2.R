###############################
# After clustering of the users
###############################
load('dt_res_clust.Rdata')

# only consider users with 25 or more positive ratings 
like_count<-dt_users_ratings_movies[like==1,.(likeSum=sum(like)), by=UserId]
like_count<-like_count[likeSum>=25,,]

dt_users_ratings_movies_clusters<-merge(dt_users_ratings_movies,dt_res_clust,by='UserId')
dt_urmcl<- merge(dt_users_ratings_movies_clusters,like_count,by='UserId')
dt_urmcl$year<-dt_urmcl[,.(year=str_sub(title, start= -6)),]
dt_urmcl$year<-dt_urmcl[,.(year=str_sub(year, -5,-2)),]
dt_urmcl$year<-as.factor(dt_urmcl$year)
dt_urmcl<-dt_urmcl[like==1,,]
dt_urmcl<-dt_urmcl[,c('Zip-code','Rating','Timestamp','title','genres','likeSum'):=NULL]

# create adjacency matrices
user_movie_matrix <- as.matrix(ifelse(table(dt_urmcl$UserId,dt_urmcl$movieId) > 0, 1, 0))
movie_year_matrix <- as.matrix(ifelse(table(dt_urmcl$movieId,dt_urmcl$year) > 0, 1, 0))
user_group_matrix <- as.matrix(ifelse(table(dt_urmcl$UserId,dt_urmcl$clusters) > 0, 1, 0))
group_movie_matrix <- as.matrix(ifelse(table(dt_urmcl$clusters,dt_urmcl$movieId) > 0, 1, 0))
user_gender_matrix <- as.matrix(ifelse(table(dt_urmcl$UserId,dt_urmcl$Gender) > 0, 1, 0))
gender_movie_matrix <- as.matrix(ifelse(table(dt_urmcl$Gender,dt_urmcl$movieId) > 0, 1, 0))
user_age_matrix <- as.matrix(ifelse(table(dt_urmcl$UserId,dt_urmcl$Age) > 0, 1, 0))
age_movie_matrix <- as.matrix(ifelse(table(dt_urmcl$Age,dt_urmcl$movieId) > 0, 1, 0))
user_occu_matrix <- as.matrix(ifelse(table(dt_urmcl$UserId,dt_urmcl$Occupation) > 0, 1, 0))
occu_movie_matrix <- as.matrix(ifelse(table(dt_urmcl$Occupation,dt_urmcl$movieId) > 0, 1, 0))

col.name<-colnames(dt_urmcl)
col.name[19]<-'SciFi'
col.name[25]<-'FilmNoir'
col.name[26]<-'no_genres_listed'
colnames(dt_urmcl)<-col.name

genre <- dt_urmcl[order(movieId),.(Adventure=first(Adventure),Animation=first(Animation),Children=first(Children),Comedy=first(Comedy),Fantasy=first(Fantasy),Romance=first(Romance),Drama=first(Drama),Action=first(Action),Crime=first(Crime),Thriller=first(Thriller),Horror=first(Horror),Mystery=first(Mystery),SciFi=first(SciFi),IMAX=first(IMAX),Documentary=first(Documentary),War=first(War),Musical=first(Musical),Western=first(Western),FilmNoir=first(FilmNoir)),by=movieId]

movie_genre_matrix<- as.matrix(genre[,2:19])
rownames(movie_genre_matrix)<-genre$movieId

# create commuting matrices
m_genre<-user_movie_matrix %*% movie_genre_matrix %*% t(movie_genre_matrix)
m_year<-user_movie_matrix %*% movie_year_matrix %*% t(movie_year_matrix)
m_likes<-user_movie_matrix %*% t(user_movie_matrix) %*% user_movie_matrix
m_group<-user_group_matrix %*% t(user_group_matrix) %*% user_movie_matrix
m_gender<-user_gender_matrix %*% t(user_gender_matrix) %*% user_movie_matrix
m_age<-user_age_matrix %*% t(user_age_matrix) %*% user_movie_matrix
m_occu<-user_occu_matrix %*% t(user_occu_matrix) %*% user_movie_matrix

# create path count matrix
user.id <- as.numeric(rownames(user_movie_matrix))
movie.id <- as.numeric(colnames(user_movie_matrix))
v_userid<- rep(user.id,length(movie.id))
v_movieid <- rep(movie.id,length(user.id))
v_movieid <- sort(v_movieid)
# vectorize commuting matrices 
DT_user_movie <- data.table(UserId=v_userid,movieId=v_movieid,likes=as.vector(m_likes),genre=as.vector(m_genre),year=as.vector(m_year),group=as.vector(m_group),gender=as.vector(m_gender),age=as.vector(m_age),occu=as.vector(m_occu))

# add information about like movies
dt_like <- dt_users_ratings_movies[,c('UserId','movieId','like')]
DT <- merge(DT_user_movie,dt_like,by=c('UserId','movieId'),all.x=T)
DT[is.na(DT)] <- 0
# group dislike and unrated movies together
DT_0 <- DT
DT_0[like==-1,c('like')] <- 0

rm(user_age_matrix,user_gender_matrix,user_group_matrix,user_movie_matrix,user_occu_matrix,movie_genre_matrix,movie_year_matrix,occu_movie_matrix,group_movie_matrix,gender_movie_matrix,age_movie_matrix)
rm(m_genre,m_year,m_likes,m_group,m_gender,m_age,m_occu)

######################
# Split in train and test
######################
#test contains for each user 10 liked movies and 1000 unknown
test_zero <- rep(c(0),(length(user.id)*1010))
train_zero <- rep(c(0),((length(movie.id)-1010)*length(user.id)))
DT_test <- data.table(UserId=as.integer(test_zero),movieId=as.integer(test_zero),likes=test_zero,genre=test_zero,year=test_zero,group=test_zero,gender=test_zero,age=test_zero,occu=test_zero,like=as.numeric(test_zero))
DT_train <- data.table(UserId=as.integer(train_zero),movieId=as.integer(train_zero),likes=train_zero,genre=train_zero,year=train_zero,group=train_zero,gender=train_zero,age=train_zero,occu=train_zero,like=as.numeric(train_zero))
DT_train.svm <- data.table(UserId=as.integer(train_zero),movieId=as.integer(train_zero),likes=train_zero,genre=train_zero,year=train_zero,group=train_zero,gender=train_zero,age=train_zero,occu=train_zero,like=as.numeric(train_zero))
DT_l <- DT_0[like==1,,]
DT_u <- DT_0[like==0,,]

for (i in user.id){
  if (i==1){
    a<-0
    s<-1
    s.svm<-1
    e<-0
    e.svm<-0
    li_train<-list()
    li_test<-list()
    li_test_tN<-list()
  }
  DT_0_i <- DT_0[UserId==i,,]
  DT_l_i <- DT_l[UserId==i,,]
  DT_u_i <- DT_u[UserId==i,,]
  
  liked_movieId <- dt_urmcl[UserId==i,movieId,]
  
  test_liked <- sample(liked_movieId,10,replace=F)
  train_liked <- setdiff(liked_movieId,test_liked)
  unl_movieId <- setdiff(movie.id,liked_movieId)
  test_unl <- sample(unl_movieId,1000,replace=F)
  
  li_test_tN[[as.character(i)]]<-list()
  for (l in 1:10){
    test_mId <- union(test_liked[l],test_unl[((l-1)*100+1):(l*100)])
    temp_test <- DT_0_i[DT_0_i$movieId %in% test_mId,]
    li_test_tN[[as.character(i)]][[l]] <- temp_test
  }
  
  q<-length(train_liked)
  train_unl <- setdiff(unl_movieId,test_unl)
  if(length(train_unl)>(q*2)){
    train_unl <- sample(train_unl,(q*2))
  }
  test_mId <- union(test_liked,test_unl)
  train_mId <- union(train_liked,train_unl)
  if(length(train_unl)>q){
    train.svm_mId <-union(train_liked,train_unl[1:q])
  } else {
    train.svm_mId <-union(train_liked,train_unl)
  }
  
  temp_train <- DT_0_i[DT_0_i$movieId %in% train_mId,]
  temp_train.svm <- DT_0_i[DT_0_i$movieId %in% train.svm_mId,]
  
  k<-nrow(temp_train)
  k.svm<-nrow(temp_train.svm)
  
  e<-e+k
  e.svm <-e+k
  
  DT_train[s:e,]<-temp_train
  DT_train.svm[s.svm:e.svm,]<-temp_train.svm
  
  a<-a+1
  s<-e+1
  s.svm<-e.svm+1
  
  li_train[[as.character(i)]]<-temp_train
}
DT_train2<-DT_train[UserId>0,]
DT_train2.svm<-DT_train.svm[UserId>0,]
save(DT_train2,file='DT_train2.RData')
save(DT_train2.svm,file='DT_train2_SVM.RData')

save(li_train,file='li_train.RData')
save(li_test_tN,file='li_test_tN.Rdata')
load('DT_train2.RData')

#########################
# incl disliked info
#########################
dt_urmcd<- merge(dt_users_ratings_movies_clusters,like_count,by='UserId')
dt_urmcd<- dt_urmcd[like==-1,,]

DT_test3 <- data.table(UserId=as.integer(test_zero),movieId=as.integer(test_zero),likes=test_zero,genre=test_zero,year=test_zero,group=test_zero,gender=test_zero,age=test_zero,occu=test_zero,like=as.numeric(test_zero))
DT_train3 <- data.table(UserId=as.integer(train_zero),movieId=as.integer(train_zero),likes=train_zero,genre=train_zero,year=train_zero,group=train_zero,gender=train_zero,age=train_zero,occu=train_zero,like=as.numeric(train_zero))
DT_train3.svm <- data.table(UserId=as.integer(train_zero),movieId=as.integer(train_zero),likes=train_zero,genre=train_zero,year=train_zero,group=train_zero,gender=train_zero,age=train_zero,occu=train_zero,like=as.numeric(train_zero))


DT_l <- DT[like==1,,]
DT_u <- DT[like==0,,]
DT_d <- DT[like==-1,,]

for (i in user.id){
  if (i==1){
    a<-0
    s<-1
    s.svm<-1
    e<-0
    e.svm<-0
    li_train3<-list()
    li_train4<-list()
    li_test3<-list()
    li_test4<-list()
    li_test_tN2<-list()
  }
  DT_i <- DT[UserId==i,,]
  DT_l_i <- DT_l[UserId==i,,]
  DT_u_i <- DT_u[UserId==i,,]
  DT_d_i <- DT_d[UserId==i,,]
  
  liked_movieId <- dt_urmcl[UserId==i,movieId,]
  disl_movieId <- dt_urmcd[UserId==i,movieId,]
  unl_movieId <- setdiff(setdiff(movie.id,liked_movieId),disl_movieId)
  
  test_liked <- sample(liked_movieId,10,replace=F)
  train_liked <- setdiff(liked_movieId,test_liked)
  
  test_unl <- sample(unl_movieId,1000,replace=F)
  li_test_tN2[[as.character(i)]]<-list()
  for (l in 1:10){
    test_mId <- union(test_liked[l],test_unl[((l-1)*100+1):(l*100)])
    temp_test <- DT_i[DT_i$movieId %in% test_mId,]
    li_test_tN2[[as.character(i)]][[l]] <- temp_test
  }
  
  train_disl <- disl_movieId
  
  q<-length(train_liked)
  
  train_unl <- setdiff(unl_movieId,test_unl)
  train_unl.svm <-setdiff(unl_movieId,test_unl)
  d <- length(train_disl)
  if(length(train_unl)>(2*q)){
    train_unl <- sample(train_unl,(2*q), replace=F)
  }
  if(length(train_unl.svm)>q){
    train_unl.svm <- sample(train_unl.svm,q, replace=F)
  }
  train_mId <- union(union(train_liked,train_disl),train_unl)
  train_mId.svm <- union(union(train_liked,train_disl),train_unl.svm)
  
  temp_train <- DT_i[DT_i$movieId %in% train_mId,]
  temp_train.svm <- DT_i[DT_i$movieId %in% train_mId.svm,]
  
  k<-nrow(temp_train)
  k.svm<-nrow(temp_train.svm)
  e<-e+k
  e.svm<-e.svm+k
  
  DT_train3[s:e,]<-temp_train
  DT_train3.svm[s.svm:e.svm,]<-temp_train.svm
  
  a<-a+1
  s<-e+1
  s.svm<-e.svm+1
  
  li_train3[[as.character(i)]]<-DT_train3[UserId==i,]
}
DT_train3<-DT_train3[UserId>0,]
DT_train3.svm<-DT_train3.svm[UserId>0,]
save(DT_train3,file='DT_train3.RData')
save(DT_train3.svm,file='DT_train3_SVM.RData')
save(li_train3,file='li_train3.RData')
save(li_test_tN2,file='li_test_tN2.RData') 