####################
# dependencies
####################
install.packages('data.table')
install.packages('rebus')
install.packages('stringr')
install.packages('ade4')

library(rebus)
library(stringr)
library(data.table)
library(ade4)

####################
# working directory
####################
#getwd()
setwd("C:/Users/jetze/OneDrive/Documenten/Msc/2017 - Machine Learning/Final Assignment")

####################
# set seed
####################
set.seed(359244)

####################
# import data
####################
#ratings
#readLines("G:/OneDrive/Documenten/Msc/2017 - Machine Learning/Final Assignment/Data/ml-1m/ratings.dat", n=10)
M1_ratings<-read.table("Data/ml-1m/ratings.dat",header=FALSE, sep = ":")
M1_ratings<-M1_ratings[,-c((1:3)*2)]
dt_M1_ratings<-as.data.table(M1_ratings)
rm(M1_ratings)
setnames(dt_M1_ratings, c('UserId','movieId','Rating','Timestamp'))
dt_M1_ratings$like<-ifelse(dt_M1_ratings$Rating>3 , 1,-1)  

#users
M1_users<-read.table("Data/ml-1m/users.dat",header=FALSE, sep = ":")
M1_users<-M1_users[,-c((1:4)*2)]
M1_users$V3<-as.factor(M1_users$V3)
M1_users$V5<-as.factor(M1_users$V5)
M1_users$V7 <-as.factor(M1_users$V7)
dt_M1_users<-as.data.table(M1_users)
setnames(dt_M1_users, c('UserId','Gender','Age','Occupation','Zip-code'))
rm(M1_users)

#movies
M20_movies<-read.table("Data/ml-20m/movies.csv", fill=TRUE,quote="\"",header=T, sep = ",", encoding="UTF-8")
M20_movies_the<-M20_movies
M20_movies_the$title<-gsub(", The", "", M20_movies$title)
M20_movies_genre<-M20_movies_the
temp<-strsplit(as.character(M20_movies$genres),split="|",fixed=T)
n <- nrow(M20_movies_genre)
for (i in 1:n){
  genres <- temp[[i]]
  k <- length(genres)
  if (k>0){
    for (j in 1:k){
      if(genres[j] %in% names(M20_movies_genre)){
        l <- match(genres[j],names(M20_movies_genre))
        M20_movies_genre[i,l]<- 1
      } else {
        df<- data.frame(rep(0,n))
        colnames(df)<-genres[j]
        df[i,1]<-1
        M20_movies_genre<-cbind(M20_movies_genre,df)
      }
    }
  }
}
rm(temp)
rm(df)

dt_M20_movies<-as.data.table(M20_movies_genre)
rm(M20_movies,M20_movies_the,M20_movies_genre)

#merging
dt_users_ratings <- merge(dt_M1_users,dt_M1_ratings)
dt_users_ratings_movies <- merge(dt_users_ratings,dt_M20_movies, by='movieId')

#transform for clustering
dt_cluster<- dt_users_ratings_movies
col.name<-colnames(dt_cluster)
col.name[24]<-'SciFi'
col.name[30]<-'FilmNoir'
col.name[31]<-'no_genres_listed'
colnames(dt_cluster)<-col.name
dt_cluster[(dt_cluster$like==-1),c(12:31)]<- (-1)*dt_cluster[(dt_cluster$like==-1),c(12:31)]
dt_cluster<-dt_cluster[ ,.(Gender=first(Gender),Age=first(Age),Occupation=first(Occupation),
                           like=mean(like),Adventure=sum(Adventure),Animation=sum(Animation),
                           Children=sum(Children),Comedy=sum(Comedy),Fantasy=sum(Fantasy),
                           Romance=sum(Romance),Drama=sum(Drama),Action=sum(Action),
                           Crime=sum(Crime),Thriller=sum(Thriller),Horror=sum(Horror),
                           Mystery=sum(Mystery),SciFi=sum(SciFi),IMAX=sum(IMAX),
                           Documentary=sum(Documentary),War=sum(War),Musical=sum(Musical),
                           Western=sum(Western),FilmNoir=sum(FilmNoir)), 
                        by=UserId]
userid<-dt_cluster$UserId

df<-dt_cluster #average rating #total number of ratings
df[,UserId:=NULL]
df2<- acm.disjonctif(df[,c(1:3)])
df<-cbind(df[,c(4:23)],df2)
df3<- cbind(scale(df[,c(4:23)]),df2)

# Save data.frame for Cluster analysis
save(df3,file='df3.Rdata')