###############
# dependencies
###############
install.packages("flexclust")
install.packages('parallel')
install.packages('fpc')
install.packages('ggplot2')

library(flexclust)
library(parallel)
library(fpc)
library(ggplot2)


###############
# set seed
###############
set.seed(359244)

###################
# Load data
###################


######################
# parallel programming
######################
no_cores <- detectCores()-1
cl <- makeCluster(no_cores)
clusterCall(cl, function() require("flexclust"))
clusterEvalQ(cl, library('flexclust'))
clusterExport(cl, c('df3'))

##################
# cluster analysis
##################
# explore data
hclusters <- hclust(dist(df3))
plot(hclusters,labels=F,xlab=NA,sub=NA)

# bootstrap 
output_bootFlex <- bootFlexclust(df3,k=2:13,nboot=100,FUN=kcca, nrep=10,correct=TRUE,multicore=cl)
save(output_bootFlex,file='output_bootFlex.RData')

# plot adjusted rand index
summary(output_bootFlex)
plot(output_bootFlex, col=flxColors(color="medium"), main="adjusted Rand indices")
densityplot(output_bootFlex, from=0)

# Calinski-Harabasz
centers1<-output_bootFlex@centers1
centers2<-output_bootFlex@centers2
cluster1<-output_bootFlex@cluster1
cluster2<-output_bootFlex@cluster2
index1<-output_bootFlex@index1
index2<-output_bootFlex@index2

CH1<-matrix(NA,nrow=100*12,ncol=2)
CH2<-matrix(NA,nrow=100*12,ncol=2)
a<-1
for(j in 1:12){
  for(i in 1:100){
    df_boot1<-df3[index1[,i],]
    cl_boot1<-cluster1[,j,i]
    CH1[a,1]<-calinhara(df_boot1,cl_boot1)
    CH1[a,2]<-j+1
    df_boot2<-df3[index2[,i],]
    cl_boot2<-cluster2[,j,i]
    CH2[a,1]<-calinhara(df_boot2,cl_boot2)
    CH2[a,2]<-j+1
    a<-a+1
  }
}

df_CH1<-as.data.frame(CH1)
df_CH1$V2<-as.factor(df_CH1$V2)
df_CH2<-as.data.frame(CH2)
df_CH2$V2<-as.factor(df_CH2$V2)

df_CH <-rbind(df_CH1,df_CH2)
names(df_CH)<- c('Calinki.Harabasz.Index','Clusters')
save(df_CH,file='df_CH.RData')

#plot boxplot of CH indices
ggplot(df_CH, aes(x=Clusters, y=Calinki.Harabasz.Index)) + 
  geom_boxplot(
    # custom boxes
    color="blue",
    fill="blue",
    alpha=0.2,
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3
  )


##################
# Final Clustering
##################
res_kmeans<-stepFlexclust(df3, k=10, nrep=100, verbose=FALSE, FUN = kcca, drop=TRUE,
                          group=NULL, simple=FALSE, save.data=FALSE, seed=NULL,
                          multicore=TRUE)
clusters<-res_kmeans@cluster

df4<-data.frame(UserId=userid,clusters=clusters)
df4$clusters <- as.factor(df4$clusters)
dt_res_clust<-df4

save(dt_res_clust,file='dt_res_clust.Rdata')

####################
# Descriptive stats
####################
# per cluster and on average

temp<-dt_cluster
temp2<- acm.disjonctif(temp[,c(2:4)])
temp<-cbind(df[,c(1,5:24)],temp2)

dt_res_clust2<-merge(temp,dt_res_clust,by='UserId')
dt_res_clust2<- dt_res_clust2[,-c(1)]

dt_ave_clust <- dt_res_clust2[order(clusters),lapply(.SD,mean),by=clusters] #lapply(.SD, mean)
dt_ave <- colMeans(dt_res_clust2[,-c(51)])
dt_ave_clust2 <- rbind(dt_ave_clust[,2:51], t(dt_ave))
rownames(dt_ave_clust2)[11] <- c('Ave')
write.csv(dt_ave_clust2, "clustersDescrStat.csv", row.names = T)
