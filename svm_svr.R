###############
# dependencies
###############
install.packages('e1071')
install.packages('parallel')

library(e1071)
library(parallel)

###############
# set seed
###############
set.seed(359244)

###############
# Data and parameter
###############
load('DT_train2_SVM.RData')
frml<- like~likes+genre+year+group+gender+age+occu

dt_class <- DT_train2.svm
dt_class$like <- as.factor(dt_class$like)

l <- nrow(dt_class[dt_class$like==1,])
u <- nrow(dt_class[dt_class$like==0,])
train_l<-sample(l,5000)
train_u<-sample(u,5000)
train_class <- rbind(dt_class[dt_class$like==1,][train_l,],dt_class[dt_class$like==0,][train_u,])
train_regr <- rbind(DT_train2.svm[DT_train2.svm$like==1,][train_l,],DT_train2.svm[DT_train2.svm$like==0,][train_u,])

gamma<-2^((-8:-4)*2)
cost<-2^((-1:3)*2)

######################
# Cross validation (single core)
######################

res.svm.regr<-list()
res.svm.class<-list()
a<-1
for (g in gamma){
  for (c in cost){
    res.svm.regr[[a]] <- svm(frml, data=train_regr, kernel='radial', gamma=g, cost=C, scale = TRUE, shrinking = TRUE,probability=TRUE,cross=5)
    res.svm.class[[a]] <- svm(frml, data=train_class, kernel='radial', gamma=g, cost=C, scale = TRUE, shrinking = TRUE,probability=TRUE,cross=5)
    a<-a+1
  }
}

###################
# Parallel Programming for cross validation
###################
no_cores <- detectCores()-1
cl<- makeCluster(no_cores)
clusterExport(cl, c('train_class','train_regr','gamma','cost','frml'))
clusterEvalQ(cl, library('e1071','parallel'))

res.par.class<-list()
res.par.regr<-list()
for (i in 1:length(gamma)){
  g<- gamma[i]
  clusterExport(cl, c('g'))
  res.par.class[[i]] <- parLapply(cl,cost, function(cost)
    svm(frml, data=train_class, kernel='radial', gamma=g, cost=cost, scale = TRUE, shrinking = TRUE,probability=TRUE,cross=5))
  res.par.regr[[i]] <- parLapply(cl,cost, function(cost)
    svm(frml, data=train_regr, kernel='radial', gamma=g, cost=cost, scale = TRUE, shrinking = TRUE,probability=TRUE,cross=5))
}
save(res.par.class, file='res-par-class.Rdata')
save(res.par.regr, file='res-par-regr.Rdata')


m_acc<-matrix(0,nrow=length(gamma),ncol=length(cost))
m_MSE<-matrix(0,nrow=length(gamma),ncol=length(cost))
for (i in 1:length(gamma)){
  for (j in 1:length(cost)){
    m_acc[i,j] <- res.par.class[[i]][[j]]$tot.accuracy
    m_MSE[i,j] <- res.par.regr[[i]][[j]]$tot.MSE
  }
}
max_acc <- which(m_acc==max(m_acc),arr.ind = TRUE)
class_g <- gamma[max_acc[1,1]]
class_c <- cost[max_acc[1,2]]
min_MSE <- which(m_MSE==min(m_MSE),arr.ind = TRUE)
regr_g <- gamma[min_MSE[1,1]]
regr_c <- cost[min_MSE[1,2]]

#####################
# Final SVM models
#####################
res.svm.regr <- svm(frml, data=train_regr, kernel='radial', gamma=regr_g, cost=regr_c, scale = TRUE, shrinking = TRUE)
res.svm.class <- svm(frml, data=train_class, kernel='radial', gamma=class_g, cost=class_c, scale = TRUE, shrinking = TRUE)

########################
# Recall@N 
########################
# Out of sample performance measure
pred.svm.class<-list()
pred.svm.regr<-list()
v_index.class <- rep(0,(length(user.id)*10))
v_index.regr <- rep(0,(length(user.id)*10))
a<-1
for (i in 1:length(user.id)){
  user<- as.character(user.id[i])
  pred.svm.class[[user]] <- list()
  pred.svm.regr[[user]]<-list()
  for (l in 1:10){
    #Class
    pred.svm.class[[user]][[l]] <- rep(0,101)
    pred.svm.class[[user]][[l]][1]<- attr(predict(res.svm.class, li_test_tN[[user]][[l]][like==1,],decision.values = T),'decision.values')
    pred.svm.class[[user]][[l]][2:101] <-attr(predict(res.svm.class, li_test_tN[[user]][[l]][like==0,],decision.values = T),'decision.values')
    
    order<-sort(pred.svm.class[[user]][[l]],decreasing = T)
    liked <- pred.svm.class[[user]][[l]][1]
    index<-which(order==liked)
    v_index.class[a]<-index
    
    #Regr
    pred.svm.regr[[user]][[l]] <-rep(0,101)
    pred.svm.regr[[user]][[l]][1]<- predict(res.svm.regr, li_test_tN[[user]][[l]][like==1,])
    pred.svm.regr[[user]][[l]][2:101] <-predict(res.svm.regr, li_test_tN[[user]][[l]][like==0,])
    
    order<-sort(pred.svm.regr[[user]][[l]], decreasing = T)
    liked <- pred.svm.regr[[user]][[l]][1]
    index<-which(order==liked)
    v_index.regr[a]<-index
    
    a<-a+1
  }
}
save(pred.svm.class,file='pred-svm-class.Rdata')
save(pred.svm.regr,file='pred-svm-regr.Rdata')

N <- c(5,10,15,20,25)
m_R.N.svm<-matrix(0,nrow=length(N),ncol=2)
hits.class<-rep(0,length(N))
hits.regr<-rep(0,length(N))
for (i in 1:length(N)){
  n<-N[i]
  hits.class[i]<- sum(v_index.class<=n)
  m_R.N.svm[i,1]<-hits.class[i]/(10*length(user.id))
  
  hits.regr[i]<- sum(v_index.regr<=n)
  m_R.N.svm[i,2]<-hits.regr[i]/(10*length(user.id))
} 
rownames(m_R.N.svm)<-N
colnames(m_R.N.svm)<-c('Class', 'Regr')
