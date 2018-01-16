###############
# dependencies
###############
install.packages('randomForest')
#install.packages('ParallelForest')
install.packages('parallel')

library('randomForest')
#library('ParallelForest')
library(parallel)

###############
# set seed
###############
set.seed(359244)

###################
# Load data
###################
load('li_train.Rdata')
load('li_train3.Rdata')
load('li_test_tN.Rdata')
load('li_test_tN2.Rdata')
load('user_id.Rdata')
frml <- like~likes+genre+year+group+gender+age+occu
frml2<- like~likes+group+gender+age+occu
frml3<- like~likes
frml4<- like~likes+group
frml5<- like~likes+genre+year+gender+age+occu

###################
# Parallel Programming for RF
###################
no_cores <- detectCores()-1
cl<- makeCluster(no_cores)

clusterExport(cl, c('li_train','li_train3','user.id','frml','frml2','frml3','frml4','frml5'))
clusterEvalQ(cl, library('randomForest'))

########################
# Complete (all path types, in parallel)
########################
res.par.rf <- parLapply(cl,user.id, function(i)
  randomForest(formula=frml, data=li_train[[as.character(i)]],ntree=100,mtry=2,importance=T,replace=T,keep.inbag=F)
)
save(res.par.rf, file='res-par-rf.Rdata')

########################
# Complete (all path types, single core)
########################
res.rf <- list()
for (i in 1:length(user.id)){
  user<- as.character(user.id[i])
  data<-li_train[[user]][,c('like','likes','genre','year','group','gender','age','occu')]
  res.rf[[i]]<- randomForest(formula=frml,data = data,
                                ntree=100,mtry=2,importance=T,replace=T,
                                keep.inbag = F)#,
}
save(res.rf,file='res-rf.Rdata')

######################
# Variable importance (Complete RF)
######################
# !note that res.rf needs to be changed to res.par.rf if parallel is used
varImp <- list()
sum <- matrix(0,nrow=7)
for (i in 1:length(user.id)){
  varImp[[i]]<- importance(res.par.rf[[i]],type=1)
  temp <- varImp[[i]]
  sum <- sum+temp
}
aveVarImp<- sum/length(user.id)

########################
# Aero (selection of path types, in parallel)
########################
res.par.rf.Aero <- parLapply(cl,user.id, function(i)
  randomForest(formula=frml2, data=li_train[[as.character(i)]],ntree=100,mtry=2,importance=T,replace=T,keep.inbag=F)
)
save(res.par.rf.Aero, file='res-par-rf-Aero.Rdata')

###########################
# Aero (selection of path types, single core)
###########################
res.rf.Aero<-list()
for (i in 1:length(user.id)){
  user<- as.character(user.id[i])
  data<-li_train[[user]][,c('like','likes','group','gender','age','occu')]
  res.rf.Aero[[i]]<- randomForest(formula=frml2,data = data,
                                ntree=100,mtry=2,importance=T,replace=T,
                                keep.inbag = F)
}
save(res.rf.Aero,file='res-rf-Aero.Rdata')

######################
# Variable importance (Aero RF)
######################
# !note that res.rf.Aero needs to be changed to res.par.rf.Aero if parallel is used
varImp.Aero<-list()
sum <- matrix(0,nrow=5)
for (i in 1:length(user.id)){
  varImp.Aero[[i]]<- importance(res.par.rf.Aero[[i]],type=1)
  temp <- varImp.Aero[[i]]
  sum <- sum+temp
}
aveVarImp.Aero<- sum/length(user.id)

########################
# CF (only path type like, in parallel)
########################
res.par.rf.CF <- parLapply(cl,user.id, function(i)
  randomForest(formula=frml3, data=li_train[[as.character(i)]],ntree=100,mtry=2,importance=T,replace=T,keep.inbag=F)
)
save(res.par.rf.CF, file='res-par-rf-CF.Rdata')

###########################
# CF (only path type like, single core)
###########################
res.rf.CF<-list()
for (i in 1:length(user.id)){
  user<- as.character(user.id[i])
  data<-li_train[[user]][,c('like','likes')]
  res.rf.CF[[user]]<- randomForest(formula=frml3,data = data,ntree=100)
}
save(res.rf.CF,file='res-rf-CF.Rdata')

########################
# Group (path type like and group, in parallel)
########################
res.par.rf.group <- parLapply(cl,user.id, function(i)
  randomForest(formula=frml4, data=li_train[[as.character(i)]],ntree=100,mtry=2,importance=T,replace=T,keep.inbag=F)
)
save(res.par.rf.group, file='res-par-rf-group.Rdata')

########################
# exclGroup (path type like and group, in parallel)
########################
res.par.rf.exgroup <- parLapply(cl,user.id, function(i)
  randomForest(formula=frml5, data=li_train[[as.character(i)]],ntree=100,mtry=2,importance=T,replace=T,keep.inbag=F)
)
save(res.par.rf.exgroup, file='res-par-rf-exgroup.Rdata')

###################
# Dislike (Parallel)
###################
# like: -1, 0, +1
res.par.rf.disl <- parLapply(cl,user.id, function(i)
  randomForest(formula=frml, data=li_train3[[as.character(i)]],ntree=100,mtry=2,importance=F,replace=T,keep.inbag=F)
)

save(res.par.rf.disl, file='res-par-rf-disl.Rdata')

#####################
# Recall@N 
#####################
# measuring out of sample performance
# !!! Note that if paralell is used, names need to be changes
pred.rf<-list()
pred.rf.Aero<-list()
pred.rf.CF<-list()
pred.rf.group<-list()
pred.rf.exgroup<-list()
pred.rf.disl <- list()
v_index <- rep(0,(length(user.id)*10))
v_index.Aero <- rep(0,(length(user.id)*10))
v_index.CF <- rep(0,(length(user.id)*10))
v_index.group <- rep(0,(length(user.id)*10))
v_index.exgroup <- rep(0,(length(user.id)*10))
v_index.disl <- rep(0,(length(user.id)*10))
a<-1
for (i in 1:length(user.id)){
  user<- as.character(user.id[i])
  pred.rf[[user]] <- list()
  pred.rf.Aero[[user]]<-list()
  pred.rf.CF[[user]]<-list()
  pred.rf.group[[user]]<-list()
  pred.rf.exgroup[[user]]<-list()
  pred.rf.disl[[user]] <- list()
  for (l in 1:10){
    #Complete
    pred.rf[[user]][[l]] <- rep(0,101)
    pred.rf[[user]][[l]][1]<- predict(res.par.rf[[i]], li_test_tN[[user]][[l]][like==1,])
    pred.rf[[user]][[l]][2:101] <-predict(res.par.rf[[i]], li_test_tN[[user]][[l]][like==0,])

    order<-sort(pred.rf[[user]][[l]], decreasing = T)
    liked <- pred.rf[[user]][[l]][1]
    index<-which(order==liked)
    v_index[a]<-index

    #Aero
    pred.rf.Aero[[user]][[l]] <- rep(0,101)
    pred.rf.Aero[[user]][[l]][1]<- predict(res.par.rf.Aero[[i]], li_test_tN[[user]][[l]][like==1,])
    pred.rf.Aero[[user]][[l]][2:101] <-predict(res.par.rf.Aero[[i]], li_test_tN[[user]][[l]][like==0,])

    order<-sort(pred.rf.Aero[[user]][[l]], decreasing = T)
    liked <- pred.rf.Aero[[user]][[l]][1]
    index<-which(order==liked)
    v_index.Aero[a]<-index

    #CF
    pred.rf.CF[[user]][[l]] <-rep(0,101)
    pred.rf.CF[[user]][[l]][1]<- predict(res.par.rf.CF[[i]], li_test_tN[[user]][[l]][like==1,])
    pred.rf.CF[[user]][[l]][2:101] <-predict(res.par.rf.CF[[i]], li_test_tN[[user]][[l]][like==0,])

    order<-sort(pred.rf.CF[[user]][[l]], decreasing = T)
    liked <- pred.rf.CF[[user]][[l]][1]
    index<-which(order==liked)
    v_index.CF[a]<-index

    #Group
    pred.rf.group[[user]][[l]] <-rep(0,101)
    pred.rf.group[[user]][[l]][1]<- predict(res.par.rf.group[[i]], li_test_tN[[user]][[l]][like==1,])
    pred.rf.group[[user]][[l]][2:101] <-predict(res.par.rf.group[[i]], li_test_tN[[user]][[l]][like==0,])

    order<-sort(pred.rf.group[[user]][[l]], decreasing = T)
    liked <- pred.rf.group[[user]][[l]][1]
    index<-which(order==liked)
    v_index.group[a]<-index
    
    #Excluding Group
    pred.rf.exgroup[[user]][[l]] <-rep(0,101)
    pred.rf.exgroup[[user]][[l]][1]<- predict(res.par.rf.exgroup[[i]], li_test_tN[[user]][[l]][like==1,])
    pred.rf.exgroup[[user]][[l]][2:101] <-predict(res.par.rf.exgroup[[i]], li_test_tN[[user]][[l]][like==0,])
    
    order<-sort(pred.rf.exgroup[[user]][[l]], decreasing = T)
    liked <- pred.rf.exgroup[[user]][[l]][1]
    index<-which(order==liked)
    v_index.exgroup[a]<-index

    #Disl
    pred.rf.disl[[user]][[l]] <- rep(0,101)
    pred.rf.disl[[user]][[l]][1] <- predict(res.par.rf.disl[[i]], li_test_tN2[[user]][[l]][like==1,])
    pred.rf.disl[[user]][[l]][2:101] <- predict(res.par.rf.disl[[i]],li_test_tN2[[user]][[l]][like==0,])

    order<- sort(pred.rf.disl[[user]][[l]], decreasing = T)
    liked <- pred.rf.disl[[user]][[l]][1]
    index <- which(order==liked)
    v_index.disl[a]<-index

    a<-a+1
  }
}
save(pred.rf,file='pred-rf.Rdata')
save(pred.rf.Aero,file='pred-rf-Aero.Rdata')
save(pred.rf.CF,file='pred-rf-CF.Rdata')
save(pred.rf.group,file='pred-rf-group.Rdata')
save(pred.rf.exgroup,file='pred-rf-exgroup.Rdata')
save(pred.rf.disl,file='pred-rf-disl.Rdata')

N <- c(5,10,15,20,25)
m_R.N<-matrix(0,nrow=length(N),ncol=5)
hits<-rep(0,length(N))
hits.Aero<-rep(0,length(N))
hits.CF<-rep(0,length(N))
hits.group<-rep(0,length(N))
hits.exgroup<-rep(0,length(N))
hits.disl<-rep(0,length(N))
for (i in 1:length(N)){
  n<-N[i]
  hits[i]<-sum(v_index<=n)
  m_R.N[i,1]<-hits[i]/(10*length(user.id))
  
  hits.Aero[i]<- sum(v_index.Aero<=n)
  m_R.N[i,2]<-hits.Aero[i]/(10*length(user.id))
  
  hits.CF[i]<- sum(v_index.CF<=n)
  m_R.N[i,3]<-hits.CF[i]/(10*length(user.id))
  
  hits.group[i]<- sum(v_index.group<=n)
  m_R.N[i,4]<-hits.group[i]/(10*length(user.id))
  
  hits.group[i]<- sum(v_index.exgroup<=n)
  m_R.N[i,5]<-hits.exgroup[i]/(10*length(user.id))
  
  hits.disl[i]<-sum(v_index.disl<=n)
  m_R.N[i,6]<-hits.disl[i]/(10*length(user.id))
}
rownames(m_R.N)<-N
colnames(m_R.N)<-c('Complete','Aero','CF','Group','Excl. Group','Dislike')
