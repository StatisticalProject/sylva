require(ade4)
library(randomForest)
library(rpart)
library(rpart.plot)

train.data <- read.table("data/sylva_train.data")
train.labels <- read.table("data/sylva_train.labels")
train.labels$V1 <- as.factor(train.labels$V1)
colnames(train.labels)[1] <- "Y"
train.full=(cbind(train.labels,train.data))
train.quali<-train.data[,c(1,2,4:6,8,9,11:17,19:40,44:49,51:54,56:74,77:80,82:87,90:99,101:103,106:109,111,112,114:116,118:124,128:138,140:142,144:146,148,149,151:154,156:165,167:170,172:178,180,181,184:186,189,190,192:200,202:204,206,207,209,210,212:216)]
train.quant<-train.data[,c(3,7,10,18,41,42,43,50,55,75,76,81,88,89,100,104,105,110,113,117,125,126,127,139,143,147,150,155,166,171,179,182,183,187,188,191,201,205,208,211)]
ncol(train.quali)+ncol(train.quant)
ncol(train.data)
for(i in 1:ncol(train.quali)){
  train.quali[,i]=as.factor(train.quali[,i])
}

train.qualAcm<-dudi.acm(train.quali,nf=ncol(train.quali),scannf = FALSE)
burtd<-acm.burt(train.quali,train.quali)

train.qualiFact<-train.quant
for(i in 1:ncol(train.qualiFact)){
  train.qualiFact[,i]=cut(train.qualiFact[,i], breaks=quantile(train.qualiFact[,i]),include.lowest=TRUE)
}

train.qualAcm<-dudi.acm(cbind(train.qualiFact,train.quali),nf=ncol(train.quali),scannf = FALSE)

quantile(train.qualiFact[,3])
train.qualAcm$eig
barplot(train.qualAcm$eig)
#critere de kaiser
kaiserLimit<-1/ncol(train.data)
kaiserLimitCol<-ncol(train.data)
for(i in 1:ncol(train.data)){
  if(train.qualAcm$eig[i]<kaiserLimit)
  {
    break
  }
  kaiserLimitCol<-i
}

#make with the label



train.full<-(cbind(train.labels,train.qualAcm$li[,1:kaiserLimitCol]))
dataTree <- rpart(Y ~ ., data=train.full)
plotcp(dataTree)
dataTreeSimple <- prune(dataTree,cp=0.014)
prp(dataTree,extra=1)
prp(dataTreeSimple,extra=1)
table(train.full$Y, predict(dataTree, train.full, type="class"))
table(train.full$Y, predict(dataTreeSimple, train.full, type="class"))

iris.rf <- randomForest(Y ~ ., data=train.full, importance=TRUE,proximity=TRUE)
