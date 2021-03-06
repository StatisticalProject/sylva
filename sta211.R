require(ade4)
library(randomForest)
library(rpart)
library(rpart.plot)
library(neuralnet)

train.data <- read.table("data/sylva_train.data")
train.labels <- read.table("data/sylva_train.labels")

train.labels$V1 <- as.factor(train.labels$V1)
colnames(train.labels)[1] <- "Y"

#train.full=(cbind(train.labels,train.data))

set.seed(100) 

  
train.quali<-train.data[,c(1,2,4:6,8,9,11:17,19:40,44:49,51:54,56:74,77:80,82:87,90:99,101:103,106:109,111,112,114:116,118:124,128:138,140:142,144:146,148,149,151:154,156:165,167:170,172:178,180,181,184:186,189,190,192:200,202:204,206,207,209,210,212:216)]
train.quant<-train.data[,c(3,7,10,18,41,42,43,50,55,75,76,81,88,89,100,104,105,110,113,117,125,126,127,139,143,147,150,155,166,171,179,182,183,187,188,191,201,205,208,211)]
ncol(train.quali)+ncol(train.quant)
ncol(train.data)
for(i in 1:ncol(train.quali)){
  train.quali[,i]=as.factor(train.quali[,i])
}



train.qualiFact<-train.quant
for(i in 1:ncol(train.qualiFact)){
  train.qualiFact[,i]=cut(train.qualiFact[,i], breaks=quantile(train.qualiFact[,i]),include.lowest=TRUE)
}

train.FullAcm<-cbind(train.qualiFact,train.quali)
train.full<-cbind(train.labels,train.FullAcm)

set.seed(15) 
bound <- floor(nrow(train.full)*0.7)         
bound2<-floor((nrow(train.full)-bound)*2.0/3.0)
df <- train.full[sample(nrow(train.full)), ]           #sample rows 
train.learn <- df[1:bound, ]              #get training set
train.validation <- df[(bound+1):(bound+bound2), ]    #get test set

train.test <- df[(bound+bound2+1):nrow(df), ]    #get test set

train.FullAcm<-train.learn[,2:ncol(train.full)]
train.labels<-train.learn[,1]


train.qualAcm<-dudi.acm(train.FullAcm,nf=ncol(train.FullAcm),scannf = FALSE)

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

colnames(train.full)[1] <- "Y"
dataTree <- rpart(Y ~ ., data=train.full)
plotcp(dataTree)
dataTreeSimple <- prune(dataTree,cp=0.013)
prp(dataTree,extra=1)
prp(dataTreeSimple,extra=1)
table(train.full$Y, predict(dataTree, train.full, type="class"))
table(train.full$Y, predict(dataTreeSimple, train.full, type="class"))
colnames(train.test)[1] <- "Y"
train.suprow<-acm.disjonctif(train.test[,2:ncol(train.test)])
colw <- train.qualAcm$cw*ncol(train.FullAcm)
train.suprow <- data.frame(t(t(train.suprow)/colw) - 1)
newValue<-suprow(train.qualAcm, train.suprow)
newFull<-cbind(train.test[,1],newValue$lisup[,1:kaiserLimitCol])
table(train.test$Y, predict(dataTree, newFull, type="class"))
table(train.test$Y, predict(dataTreeSimple, newFull, type="class"))

train.rf <- randomForest(Y ~ ., data=train.full, importance=TRUE,proximity=TRUE)
Prediction <- predict(train.rf, newFull)
table(train.test$Y,Prediction)
Prediction <- predict(train.rf, train.full)
table(train.full$Y,Prediction)

n <- names(train.full[,1:60])
f <- as.formula(paste("Y1 + Y2 ~", paste(n[!n %in% "Y"], collapse = " + ")))

full<-train.full[,-(1)]
full<-cbind(train.full[,1]=="-1",full)
full<-cbind(train.full[,1]=="1",full)
colnames(full)[1] <- "Y1"
colnames(full)[2] <- "Y2"

nn <- neuralnet(f, data=full[,1:62],hidden=c(10),linear.output=F,rep=1000)
pr.nn <- compute(nn,newFull[,2:ncol(newFull)][,1:60])
rewritedRes<- list(1:nrow(pr.nn$net.result))
reW<-function(x) {
  result=1
  if (x[1]>=0.5){
    result=-1
  }
  return(result)
}
table(train.test$Y,apply(pr.nn$net.result, 1, reW))

pr.nn <- compute(nn,full[,3:ncol(full)])
rewritedRes<- list(1:nrow(pr.nn$net.result))
reW<-function(x) {
  result=1
  if (x[1]>=0.5){
    result=-1
  }
  return(result)
}
table(train.full$Y,apply(pr.nn$net.result, 1, reW))

