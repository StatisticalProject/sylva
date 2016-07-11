#librairie utilise pendant l analyse
library(ade4)
library(lattice)
library(gridExtra)
library(FactoMineR)
library(randomForest)
#Jeu de donnee SYLVA
#Chargement des donnees d'entrainement et de validation
train.data <- read.table("data/sylva_train.data")
train.labels <- read.table("data/sylva_train.labels")
valid.data <- read.table("data/sylva_valid.data")
valid.labels <- read.table("data/sylva_valid.labels")

#on change la variable de resultat Y en facteur
train.labels$V1 <- as.factor(train.labels$V1)
colnames(train.labels)[1] <- "Y"
valid.labels$V1 <- as.factor(valid.labels$V1)
colnames(valid.labels)[1] <- "Y"


## Partie Analyse

#Analyse univarie
nb<-ncol(train.data)
#generation de boxplot pour toutes les variables 
png("boxplot.png",width = 2400, height = 1400)
par(mfrow=c(12,18),mar=c(1.0,1.8,1.0,1.0))
for (i in 1:nb){
    boxplot(train.data[,i],main = paste("Variable",i))
}
dev.off()

#La variable d'interet
summary(train.labels)

#Separation des variables
#Suppression des variables unaires :15 ,73 , 145,159,213
train.quali<-train.data[,c(1,2,4:6,8,9,11:14,16,17,19:40,44:49,51:54,56:72,74,77:80,82:87,90:99,101:103,106:109,111,112,114:116,118:124,128:138,140:142,144,146,148,149,151:154,156:158,160:165,167:170,172:178,180,181,184:186,189,190,192:200,202:204,206,207,209,210,212,214:216)]
train.quant<-train.data[,c(3,7,10,18,41,42,43,50,55,75,76,81,88,89,100,104,105,110,113,117,125,126,127,139,143,147,150,155,166,171,179,182,183,187,188,191,201,205,208,211)]
valid.quali <- valid.data[,c(1,2,4:6,8,9,11:14,16,17,19:40,44:49,51:54,56:72,74,77:80,82:87,90:99,101:103,106:109,111,112,114:116,118:124,128:138,140:142,144,146,148,149,151:154,156:158,160:165,167:170,172:178,180,181,184:186,189,190,192:200,202:204,206,207,209,210,212,214:216)]
valid.quant <- valid.data[,c(3,7,10,18,41,42,43,50,55,75,76,81,88,89,100,104,105,110,113,117,125,126,127,139,143,147,150,155,166,171,179,182,183,187,188,191,201,205,208,211)]

ncol(train.quali)
ncol(train.quant)

#transforme les variables qualitatives en facteur
quali <- as.data.frame(lapply(rbind(train.quali,valid.quali), as.factor)) 
valid.quali <- quali[(nrow(train.quali)+1):nrow(quali),]
train.quali <- quali[1:nrow(train.quali),]

#Tableau de burt des variables quantitatives
burtd<-acm.burt(train.quali,train.quali)
n <- names(burtd)
burtd.0=burtd[grep(pattern = ".0$", x = n),grep(pattern = ".0$", x = n)]
burtd.1=burtd[grep(pattern = ".1$", x = n),grep(pattern = ".1$", x = n)]
burtd.0.1=burtd[grep(pattern = ".0$", x = n),grep(pattern = ".1$", x = n)]

png("burt.png",width = 1024, height = 768)
img1<-levelplot(data.matrix(burtd.0),aspect="iso",pretty=TRUE,xlim=c(1,171), ylim=c(1,171),xlab="",ylab="",main="Categorie : 0 - 0")
img2<-levelplot(data.matrix(burtd.0.1),aspect="iso",pretty=TRUE,xlim=c(1,171), ylim=c(1,171),xlab="",ylab="",main="Categorie : 0 - 1")
img4<-levelplot(data.matrix(burtd.1),aspect="iso",pretty=TRUE,xlim=c(1,171), ylim=c(1,171),xlab="",ylab="",main="Categorie : 1 - 1")
grid.arrange( img2, img4,img1,clip=T,ncol=2)
dev.off()
 
#Analyse des correlations des variables quantitatives
corMAt<-cor(train.quant)
png("correlations.png",width = 1024, height = 768)
levelplot(corMAt,aspect="iso",pretty=TRUE,xlim=c(1,ncol(train.quant)), ylim=c(1,ncol(train.quant)),xlab="",ylab="",main="Correlation")
dev.off()


## Partie Modelisation

#Transformation des variables quantitatives en variables qualitatives
train.quantQuali<-train.quant
for(i in 1:ncol(train.quantQuali)){
  train.quantQuali[,i]=cut(train.quantQuali[,i], breaks=quantile(train.quant[,i]),include.lowest=TRUE)
}
valid.quantQuali<-valid.quant
for(i in 1:ncol(valid.quantQuali)){
  valid.quantQuali[,i]=cut(valid.quantQuali[,i], breaks=quantile(train.quant[,i]),include.lowest=TRUE)
}
#Creation du jeux de données
train.FullAcm<-cbind(train.quantQuali,train.quali)
valid.FullAcm<-cbind(valid.quantQuali,valid.quali)
both.FullAcm<-rbind(train.FullAcm,valid.FullAcm)

#Analyse des Correspondances Multiples
train.acm<-dudi.acm(both.FullAcm,nf=ncol(train.FullAcm),scannf = FALSE)
train.coord<-train.acm$li[1:nrow(train.quali),]
valid.coord<-train.acm$li[(nrow(train.quali)+1):nrow(quali),]

#Visualisation des valeurs propres
png("eigenvalue.png")
barplot(train.acm$eig)
dev.off()
#Calcul de l'inertie
inertie<- as.data.frame(train.acm$eig/sum(train.acm$eig)*100)
#critere de kaiser
kaiserLimit<-100/ncol(train.FullAcm)
kaiserLimitCol<-ncol(train.FullAcm)
for(i in 1:nrow(inertie)){
  if(inertie[i,1]<kaiserLimit)
  {
    break
  }
  kaiserLimitCol<-i
}

colnames(valid.coord)<-colnames(train.coord)

#Modelisation
#ajout des labels
train.full<-(cbind(train.labels,train.coord[,1:kaiserLimitCol]))
valid.full<-(cbind(valid.labels,valid.coord[,1:kaiserLimitCol]))

#Renomate de la variable d'interet'
colnames(train.full)[1] <- "Y"

#Calcul de l'Arbre de Classification
dataTree <- rpart(Y ~ ., data=train.full)
plotcp(dataTree)
#Affichage de l'arbre
png("arbre.png")
prp(dataTree,extra=1)
dev.off()

#Balanced Error Rate (BER)
ber<-function(tru,pred) {
  test<-table(tru,pred)
  a<-test[1,1]
  b<-test[1,2]
  c<-test[2,1]
  d<-test[2,2]
  retVal<-0.5*(b/(a+b) + c/(c+d))
  return(retVal)
}
#Calcul du sigma
sigmaBer<-function(tru,pred) {
  test<-table(tru,pred)
  size<-test[1,2]+test[1,1]+test[2,1]+test[2,2]
  p1<-test[1,2]/size
  m1<-test[1,1]+test[1,2]
  p2<-test[2,1]/size
  m2<-test[2,1]+test[2,2]
  sigma = 0.5*sqrt(p1*(1-p1)/m1+p2*(1-p2)/m2) 
  return(sigma)
}

#Test avec les donnees d'apprentissage
table(train.full$Y, predict(dataTree, train.coord, type="class"))
arbre.train<-ber(train.full$Y, predict(dataTree, train.coord, type="class"))
#Test avec les donnees de validation
table(valid.full$Y, predict(dataTree, valid.coord, type="class"))
arbre.valid<-ber(valid.full$Y, predict(dataTree, valid.coord, type="class"))
arbre.sigma<-sigmaBer(valid.full$Y, predict(dataTree, valid.coord, type="class"))

#BER guess error
arbre.guess<-abs(arbre.valid-arbre.train)

#Test score
arbre.score<-arbre.valid + arbre.guess * (1- exp(-arbre.guess/arbre.sigma))

#Foret Aleatoire
train.rf <- randomForest(Y ~ ., data=train.full)

#nombre d'Arbre de la foret
train.rf$ntree

#Verification avec les donnees de validation
Prediction <- predict(train.rf, valid.coord)
table(valid.full$Y,Prediction)
foret.valid<-ber(valid.full$Y,Prediction)
foret.sigma<-sigmaBer(valid.full$Y, Prediction)

#Verification avec les donnees d'apprentissage
Prediction <- predict(train.rf, train.coord)
table(train.full$Y,Prediction)
foret.train<-ber(train.full$Y,Prediction)


#BER guess error
foret.guess<-abs(foret.valid-foret.train)

#Test score
foret.score<-foret.valid + foret.guess * (1- exp(-foret.guess/foret.sigma))

