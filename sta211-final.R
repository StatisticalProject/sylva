#librairie utilise pendant l analyse
library(ade4)
library(lattice)
library(gridExtra)
library(FactoMineR)
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
train.quali<-train.data[,c(1,2,4:6,8,9,11:17,19:40,44:49,51:54,56:74,77:80,82:87,90:99,101:103,106:109,111,112,114:116,118:124,128:138,140:142,144:146,148,149,151:154,156:165,167:170,172:178,180,181,184:186,189,190,192:200,202:204,206,207,209,210,212:216)]
train.quant<-train.data[,c(3,7,10,18,41,42,43,50,55,75,76,81,88,89,100,104,105,110,113,117,125,126,127,139,143,147,150,155,166,171,179,182,183,187,188,191,201,205,208,211)]
valid.quali <- valid.data[,c(1,2,4:6,8,9,11:17,19:40,44:49,51:54,56:74,77:80,82:87,90:99,101:103,106:109,111,112,114:116,118:124,128:138,140:142,144:146,148,149,151:154,156:165,167:170,172:178,180,181,184:186,189,190,192:200,202:204,206,207,209,210,212:216)]
valid.quant <- valid.data[,c(3,7,10,18,41,42,43,50,55,75,76,81,88,89,100,104,105,110,113,117,125,126,127,139,143,147,150,155,166,171,179,182,183,187,188,191,201,205,208,211)]

ncol(train.quali)
ncol(train.quant)

#transforme les variables qualitatives en facteur
quali <- as.data.frame(lapply(rbind(train.quali,valid.quali), as.factor)) 
valid.quali <- quali[nrow(train.quali)+1:nrow(valid.quali),]
train.quali <- quali[1:nrow(train.quali),]

#Tableau de burt des variables quantitatives
burtd<-acm.burt(train.quali,train.quali)
n <- names(burtd)
burtd.0=burtd[grep(pattern = ".0$", x = n),grep(pattern = ".0$", x = n)]
burtd.1=burtd[grep(pattern = ".1$", x = n),grep(pattern = ".1$", x = n)]
burtd.0.1=burtd[grep(pattern = ".0$", x = n),grep(pattern = ".1$", x = n)]

png("burt.png",width = 1024, height = 768)
img1<-levelplot(data.matrix(burtd.0),aspect="iso",pretty=TRUE,xlim=c(1,176), ylim=c(1,176),xlab="",ylab="",main="Categorie : 0 - 0")
img2<-levelplot(data.matrix(burtd.0.1),aspect="iso",pretty=TRUE,xlim=c(1,176), ylim=c(1,171),xlab="",ylab="",main="Categorie : 0 - 1")
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

#Analyse des Correspondances Multiples
train.acm<-dudi.acm(train.FullAcm,nf=ncol(train.FullAcm),scannf = FALSE)
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

#Projection des données de test
valid.suprow<-acm.disjonctif(valid.FullAcm)
train.suprow<-acm.disjonctif(train.FullAcm)
colw <- train.acm$cw*ncol(valid.FullAcm)
valid.suprow <- data.frame(t(t(valid.suprow)/colw) - 1)
valid.proj<-suprow(train.acm, valid.suprow)
