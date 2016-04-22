## ----echo=FALSE----------------------------------------------------------
library(ISLR)

myVariablesNames<-c("Ages","Worclass","fnlwgt","Education","Education_num","Martial_Status","Occupation","Relationship","Race","Sex","CapitalGain","Capital_Loss","hours_per_Week","NativeCountry","cible")
adult<-read.table("~/ensiie_S3/MAD/adult.data",h=FALSE,col.names = myVariablesNames,sep = "," ,strip.white = TRUE)

#on supprime la variables Education_num et fnlgwt qui n'apportent aucune information
adult<- adult[,!colnames(adult)=="Education_num"]
adult<-adult[,!colnames(adult)=="fnlwgt"]
#les variables de notre jeu de données 

#dimension de notre jeu de données
#dim(adult)
#le jeux de données est donc complet
sum(is.na(adult)) 
is.na(adult) = adult=='?'
is.na(adult) = adult==' ?'
adult=na.omit(adult)
sum(is.na(adult$cible)) 

## ----echo=FALSE----------------------------------------------------------
names(adult)

## ----echo=FALSE----------------------------------------------------------
library(lattice)
histogram(~adult$Ages | adult$cible, data=adult,type="percent",col="grey",xlab="Ages")


## ----echo=FALSE----------------------------------------------------------
histogram(~adult$Capital_Loss | adult$cible, data=adult,type="percent",col="grey",xlab="Capital_Loss")

## ----echo =FALSE---------------------------------------------------------
histogram(~adult$CapitalGain | adult$cible, data=adult,type="percent",col="grey",xlab="CapitalGain")

## ----echo=FALSE----------------------------------------------------------

histogram(~adult$hours_per_Week | adult$cible, data=adult,type="percent",col="grey",xlab="hours_per_Week")

## ----echo=FALSE----------------------------------------------------------
by(adult[,c("Ages","CapitalGain","Capital_Loss","hours_per_Week")],list(Cible=adult$cible),summary)


## ---- echo=FALSE---------------------------------------------------------
#par(mfrow=c(1,1))
#La variable a étudier 
#plot(adult$cible,col= "orange",ylab="nombres d'individus",xlab="salaire en k$/an")
#title("\n Représentation des  variables Qualitative",outer = T)
#La variable Race
pie(table(adult$Race),main="Race")
#La variable Relationship
##plot(adult$Relationship,col="red",xlab="relation conjugale",ylab="nombre d'invidus")
pie(table(adult$Relationship),main="Relationship")
#La variable Nationalité
pie(table(adult$Worclass),main="Workclass")
#La variable Sex
plot(adult$Sex,col="yellow",xlab="Sex",ylab="nombre d'invidus")

#La variable Nationalité
plot(adult$NativeCountry,main="NativeCountry")

## ----echo=FALSE----------------------------------------------------------
by(adult[,c("Sex","Worclass","Education")],list(Cible=adult$cible),summary)

## ----echo=FALSE----------------------------------------------------------
by(adult[,c("Occupation","Martial_Status","NativeCountry")],list(Cible=adult$cible),summary)

## ----echo=FALSE----------------------------------------------------------
by(adult[,c("Relationship","Race")],list(Cible=adult$cible),summary)

## ----echo=FALSE----------------------------------------------------------
library(FactoMineR)
#tranformer la variable cible
myVariablesNames<-c("Ages","Worclass","fnlwgt","Education","Education_num","Martial_Status","Occupation","Relationship","Race","Sex","CapitalGain","Capital_Loss","hours_per_Week","NativeCountry","cible")
adult<-read.table("~/ensiie_S3/MAD/adult.data",h=FALSE,col.names = myVariablesNames,sep = "," ,strip.white = TRUE)
#on supprime la variables Education_num et fnlgwt qui n'apportent aucune information
adult<- adult[,!colnames(adult)=="Education_num"]
adult<-adult[,!colnames(adult)=="fnlwgt"]
#les variables de notre jeu de données 


#adult$cible<-as.character(adult$cible)
####ACP
res.pca <- PCA(adult[,1:13], scale.unit=TRUE,quali.sup=c(2,3,4,5,6,7,8,12,13))

res.pca$var
#dimdesc(res.pca)
summary(res.pca)


## ----echo=FALSE----------------------------------------------------------
res.pca$eig
barplot(res.pca$eig[,3],names=paste("Dim",1:nrow(res.pca$eig) ),col="grey")
legend("topleft","Information cumulée",cex=1)

## ----echo=FALSE----------------------------------------------------------
plot(res.pca,choix="ind",cex=0.7,shadow=TRUE,habillage=13,invisible = "quali")


## ----echo=FALSE----------------------------------------------------------
plot(res.pca,choix="var",cex=0.7,shadow=TRUE)
legend("topleft", "active quantitative variable", cex=0.5, pch=0.5, pt.cex = 0.1)

## ----echo=FALSE----------------------------------------------------------

set.seed(1)
train=sample(nrow(adult),1000)
data<-adult[train,]
res.mca = MCA(data[,1:13], quanti.sup=c(1,9:11), quali.sup=c(13))
#ACM que sur les variables qualitatives sans V15

 plot.MCA(res.mca, invisible=c("var","quali.sup"), cex=0.7)#Individu tracé
 plot.MCA(res.mca, invisible=c("ind","quali.sup"), cex=0.7)#Variable sans V15
 plot.MCA(res.mca, invisible=c("ind", "var"))#

 
#Comme avant on va regardé le positionnement des indivus suivant d'autres variables et juxtaposé

plot.MCA(res.mca, invisible=c("var","quali.sup"), habillage=2, cex=0.7)#Self-emp-inc+gris
plot.MCA(res.mca, invisible=c("var","quali.sup"), habillage=3, cex=0.7)#HS-grade

plot.MCA(res.mca, invisible=c("var","quali.sup"), habillage=6, cex=0.7)#Exe-manag+jaune
plot.MCA(res.mca, invisible=c("var","quali.sup"), habillage=5, cex=0.7)#Husband


plot.MCA(res.mca, invisible=c("var","quali.sup"), habillage=12, cex=0.7)#Americain


## ----echo=FALSE----------------------------------------------------------
plot.MCA(res.mca, invisible=c("var","quali.sup"), habillage=4, cex=0.7)#Married-civ-spouse

## ----echo=FALSE----------------------------------------------------------
plot.MCA(res.mca, invisible=c("var","quali.sup"), habillage=8, cex=0.7)#Male

## ----echo=FALSE----------------------------------------------------------
library(ISLR)
#tranformer la variable cible
myVariablesNames<-c("Ages","Worclass","fnlwgt","Education","Education_num","Martial_Status","Occupation","Relationship","Race","Sex","CapitalGain","Capital_Loss","hours_per_Week","NativeCountry","cible")
adult<-read.table("~/ensiie_S3/MAD/adult.data",h=FALSE,col.names = myVariablesNames,sep = "," ,strip.white = TRUE)
#on supprime la variables Education_num et fnlgwt qui n'apportent aucune information
adult<- adult[,!colnames(adult)=="Education_num"]
adult<-adult[,!colnames(adult)=="fnlwgt"]
adult[,"cible"]<-as.factor(adult[,"cible"])
adult[,"cible"]<-as.numeric(adult[,"cible"])
#oN Recode la variable cible
adult$cible[adult$cible ==1] <-0 # ok
adult$cible[adult$cible ==2] <-1 #KO<=50K
###################################
adult[,"Martial_Status"]<-as.numeric(adult[,"Martial_Status"])
adult[,"Education"]<-as.numeric(adult[,"Education"])
adult[,"Occupation"]<-as.numeric(adult[,"Occupation"])
adult[,"Sex"]<-as.numeric(adult[,"Sex"])
adult[,"Race"]<-as.numeric(adult[,"Race"])
adult[,"Relationship"]<-as.numeric(adult[,"Relationship"])
adult[,"NativeCountry"]<-as.numeric(adult[,"NativeCountry"])
adult[,"Worclass"]<-as.numeric(adult[,"Worclass"])



## ----echo=FALSE----------------------------------------------------------

set.seed(1)
train=sample(nrow(adult),1000)
data<-adult[train,]
cible<-data$cible
pr.out=prcomp(data,scale=TRUE)

Cols=function(vec){
  cols=rainbow(length(unique(vec)))
return(cols[as.numeric(as.factor(vec))])
}

#names(pr.out)
#summary(pr.out$x)
par(mfrow=c(1,2))
plot(pr.out$x[,1:2],col=Cols(cible),pch=19,xlab="Dim1",ylab="DIM2")
plot(pr.out$x[,c(1,3)],col=Cols(cible),pch=19,xlab="DIM1",ylab="DIM3")


## ----echo=FALSE----------------------------------------------------------
plot(pr.out)
summary(pr.out)$importance[2,]
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
plot(cumsum(pve),type="o",ylab="Variance Cumulée",col="brown3")

## ----echo=FALSE----------------------------------------------------------
sd.data=scale(data)

data.dist=dist(sd.data)
#plot(hclust(data.dist),labels=cible,main="Liens entre variables",sub="",ylab="")
###Hierachique clustering
hc.out=hclust(dist(sd.data))
hc.clusters=cutree(hc.out,3)

#K-MEANS CULSTERING
set.seed(2)
km.out=kmeans(data,3,nstart=20)

km.cluster=km.out$cluster
table(hc.clusters)
table(km.cluster)
table(km.cluster,hc.clusters)

## ----echo=FALSE----------------------------------------------------------
hc.out=hclust(dist(pr.out$x[,1:7]))
hc.clusters=cutree(hc.out,3)

set.seed(2)
km.out=kmeans(pr.out$x[,1:7],3,nstart=20)
km.cluster=km.out$cluster
table(hc.clusters)
table(km.cluster)
table(km.cluster,hc.clusters)


## ----echo=FALSE----------------------------------------------------------
library(ISLR)
#tranformer la variable cible
myVariablesNames<-c("Ages","Worclass","fnlwgt","Education","Education_num","Martial_Status","Occupation","Relationship","Race","Sex","CapitalGain","Capital_Loss","hours_per_Week","NativeCountry","cible")
adult<-read.table("~/ensiie_S3/MAD/adult.data",h=FALSE,col.names = myVariablesNames,sep = "," ,strip.white = TRUE)
#on supprime la variables Education_num et fnlgwt qui n'apportent aucune information
adult<- adult[,!colnames(adult)=="Education_num"]
adult<-adult[,!colnames(adult)=="fnlwgt"]
adult[,"cible"]<-as.factor(adult[,"cible"])
adult[,"cible"]<-as.numeric(adult[,"cible"])
#oN Recode la variable cible
adult$cible[adult$cible ==1] <-0 # ok
adult$cible[adult$cible ==2] <-1 #KO<=50K
###################################
adult[,"Martial_Status"]<-as.numeric(adult[,"Martial_Status"])
adult[,"Education"]<-as.numeric(adult[,"Education"])
adult[,"Occupation"]<-as.numeric(adult[,"Occupation"])
adult[,"Sex"]<-as.numeric(adult[,"Sex"])
adult[,"Race"]<-as.numeric(adult[,"Race"])
adult[,"Relationship"]<-as.numeric(adult[,"Relationship"])
adult[,"NativeCountry"]<-as.numeric(adult[,"NativeCountry"])
adult[,"Worclass"]<-as.numeric(adult[,"Worclass"])


set.seed(1)
train<-sample(nrow(adult),1000) #on prélève 1000 observations de notre jeu data
###########Creation de mes dataset
adult.test=adult[-train,]
adult.train=adult[train,]
cible.test=adult$cible[-train]


## ----echo=FALSE----------------------------------------------------------
library(ISLR)
library(leaps)
regfit.full=regsubsets(adult.train$cible~.,adult.train)
stats.regfit=summary(regfit.full)


## ----echo=FALSE----------------------------------------------------------
stats.regfit$rsq

## ----echo=FALSE----------------------------------------------------------
par(mfrow=rep(2,2))

#rss
plot(stats.regfit$rss,xlab="Number of Variables", ylab="RSS",type="b",col="grey",cex=0.8,main="RSS")

# ADJR2
plot(stats.regfit$adjr2,xlab="Number of Variables",ylab= "Adj" ,main="ADJR2",type="b",cex=0.8,col="grey")
max=which.max(stats.regfit$adjr2) #8
points(max,stats.regfit$adjr2[max],col="red",cex=1.5,pch=20)

#Cp mallow
plot(stats.regfit$cp,xlab="Number of Variables",ylab= "Cp's",type="b",cex=0.8,col="grey",main="Cp's Mallows")
min=which.min(stats.regfit$cp) #7
points(min,stats.regfit$cp[min],col="red",cex=1.5,pch=20)

#BIC
plot(stats.regfit$bic,xlab="Number of Variables",ylab= "BIC",type="b",cex=0.8,col="grey",main="BIC")
min=which.min(stats.regfit$bic) #5
points(min,stats.regfit$bic[min],col="red",cex=1.5,pch=20)

## ----echo=FALSE----------------------------------------------------------
summary(regfit.full)

## ----echo=FALSE----------------------------------------------------------
coef(regfit.full,5)

## ----echo=FALSE----------------------------------------------------------
library(glmnet)
set.seed(1)

grid=10^seq(10,-2,length=100) #On choist un vaste intervalle de choix de 10^-2<lambda<10¹0
x=model.matrix(adult$cible~.,adult)[,-1]
y=adult$cible

trainRidge=sample(1:nrow(x),nrow(x)/2)

test=-train
y.test=y[test]

ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)

## ----echo=FALSE----------------------------------------------------------
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])

## ----echo=FALSE----------------------------------------------------------
mean((ridge.pred-y.test)^2) #0.1518015

## ----echo=FALSE----------------------------------------------------------
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:12,]


## ----echo=FALSE----------------------------------------------------------
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)

bestlam=cv.out$lambda.min
bestlam
plot(cv.out)

lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])

## ------------------------------------------------------------------------
mean((lasso.pred-y.test)^2)

## ----echo=FALSE----------------------------------------------------------
out=glmnet(x,y,alpha=1)
predict(out,type="coefficients",s=bestlam)[1:12,]


## ----echo=FALSE----------------------------------------------------------
library(ISLR)
#tranformer la variable cible
myVariablesNames<-c("Ages","Worclass","fnlwgt","Education","Education_num","Martial_Status","Occupation","Relationship","Race","Sex","CapitalGain","Capital_Loss","hours_per_Week","NativeCountry","cible")
adult<-read.table("~/ensiie_S3/MAD/adult.data",h=FALSE,col.names = myVariablesNames,sep = "," ,strip.white = TRUE)
#on supprime la variables Education_num et fnlgwt qui n'apportent aucune information
adult<- adult[,!colnames(adult)=="Education_num"]
adult<-adult[,!colnames(adult)=="fnlwgt"]
adult[,"cible"]<-as.factor(adult[,"cible"])
adult[,"cible"]<-as.numeric(adult[,"cible"])
#oN Recode la variable cible
adult$cible[adult$cible ==1] <-0 # ok
adult$cible[adult$cible ==2] <-1 #KO<=50K
###################################
adult[,"Martial_Status"]<-as.numeric(adult[,"Martial_Status"])
adult[,"Education"]<-as.numeric(adult[,"Education"])
adult[,"Occupation"]<-as.numeric(adult[,"Occupation"])
adult[,"Sex"]<-as.numeric(adult[,"Sex"])
adult[,"Race"]<-as.numeric(adult[,"Race"])
adult[,"Relationship"]<-as.numeric(adult[,"Relationship"])
adult[,"NativeCountry"]<-as.numeric(adult[,"NativeCountry"])
adult[,"Worclass"]<-as.numeric(adult[,"Worclass"])
####################################################problem pour tree
# adult[,"Martial_Status"]<-as.factor(adult[,"Martial_Status"])
# adult[,"Education"]<-as.factor(adult[,"Education"])
# adult[,"Occupation"]<-as.factor(adult[,"Occupation"])
# adult[,"Sex"]<-as.factor(adult[,"Sex"])
# adult[,"Race"]<-as.factor(adult[,"Race"])
# adult[,"Relationship"]<-as.factor(adult[,"Relationship"])
# adult[,"NativeCountry"]<-as.factor(adult[,"NativeCountry"])
# adult[,"Worclass"]<-as.factor(adult[,"Worclass"])

## ----echo=FALSE----------------------------------------------------------
library(tree)
set.seed(1)
train<-sample(nrow(adult),1000) #on prélève 1000 observations de notre jeu data
###########Creation de mes dataset
adult.test=adult[-train,]
adult.train=adult[train,]
cible.test=adult$cible[-train]
#################################

tree.adult<-tree(as.factor(adult.train$cible)~.,adult.train)
tree.pred=predict(tree.adult,adult.test,type="class")
table(tree.pred,cible.test) #23822,1925
(23822 +1925)/1000

## ----echo=FALSE----------------------------------------------------------
summary(tree.adult)


## ----echo=FALSE----------------------------------------------------------
par(mfrow=c(1,1))
plot(tree.adult)
text(tree.adult,pretty=0)

## ----echo=FALSE----------------------------------------------------------
##Mise en place du random forest sur 25 arbres 
library(randomForest)

rf.adult=randomForest(adult$cible~.,data=adult ,subset=train,mtry=3,importance=TRUE,ntree=25) 
pred.rf=predict(rf.adult,newdata=adult[-train,])
adult.test=adult[-train,"cible"] #je prélève la variable cible de mon jeu de test

#fix(cible.test)
#fix(adult.test)
mean((pred.rf-cible.test)^2) 


## ----echo=FALSE----------------------------------------------------------
importance(rf.adult)
varImpPlot(rf.adult,col="blue",cex=1,main="Choose best_features")

## ----echo=FALSE----------------------------------------------------------
library(gbm)
#adult.train=adult[train,]
boost.adult=gbm(adult.train$cible~.,adult.train,distribution="bernoulli",n.trees=5000,interaction.depth=4)
summary(boost.adult)

## ----echo=FALSE----------------------------------------------------------
pred.boost=predict(boost.adult,newdata=adult[-train,],n.trees=5000)
mean((pred.boost-adult.test)^2)

