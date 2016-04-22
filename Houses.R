
############Skomy
## ----echo=FALSE----------------------------------------------------------
load("~/ensiie_S3/MLG/Projet/hedonic.rda")
par(mfrow=c(1,2),pty="m")
hist(Hedonic$mv,main="Histogramme de mv",xlab="",freq=FALSE,col="grey")
lines(density(Hedonic$mv),col="blue")
par(mfrow=c(1,1),pty="m")
boxplot(Hedonic$mv,col="orange")
title("Boxplot de la variable cible \n mv")

## ----echo=FALSE----------------------------------------------------------
library(Hmisc)
par(mfrow=c(2,2),pty="m")
hist.data.frame(Hedonic[,1:15])

## ------------------------------------------------------------------------
Hedonic[,"chas"]<-as.numeric(Hedonic[,"chas"])
library(corrplot)
M<-cor(Hedonic)
corrplot(M,method="circle")

## ------------------------------------------------------------------------
library(ISLR)
#les variables de notre jeu de données 
names(Hedonic)
#dimension de notre jeu de données
dim(Hedonic)

## ------------------------------------------------------------------------
#Transformation de la variable chas en variable numérique.
Hedonic[,"chas"]<-as.numeric(Hedonic[,"chas"])
Hedonic$chas[Hedonic$chas == 1] <- 0 #no
Hedonic$chas[Hedonic$chas == 2] <- 1 #yes
Hedonic$chas<-factor(Hedonic$chas)
#Désormais on vérifie que notre jeu de données est bien complet.
sum(is.na(Hedonic$mv)) 

## ------------------------------------------------------------------------
library(leaps)
regfit.full=regsubsets(Hedonic$mv~.,data=Hedonic,nvmax=14)
reg.summary=summary(regfit.full)

## ------------------------------------------------------------------------
par(mfrow=c(2,2),pty="m")
#Etude du RSS
plot(reg.summary$rss,xlab="Number of variables",ylab="RSS",type="b",col="blue")
bss_rss=which.min(reg.summary$rss)
points(bss_rss,reg.summary$rss[bss_rss],col="orange",cex=2,pch=20)

#Etude du adjusted R² 
plot(reg.summary$adjr2,xlab="Number of variables",ylab="adjr2",type="b",col="blue")
bss_adjr2=which.max(reg.summary$adjr2)
points(bss_adjr2,reg.summary$adjr2[bss_adjr2],col="orange",cex=2,pch=20)

#Etude du CP 
plot(reg.summary$cp,xlab="Number of variables",ylab="Mallows'Cp",type="b",col="blue")
bss_cp=which.min(reg.summary$cp)
points(bss_cp,reg.summary$cp[bss_cp],col="orange",cex=2,pch=20)

#Etude du Bic
plot(reg.summary$bic,xlab="Number of variables",ylab="BIC",type="b",col="blue")
bss_bic=which.min(reg.summary$bic)
points(bss_bic,reg.summary$bic[bss_bic],col="orange",cex=2,pch=20)


## ------------------------------------------------------------------------
plot(regfit.full,scale="bic")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="r2")


## ------------------------------------------------------------------------
coef(regfit.full,10)

## ------------------------------------------------------------------------
regfit.fwd<-regsubsets(Hedonic$mv~.,data=Hedonic,nvmax=14,method="forward")
plot(regfit.fwd)


## ------------------------------------------------------------------------
regfit.bwd<-regsubsets(Hedonic$mv~.,data=Hedonic,nvmax=14,method="backward")
plot(regfit.bwd)

## ------------------------------------------------------------------------
#Comparaison
coef(regfit.fwd,10)
coef(regfit.full, 10)
coef(regfit.bwd, 10)

## ------------------------------------------------------------------------
set.seed(1)
#Train et test
train=sample(c(TRUE,FALSE),nrow(Hedonic),rep=TRUE)
test=(!train)
#Jeux de données d'entrainement et de test
Hedonic.train<-Hedonic[train,]
Hedonic.test<-Hedonic[test,]
dim(Hedonic.test)
dim(Hedonic.train)

## ------------------------------------------------------------------------
regfit.best.cross=regsubsets(Hedonic.train$mv ~.,data=Hedonic.train,nvmax=14)

## ------------------------------------------------------------------------

test.mat=model.matrix(Hedonic.test$mv~. , data = Hedonic[test,])
Hedonic.train<-subset(Hedonic,train==TRUE,-train)
#vecteurs de stockage du MSE
val.errors=rep(NA,14)

for (i in 1:14){
  coefi=coef(regfit.best.cross,id=i)
  pred=test.mat[,names(coefi)] %*%coefi
  val.errors[i] =mean((Hedonic$mv[test]-pred)^2)
}

#Vecteur stockant le MSE
val.errors

## ------------------------------------------------------------------------
bss.coeff.cross=which.min(val.errors)
#On a donc un modèle à 10 variables
bss.coeff.cross
#Comparaison
coef(regfit.best.cross,10)

coef(regfit.full,10)

## ------------------------------------------------------------------------
k=10  #nombre de test
set.seed(1)
folds=sample(1:k,nrow(Hedonic),replace =TRUE)

#Matrice dans laquelle on stockera les erreurs.
cross_valid.errors=matrix(NA,k,14,dimnames=list(NULL,paste(1:14)))  


#Création d'une méthode predict
predict.regsubsets=function(object,newdata,id,...) {
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

#Creation de la boucle pour la cross-validation
for(j in 1:k){
  Hedonic_folds<-Hedonic[folds != j,]
  best.fit.cross=regsubsets(Hedonic_folds$mv~.,data=Hedonic_folds,nvmax=14)
  for (i in 1:14) {
    Hedonic_folds<-Hedonic[folds == j,]
    pred =predict (best.fit.cross,Hedonic[folds == j,] , id=i)
    cross_valid.errors[j,i]=mean( (Hedonic$mv [folds==j] - pred) ^2)
  }

}
#Compositon de notre vector contenant les MSE
mean.cv.errors=apply(cross_valid.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
#La cross validation choisie une nouvelle fois 10 variables 
plot(mean.cv.errors,type='b') 
mean.error.bss=which.min(mean.cv.errors)
mean.error.bss
points(mean.error.bss,mean.cv.errors[mean.error.bss],col="red",cex=2,pch=20)

## ------------------------------------------------------------------------
reg.best.cross=regsubsets(Hedonic$mv~.,data=Hedonic,nvmax=14)
coef(reg.best.cross,10)
coef(regfit.full,10)

## ----results=FALSE-------------------------------------------------------
#variable explicative
x=model.matrix(Hedonic$mv~.,Hedonic) [,-1]
#variable expliqué
y=Hedonic$mv
#Ridge Regression alpha=0
library(glmnet)
 #RIDGE
grid=10^seq(10,-2,length=100)
ridge.mod<-glmnet(x,y,alpha=0,lambda=grid)
# Matrice avec 15 lignes (pour chaque predictor + intercept) et 100 colonnes (pour chaque valeur de lambda)
dim(coef(ridge.mod))

#ridge.mod$lambda[50]
#coef(ridge.mod)[,50]
# sqrt(sum(coef(ridge.mod)[-1,50]^2))
 #ridge.mod$lambda[60]
 #coef(ridge.mod)[,60]
#predict(ridge.mod,s=50,type="coefficients")[1:15,]
 

## ------------------------------------------------------------------------
##SET
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=-train
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,new=x[test,]) #(prenons lambda =4)
mean((ridge.pred-y.test)^2) #MSE =0.169896  Mean Squarred Error

## ------------------------------------------------------------------------
#Utilisation de la cross validation pour déterminer le best lambda
library(glmnet)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
#On a trouvé notre bestlam
bestlam=cv.out$lambda.min
bestlam
#Calcul du MSE pour ce bestlamb
ridge.pred=predict(ridge.mod,s=bestlam,new=x[test,])
mean((ridge.pred-y.test)^2) #MSE 

out=glmnet(x,y,alpha=0)
ridge.coeff=predict(out ,type="coefficients",s=bestlam)[1:15,]
ridge.coeff

lbs_fun <- function(lasso.mod, ...) {
  L <- length(lasso.mod$lambda)
  x <- log(lasso.mod$lambda[L])
  y <- lasso.mod$beta[ ,L]
  labs <- names(y)
  text(x, y, labels=labs, ...) }

plot(ridge.mod,col=1:dim(coef(ridge.mod))[1],xvar="norm")
lbs_fun(ridge.mod)
out=glmnet(x,y)

## ------------------------------------------------------------------------
#Lasso pour lambda = grid (défini plus haut)
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)


L <- length(lasso.mod$lambda)
lasso.mod$beta[ ,L]

plot(lasso.mod,col=1:dim(coef(lasso.mod))[1])
lbs_fun(lasso.mod)
set.seed(1)

#Recherche de notre Bestlam
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam 
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2) #MSE 0.03735894
out=glmnet(x,y,alpha=1,lambda=grid)
plot(out)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:15,]

## ----echo=FALSE----------------------------------------------------------
#Comparaison des valeurs des coefficients 
ridge.coeff[ridge.coeff != 0]
lasso.coef[lasso.coef !=0]

## ----echo=FALSE----------------------------------------------------------
#Comparaison avec nos précédents modèles
lasso.coef[lasso.coef !=0] ##MSE 0.03735894
coef(reg.best.cross,10) #MSE = 0.03484195
coef(regfit.full,10)

## ----echo=FALSE----------------------------------------------------------
library(pls)
set.seed(2)
pcr.fit=pcr(Hedonic$mv~.,data=Hedonic,scale=TRUE,validation="CV")
summary(pcr.fit) #RMS²=MSE
validationplot(pcr.fit,val.type = "MSEP")
coef(pcr.fit)

## ----echo=FALSE----------------------------------------------------------
summary(pcr.fit)

## ------------------------------------------------------------------------
pcr.fit.cross=pcr(Hedonic$mv~.,data=Hedonic,subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit.cross,val.type="MSEP")
summary(pcr.fit.cross)

## ------------------------------------------------------------------------
pcr.pred=predict(pcr.fit,x[test,],ncomp=12)
mean((pcr.pred-y.test)^2) #MSE =0.03592219

## ------------------------------------------------------------------------
pcr.fit=pcr(y~x,scale=TRUE,ncomp=12)
summary(pcr.fit)

## ------------------------------------------------------------------------
library(splines)
y<-Hedonic$mv
x<-Hedonic$rm
fit.poly=lm(y~bs(x,degree=3),data=Hedonic)
#summary(fit.poly)
fit.rm<-coef(summary(fit.poly))
fit.rm
anova(fit.poly)
xlims=range(x)
x.grid=seq(from=xlims[1],to=xlims[2])
pred.poly=predict(fit.poly,newdata=list(x=x.grid),se=T)
se.bands=cbind(pred.poly$fit+2*pred.poly$se.fit, pred.poly$fit-2*pred.poly$se.fit)

#Traçons les données en y ajoutant notre modèle polynomial de degré 4
par(mfrow=c(2,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(fit.poly,which=1,pch=16)
plot(fit.poly,which=2,pch=16)
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
lines(x.grid,pred.poly$fit,lwd=1,col="orange")
matlines(x.grid,se.bands,lwd=1,col="blue",lty=2)

## ---- echo=FALSE---------------------------------------------------------
y<-Hedonic$mv
x<-Hedonic$crim
fit.poly=lm(y~bs(x,degree=3),data=Hedonic)
#summary(fit.poly)
fit.crim<-coef(summary(fit.poly))
xlims=range(x)
x.grid=seq(from=xlims[1],to=xlims[2])
pred.poly=predict(fit.poly,newdata=list(x=x.grid),se=T)
se.bands=cbind(pred.poly$fit+2*pred.poly$se.fit, pred.poly$fit-2*pred.poly$se.fit)
#Traçons les données en y ajoutant notre modèle polynomial de degré 4
par(mfrow=c(2,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(fit.poly,which=1,pch=16)
plot(fit.poly,which=2,pch=16)
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
lines(x.grid,pred.poly$fit,lwd=1,col="orange")
title("X: CRIM",outer=T)
matlines(x.grid,se.bands,lwd=1,col="blue",lty=2)

## ----echo=FALSE,fig.align='center'---------------------------------------
y<-Hedonic$mv
x<-Hedonic$age
fit.poly=lm(y~bs(x,degree=3),data=Hedonic)
#summary(fit.poly)
fit.age<-coef(summary(fit.poly))
xlims=range(x)
x.grid=seq(from=xlims[1],to=xlims[2])
pred.poly=predict(fit.poly,newdata=list(x=x.grid),se=T)
se.bands=cbind(pred.poly$fit+2*pred.poly$se.fit, pred.poly$fit-2*pred.poly$se.fit)
#Traçons les données en y ajoutant notre modèle polynomial de degré 4
par(mfrow=c(2,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(fit.poly,which=1,pch=16)
plot(fit.poly,which=2,pch=16)
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
lines(x.grid,pred.poly$fit,lwd=1,col="orange")
title(" X= Age \n",outer=T)
matlines(x.grid,se.bands,lwd=1,col="blue",lty=2)

## ----echo=FALSE----------------------------------------------------------
y<-Hedonic$mv
x<-Hedonic$blacks
fit.poly=lm(y~bs(x,degree=3),data=Hedonic)
#summary(fit.poly)
fit.blacks<-coef(summary(fit.poly))
xlims=range(x)
x.grid=seq(from=xlims[1],to=xlims[2])
pred.poly=predict(fit.poly,newdata=list(x=x.grid),se=T)
se.bands=cbind(pred.poly$fit+2*pred.poly$se.fit, pred.poly$fit-2*pred.poly$se.fit)
#Traçons les données en y ajoutant notre modèle polynomial de degré 4
par(mfrow=c(2,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(fit.poly,which=1,pch=16)
plot(fit.poly,which=2,pch=16)
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
lines(x.grid,pred.poly$fit,lwd=1,col="orange")
title("X= Black \n",outer=T)
matlines(x.grid,se.bands,lwd=1,col="blue",lty=2)

## ----echo=FALSE----------------------------------------------------------
y<-Hedonic$mv
x<-Hedonic$zn
fit.poly=lm(y~bs(x,degree=3),data=Hedonic)
#summary(fit.poly)
fit.zn<-coef(summary(fit.poly))
xlims=range(x)
x.grid=seq(from=xlims[1],to=xlims[2])
pred.poly=predict(fit.poly,newdata=list(x=x.grid),se=T)
se.bands=cbind(pred.poly$fit+2*pred.poly$se.fit, pred.poly$fit-2*pred.poly$se.fit)
#Traçons les données en y ajoutant notre modèle polynomial de degré 4
par(mfrow=c(2,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(fit.poly,which=1,pch=16)
plot(fit.poly,which=2,pch=16)
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
lines(x.grid,pred.poly$fit,lwd=1,col="orange")
title("X= Zn \n",outer=T)
matlines(x.grid,se.bands,lwd=1,col="blue",lty=2)

## ----echo=FALSE----------------------------------------------------------
y<-Hedonic$mv
x<-Hedonic$lstat
fit.poly=lm(y~bs(x,degree=3),data=Hedonic)
#summary(fit.poly)
fit.lstat<-coef(summary(fit.poly))
xlims=range(x)
x.grid=seq(from=xlims[1],to=xlims[2])
pred.poly=predict(fit.poly,newdata=list(x=x.grid),se=T)
se.bands=cbind(pred.poly$fit+2*pred.poly$se.fit, pred.poly$fit-2*pred.poly$se.fit)
#Traçons les données en y ajoutant notre modèle polynomial de degré 4
par(mfrow=c(2,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(fit.poly,which=1,pch=16)
plot(fit.poly,which=2,pch=16)
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
lines(x.grid,pred.poly$fit,lwd=1,col="orange")
title("X= Lstat\n",outer=T)
matlines(x.grid,se.bands,lwd=1,col="blue",lty=2)

## ----echo=FALSE----------------------------------------------------------
y<-Hedonic$mv
x<-Hedonic$townid
fit.poly=lm(y~bs(x,degree=3),data=Hedonic)
#summary(fit.poly)
fit.townid<-coef(summary(fit.poly))
xlims=range(x)
x.grid=seq(from=xlims[1],to=xlims[2])
pred.poly=predict(fit.poly,newdata=list(x=x.grid),se=T)
se.bands=cbind(pred.poly$fit+2*pred.poly$se.fit, pred.poly$fit-2*pred.poly$se.fit)
#Traçons les données en y ajoutant notre modèle polynomial de degré 4
par(mfrow=c(2,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(fit.poly,which=1,pch=16)
plot(fit.poly,which=2,pch=16)
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
lines(x.grid,pred.poly$fit,lwd=1,col="orange")
title("X= Townid\n",outer=T)
matlines(x.grid,se.bands,lwd=1,col="blue",lty=2)

## ----echo=FALSE----------------------------------------------------------
y<-Hedonic$mv
x<-Hedonic$dis
fit.poly=lm(y~bs(x,degree=3),data=Hedonic)
#summary(fit.poly)
fit.dis<-coef(summary(fit.poly))
xlims=range(x)
x.grid=seq(from=xlims[1],to=xlims[2])
pred.poly=predict(fit.poly,newdata=list(x=x.grid),se=T)
se.bands=cbind(pred.poly$fit+2*pred.poly$se.fit, pred.poly$fit-2*pred.poly$se.fit)
#Traçons les données en y ajoutant notre modèle polynomial de degré 4
par(mfrow=c(2,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(fit.poly,which=1,pch=16)
plot(fit.poly,which=2,pch=16)
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
lines(x.grid,pred.poly$fit,lwd=1,col="orange")
title("X=DIS\n",outer=T)
matlines(x.grid,se.bands,lwd=1,col="blue",lty=2)

## ----echo=FALSE----------------------------------------------------------
y<-Hedonic$mv
x<-Hedonic$tax
fit.poly=lm(y~bs(x,degree=3),data=Hedonic)
#summary(fit.poly)
fit.tax<-coef(summary(fit.poly))
xlims=range(x)
x.grid=seq(from=xlims[1],to=xlims[2])
pred.poly=predict(fit.poly,newdata=list(x=x.grid),se=T)
se.bands=cbind(pred.poly$fit+2*pred.poly$se.fit, pred.poly$fit-2*pred.poly$se.fit)
#Traçons les données en y ajoutant notre modèle polynomial de degré 4
par(mfrow=c(2,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(fit.poly,which=1,pch=16)
plot(fit.poly,which=2,pch=16)
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
lines(x.grid,pred.poly$fit,lwd=1,col="orange")
title("X= TAX\n",outer=T)
matlines(x.grid,se.bands,lwd=1,col="blue",lty=2)

## ----echo=FALSE----------------------------------------------------------
y<-Hedonic$mv
x<-Hedonic$ptratio
fit.poly=lm(y~bs(x,degree=3),data=Hedonic)
#summary(fit.poly)
fit.ptratio<-coef(summary(fit.poly))
xlims=range(x)
x.grid=seq(from=xlims[1],to=xlims[2])
pred.poly=predict(fit.poly,newdata=list(x=x.grid),se=T)
se.bands=cbind(pred.poly$fit+2*pred.poly$se.fit, pred.poly$fit-2*pred.poly$se.fit)
#Traçons les données en y ajoutant notre modèle polynomial de degré 4
par(mfrow=c(2,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(fit.poly,which=1,pch=16)
plot(fit.poly,which=2,pch=16)
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
lines(x.grid,pred.poly$fit,lwd=1,col="orange")
title("X= Ptratio",outer=T)
matlines(x.grid,se.bands,lwd=1,col="blue",lty=2)

## ----echo=FALSE----------------------------------------------------------
y<-Hedonic$mv
x<-Hedonic$nox
fit.poly=lm(y~bs(x,degree=3),data=Hedonic)
#summary(fit.poly)
fit.nox<-coef(summary(fit.poly))
xlims=range(x)
x.grid=seq(from=xlims[1],to=xlims[2])
pred.poly=predict(fit.poly,newdata=list(x=x.grid),se=T)
se.bands=cbind(pred.poly$fit+2*pred.poly$se.fit, pred.poly$fit-2*pred.poly$se.fit)
#Traçons les données en y ajoutant notre modèle polynomial de degré 4
par(mfrow=c(2,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(fit.poly,which=1,pch=16)
plot(fit.poly,which=2,pch=16)
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
lines(x.grid,pred.poly$fit,lwd=1,col="orange")
title("X= Nox\n",outer=T)
matlines(x.grid,se.bands,lwd=1,col="blue",lty=2)

## ----echo=FALSE----------------------------------------------------------
y<-Hedonic$mv
x<-Hedonic$rad
fit.poly=lm(y~bs(x,degree=3),data=Hedonic)

#summary(fit.poly)
fit.rad<-coef(summary(fit.poly))
xlims=range(x)
x.grid=seq(from=xlims[1],to=xlims[2])
pred.poly=predict(fit.poly,newdata=list(x=x.grid),se=T)
se.bands=cbind(pred.poly$fit+2*pred.poly$se.fit, pred.poly$fit-2*pred.poly$se.fit)
#Traçons les données en y ajoutant notre modèle polynomial de degré 4
par(mfrow=c(2,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(fit.poly,which=1,pch=16)
plot(fit.poly,which=2,pch=16)
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
plot(x,y,xlim=xlims, cex=0.8,col="darkgrey")
lines(x.grid,pred.poly$fit,lwd=1,col="orange")
title("X= Rad\n",outer=T)
matlines(x.grid,se.bands,lwd=1,col="blue",lty=2)

## ----echo=FALSE,results="hide"-------------------------------------------
model.produit<-lm (mv~(crim*(zn+indus+chas+nox+age+dis+rad+tax+ptratio+blacks+lstat+rm+townid)+zn*(chas+nox+age+dis+rad+tax+ptratio+blacks+lstat+rm+townid)+chas*(nox+age+dis+rad+tax+ptratio+blacks+lstat+rm+townid)+nox*(age+dis+rad+tax+ptratio+blacks+lstat+rm+townid)+age*(dis+rad+tax+ptratio+blacks+lstat+rm+townid)+dis*(rad+tax+ptratio+blacks+lstat+rm+townid)+rad*(tax+ptratio+blacks+lstat+rm+townid)+tax*(ptratio+blacks+lstat+rm+townid)+ptratio*(blacks+lstat+rm+townid)+blacks*(lstat+rm+townid)+lstat*(rm+townid)+
rm*townid), Hedonic)

## ----echo=FALSE, results="hide"------------------------------------------
 model.produit.AIC <- step(model.produit,k=2)
 model.produit.BIC <- step(model.produit,k=log(nrow(Hedonic)))
 
 par(mfrow=c(2,3))
 plot(model.produit.AIC,which=1:3)
 plot(model.produit.BIC,which=1:3)

## ----echo=FALSE----------------------------------------------------------
#LSTAT
library(splines)
set.seed(1)
train=sample(1:nrow(Hedonic),nrow(Hedonic)/2)
Hedonic.train<-Hedonic[train,]
lstat=Hedonic.train$lstat
lstatLims=range(lstat)
lstat.grid=seq(from=lstatLims[1],to=lstatLims[2])


Hedonic.test<-Hedonic[-train,"mv"]
fit2<-lm(Hedonic$mv~ns(lstat,df=4),data=Hedonic,subset=train)

ypred2<-predict(fit2,newdata=list(lstat=lstat.grid),se=T)
plot(lstat,  Hedonic.train$mv ,col="gray")
title("Spline Regression on Lstat")
lines(lstat.grid,ypred2$fit,col="blue",lwd=2)
lines(lstat.grid,ypred2$fit+2*ypred2$se,lty="dashed",col="green",cex=.7)
lines(lstat.grid,ypred2$fit-2*ypred2$se,lty="dashed",col="green",cex=.7)

## ----echo=FALSE----------------------------------------------------------
ypred2<-predict(fit2,newdata=Hedonic[-train,])
mean((ypred2-Hedonic.test)^2)#MSE 0.06339495


## ------------------------------------------------------------------------
rm=Hedonic$rm
rm=Hedonic.train$rm
rmLims=range(rm)
rm.grid=seq(from=rmLims[1],to=rmLims[2])

Hedonic.test<-Hedonic[-train,"mv"]
fit2<-lm(Hedonic$mv~ns(rm,df=4),data=Hedonic,subset=train)

ypred2<-predict(fit2,newdata=list(rm=rm.grid),se=T)
plot(rm,  Hedonic.train$mv ,col="gray")
title("Spline Regression on RM")
lines(rm.grid,ypred2$fit,col="blue",lwd=2)
lines(rm.grid,ypred2$fit+2*ypred2$se,lty="dashed",col="green",cex=.7)
lines(rm.grid,ypred2$fit-2*ypred2$se,lty="dashed",col="green",cex=.7)

## ------------------------------------------------------------------------
ypred2<-predict(fit2,newdata=Hedonic[-train,])
mean((ypred2-Hedonic.test)^2)#MSE 0.1006758

## ----echo=FALSE----------------------------------------------------------
library(gam)
names(Hedonic)
set.seed(1)
train=sample(1:nrow(Hedonic),nrow(Hedonic)/2)
#Hedonic.train<-Hedonic[train,]
Hedonic.test<-Hedonic[-train,"mv"]

gam<-lm(Hedonic$mv~ns(rm,df=2)+ns(lstat,df=2),data=Hedonic,subset=train)

plot.gam(gam,se=TRUE,col="blue")
ypred.gam<-predict(gam,newdata=Hedonic)
mean((ypred.gam-Hedonic.test)^2)#MSE 0.3262217

## ----echo=FALSE----------------------------------------------------------
library(splines)
library(scoop)

X <- Hedonic[, -c(1,5,15)]
y <- Hedonic$mv
X.bs <- data.frame(lapply(X, bs))
grp <- rep(1:12, each=3)
grp.lasso <- group.lasso(as.matrix(X.bs), y, grp)
plot(grp.lasso, label = TRUE)
plot(grp.lasso, yvar='group',label = TRUE)

## ----echo=FALSE----------------------------------------------------------
library(MASS)
library(tree)
set.seed(1)
train=sample(1:nrow(Hedonic),nrow(Hedonic)/2)
tree.hedonic=tree(Hedonic$mv~.,Hedonic,subset=train)
summary(tree.hedonic)


## ----echo=FALSE,fig.width=12, fig.height=8-------------------------------
plot(tree.hedonic)
text(tree.hedonic,pretty=0)

## ------------------------------------------------------------------------
cv.hedonic=cv.tree(tree.hedonic)

## ------------------------------------------------------------------------
library(randomForest)
set.seed(1)
rf.hedonic<-randomForest(Hedonic$mv~.,Hedonic,subset=train,importance=TRUE,keep.forest=T,mtry=5)
ypred.rf=predict(rf.hedonic,newdata=Hedonic[-train,])
par(mfrow=c(1,1))
Hedonic.test<-Hedonic[-train,"mv"]
plot(ypred.rf,Hedonic.test,col="gray",main=" model RF VS dataset de test\n")
abline(0,1,col="red")
mean((ypred.rf-Hedonic.test)^2) #0.02556644

## ------------------------------------------------------------------------
library(randomForest)
set.seed(1)
#mtry nombre de prédicteurs sélectionné pour la scission
bag.hedonic=randomForest(Hedonic$mv~.,data=Hedonic,subset=train,mtry=14,importance=TRUE)
bag.hedonic
##Calcul du MSE
ypre.bag=predict(bag.hedonic,newdata=Hedonic[-train,])
Hedonic.test<-Hedonic[-train,"mv"] #dataset de test
plot(ypre.bag,Hedonic.test,col="grey")
abline(0,1,col="orange")
mean((ypre.bag-Hedonic.test)^2) #0.03170125

## ------------------------------------------------------------------------
library(gbm)
set.seed(1)
Hedonic.train=Hedonic[train,]
boost.hedonic=gbm(Hedonic.train$mv~.,Hedonic.train,distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.hedonic)

## ----echo=FALSE----------------------------------------------------------
par(mfrow=c(1,2))
plot(boost.hedonic,i="lstat",main="influence de LSTAT sur MV \n",col = "blue")
plot(boost.hedonic,i="rm",main="influence de RM sur MV\n",col="blue")

## ----echo=FALSE----------------------------------------------------------
ypre.boost<-predict(boost.hedonic,newdata=Hedonic[-train,],n.trees=5000)
mean((ypre.boost-Hedonic.test)^2) #MSE 0.02760091

