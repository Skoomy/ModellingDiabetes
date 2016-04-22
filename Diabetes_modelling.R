## ------------------------------------------------------------------------
diabetes<-read.table("~/ensiie_S3/MLG/diabetes_simple_effects.txt")
#apercu des données 
head(diabetes)

## ---- echo=TRUE----------------------------------------------------------
diabetes.train<-subset(diabetes, train == TRUE, select= -train)

diabetes.test<-subset(diabetes,train !=TRUE, select= -train)

head(diabetes.train)

## ------------------------------------------------------------------------
hist(diabetes.train$prog,freq=FALSE,col="red",main="histogramme de l'Indice de progression du diabetes")
lines(density(diabetes.train$prog),col="blue")
plot(ecdf(diabetes.train$prog),main= "Fonction de répartition\n de Prog", xlab= " ")

## ------------------------------------------------------------------------
library(GGally)
ggscatmat(diabetes.train,columns=c(1:11))
heatmap(abs(cor(diabetes.train[, - 1 ])))

## ------------------------------------------------------------------------
library(ggplot2)
library(reshape2)
ggplot(melt(diabetes.train[,-1], id="sex"),aes(x=as.factor(sex),y=value)) +
geom_boxplot()+facet_wrap(~variable,scales="free_y") 


## ------------------------------------------------------------------------
full<-lm(prog~. ,diabetes.train)
null<-lm(prog~1 ,diabetes.train)

## ------------------------------------------------------------------------
anova(full)


## ------------------------------------------------------------------------
library (leaps)
best_500<-summary(regsubsets(prog~.,data=diabetes.train,nvmax=10,nbest=500,really.big=TRUE))
#mettons chacun de ces élements dans une variable
best_500.size<-as.numeric(rownames(best_500$which))
best_500.best.rss<-tapply(best_500$rss,best_500.size,min)
best_500.best.adjr2<-tapply(best_500$adjr2,best_500.size,max)
best_500.best.bic<-tapply(best_500$bic,best_500.size,min)
best_500.best.Cp<-tapply(best_500$cp, best_500.size,min) 


#Commplete les ellement
n<-nrow(diabetes.train)
residus<-sum(resid(null)^2)
best_500.best.rss<-c(residus,best_500.best.rss)
best_500.best.adjr2<-c(summary(null)$adj.r.squared,best_500.best.adjr2)
best_500.best.bic<-c(log (residus/n),best_500.best.bic)
best_500.best.Cp<-c(n+residus/(n*summary(full)$sigma^2),best_500.best.Cp)



## ------------------------------------------------------------------------
intervalle<-0:10
par(mfrow=c(2,2))
plot(intervalle,log(best_500.best.rss),type="b",xlab ="0:10",ylab="rss",col="blue",cex=0.7)
points(best_500.size,log(best_500$rss),pch=20,col="orange",cex=0.4)

plot(intervalle,best_500.best.Cp,type="b",xlab ="0:10",ylab="CP",col="blue",cex=0.7)
points(best_500.size,best_500$cp,pch=20,col="orange",cex=0.4)

plot(intervalle,best_500.best.bic,type="b",xlab ="0:10",ylab="BIC",col="blue",cex=0.7)
points(best_500.size,best_500$bic,pch=20,col="orange",cex=0.4)

plot(intervalle,best_500.best.adjr2,type="b",xlab ="0:10",ylab="ADJR2",col="blue",cex=0.7)
points(best_500.size,best_500$adjr2,pch=20,col="orange",cex=0.4)


## ------------------------------------------------------------------------
#Stepwise Regression 
stepwise_reg<-list(lower=terms(prog~1,data= diabetes.train),upper=terms(prog~ . , data=diabetes.train))
step.AIC<-step(null,stepwise_reg,direction="both" ,trace=FALSE)
step.BIC<-step(null,stepwise_reg,direction="both",k=log(n),trace=FALSE)

# comande AIC
step.AIC

#Commande BIC 
step.BIC

par(mfrow=c(2,2))
plot(step.BIC)
plot(step.AIC)

## ------------------------------------------------------------------------
library(glmnet)

x<-as.matrix(diabetes.train[,-1])
y<-diabetes.train$prog

ridge<-glmnet(x,y,alpha=0)

par(mfrow=c(1,3))

#Mise en place de la légende
lbs_fun <- function(ridge, ...) {
  L <- length(ridge$lambda)
  x <- log(ridge$lambda[L])
  y <- ridge$beta[, L]
  labs <- names(y)
  text(x, y, labels=labs, ...) }

par(mfrow=c(1,1))
plot(ridge, xvar="lambda", col=1:dim(coef(ridge))[1])
#on trace pour obtenir le nom des variables
lbs_fun(ridge)


plot(ridge,xvar="norm",col=1:dim(coef(ridge))[1])
lbs_fun(ridge)
plot(ridge,xvar="dev",col=1:dim(coef(ridge))[1])
help(plot)
lbs_fun(ridge)
help("cv.glmnet")


## ------------------------------------------------------------------------
ridge_cross10<-cv.glmnet(x,y,nfolds=10,alpha=0,grouped=FALSE)


ridge.se<-log(ridge_cross10$lambda.1se)         
ridge.min<-log(ridge_cross10$lambda.min)
# L'intervalle de points idéale est pour lambda compris entre ridge.se et ridge.min 
ridge.se
ridge.min


## ------------------------------------------------------------------------
plot(ridge_cross10)

## ------------------------------------------------------------------------
lasso <- glmnet(as.matrix(diabetes.train[,-1]),diabetes.train$prog)
plot(lasso, xvar="lambda", col=1:dim(coef(lasso))[1])
#on trace pour obtenir le nom des variables
lbs_fun(lasso)

## ------------------------------------------------------------------------
#Tracer du graphe
plot(lasso, xvar="norm", col=1:dim(coef(lasso))[1]) 
lbs_fun(lasso)

## ------------------------------------------------------------------------
plot(lasso, xvar="dev", col=1:dim(coef(lasso))[1])
lbs_fun(lasso)

## ------------------------------------------------------------------------
lasso_cross.10 <- cv.glmnet(as.matrix(diabetes.train[,-1]),diabetes.train$prog,nfolds=10, grouped=FALSE)

lasso_cross.1000 <- cv.glmnet(as.matrix(diabetes.train[,-1]),diabetes.train$prog,nfolds=1000, grouped=FALSE)

plot(lasso_cross.10)
plot(lasso_cross.1000)

## ------------------------------------------------------------------------
mean(predict(lasso_cross.10,newx=as.matrix(diabetes.train[,-1]),s=lasso_cross.10$lambda.min))
mean(predict(lasso_cross.10, newx=as.matrix(diabetes.train[,-1]),s=lasso_cross.10$lambda.1se))

