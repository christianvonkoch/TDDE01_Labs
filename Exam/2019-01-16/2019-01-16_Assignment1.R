RNGversion('3.5.1')

Dataframe=read.csv("Influenza.csv")
Mortality=Dataframe$Mortality

like=function(y, lambda){
  n=length(y)
  return(lambda*n-log(lambda)*sum(y)+sum(log(factorial(y))))
}

#Find maximum likelihood value of theta
lambda_max = function(y){
  n=length(y)
  return(sum(y)/n)
}

lambda_max=lambda_max(Mortality)
lambda=seq(10,2910,100)
plot(like(Mortality, lambda), lambda, main="The minus loglike function of mortality depending on Lambda", 
     xlim=c(10,2910))

##Answer: Towards infinity, don't know how to fix

library(cvTools)
library(glmnet)

features=scale(Dataframe[,-3])
data=cbind(features, Mortality)

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]
covariates=train[,1:8]
response=train[,9]

lassomodel=cv.glmnet(as.matrix(covariates), response, alpha=1, family="poisson")
opt_lambda=lassomodel$lambda.min
plot(lassomodel)
coef(lassomodel, s="lambda.min")
y=test[,9]
ynew=predict(lassomodel, newx=as.matrix(test[,1:8]), type="response", s="lambda.min")
MSE=mean((ynew-y)^2)
interceptval=exp(coef(lassomodel, s="lambda.min")[1])


##Answer: Optimal lasso coefficients shown above. The feature year is not used. The features Influenza, 
##temperature.deficit and influenza_lag2 seem to have the biggest impact on the target. The exponential of the
##intercept alpha is similar to the optimal lambda in step 1. This is due to the fact that the link between a poisson
##distribution and the calculated mean is the log-function. By applying exp to the intercept we thereby receive a 
##value which corresponds to the calculated mean of lambda. 

library(tree)
train=as.data.frame(train)
test=as.data.frame(test)
treemodel=tree(Mortality~., data=train)
set.seed(12345)
cv.res=cv.tree(treemodel)
plot(cv.res$size, cv.res$dev, type="b", col="red")
bestSize=cv.res$size[which(min(cv.res$dev) == cv.res$dev)]
finalTree=prune.tree(treemodel, best=bestSize)
plot(finalTree)
text(finalTree, pretty=0)
yFit=predict(finalTree, newdata=test, type="vector")
MSE_tree=mean((yFit-test$Mortality)^2)

##Answer: The MSE for the tree model is higher than for the LASSO-model which implies that the LASSO model should be used since
##it performs better. It is not reasonable to do LASSO penalization to tree model because the tree model is not
##continuous but discrete. 

PCAdata=subset(train, select=-Mortality)
pcaAnalysis=prcomp(PCAdata, scale=FALSE)
screeplot(pcaAnalysis)
lambda=pcaAnalysis$sdev^2
print(lambda)
#Proportion of variation
propVar=lambda/sum(lambda)*100
print(propVar)
#Function for calculating the number of components needed for explaining at least 95% of the variation.
calcNoVars = function(data){
  noOfVars=1
  sumOfVariation=data[noOfVars]
  while(sumOfVariation<90){
    noOfVars=noOfVars+1
    sumOfVariation=sumOfVariation+data[noOfVars]
  }
  return(noOfVars)
}
print(calcNoVars(propVar))
summary(pcaAnalysis)
new_base=pcaAnalysis$x
set.seed(12345)
lasso_PCA=cv.glmnet(new_base[,1:5], response, alpha=1, family="poisson", lambda=seq(0,50,0.1))
plot(lasso_PCA)
opt_lambda_PCA=lasso_PCA$lambda.min
coef(lasso_PCA, s="lambda.min")

##Answer: 5 components are needed to describe more than 90 % of the variation in the data. The complexity of the model
##decreases as lambda increases since a higher lambda penalizes the coefiicients more. 3 features are selected by the
##LASSO regression. The probabilistic model is:
##Yi~P(exp(7.484554465-0.035756922*PC1-0.009395205*PC2-0.004745676*PC3+0.011627449*PC4+0.003882809*PC5))