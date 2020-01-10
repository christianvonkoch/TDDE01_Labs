##Dataset crx.csv contains encrypted information about the customers of a bank and whether each individual has paid back
##the loan or not: Class 1=paid back, 0=not paid back

##Divide the dataset into training and test sets (80/20), use seed 12345. Fit a decision tree with default settings to
##the training data and plot the resulting tree. Finally, remove the second observation from the training data, fit the
##tree model again and plot the tree. Compare the trees and comment why the tree structure changed so much although only
##one observation is deleted.

#Read data
RNGversion('3.5.1')
library(tree)
Dataframe=read.csv("crx.csv")
n=dim(Dataframe)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.8))
train=Dataframe[id,]
test=Dataframe[-id,]

treemodel=tree(Class~., data=train)
summary(treemodel)
plot(treemodel)
text(treemodel, pretty=0)
train_new=train[-2,]
treemodel_new=tree(Class~., data=train_new)
plot(treemodel_new)
text(treemodel_new, pretty=0)

##Answer: Tree structure does not change at all. 

cv.res=cv.tree(treemodel)
plot(cv.res$size, cv.res$dev, type="b", col="red")
plot(log(cv.res$k), cv.res$dev, type="b", col="red")
optimalTree=prune.tree(treemodel, best=3)
summary(optimalTree)
plot(optimalTree)
text(optimalTree, pretty=0)

##Answer: Two variables were selected for the optimal tree; A9 and A10. The best no of leaves is 3. 

x_train = model.matrix( ~ .-1, train[,-16])
library(glmnet)
class=as.factor(train$Class)
lassomodel=cv.glmnet(x_train, class, alpha=1, family="binomial")
lassomodel$lambda.min
plot(lassomodel)
coef(lassomodel, s="lambda.min")

##Answer: Optimal penalty parameter is 0.01036912 and the number of components used are 23. The optimal model does not
##look significantly better than when the smallest value is used. 

x_test = model.matrix( ~ .-1, test[,-16])
pred_lasso=predict(lassomodel, s=lassomodel$lambda.min, newx=x_test, type="response")

errorfunction=function(classvector, predvector) {
  return(sum(classvector*log(predvector)+(1-classvector)*log(1-predvector)))
}

pred_tree=predict(optimalTree, newdata=test, type="vector")
error_tree=errorfunction(test$Class, pred_tree)
error_lasso=errorfunction(test$Class, pred_lasso)

##The tree model is better according to this criterion. This criterion might be more suitable since it takes into 
##account the probability of a class being classified and not just if it gets it right.
