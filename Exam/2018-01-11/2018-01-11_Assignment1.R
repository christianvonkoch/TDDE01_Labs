#Read data and divide randomely into train and test
Dataframe=read.csv("video.csv")
Dataframe$codec = c()
n=dim(Dataframe)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=Dataframe[id,]
test=Dataframe[-id,]

##Perform principle component analysis with and without scaling. How many vars for 95 % of variation in both cases.
##Explain why so few components are needed when scaling is not done. 

PCAdata=subset(train, select=-utime)
pcaAnalysis_noScale=prcomp(PCAdata, scale=FALSE)
screeplot(pcaAnalysis_noScale)
lambda=pcaAnalysis_noScale$sdev^2
print(lambda)
#Proportion of variation
propVar=lambda/sum(lambda)*100
print(propVar)
#Function for calculating the number of components needed for explaining at least 95% of the variation.
calcNoVars = function(data){
  noOfVars=1
  sumOfVariation=data[noOfVars]
  while(sumOfVariation<95){
    noOfVars=noOfVars+1
    sumOfVariation=sumOfVariation+data[noOfVars]
  }
  return(noOfVars)
}
print(calcNoVars(propVar))
pcaAnalysis_scale=prcomp(PCAdata, scale=TRUE)
lambda_scale=pcaAnalysis_scale$sdev^2
propVar_scale=lambda_scale/sum(lambda_scale)*100
print(propVar_scale)
screeplot(pcaAnalysis_scale)
print(calcNoVars(propVar_scale))

##Answer: Fewer components are needed since outliers of the different parameters have a higher impact when they are not
##scaled accordingly. When scaled outliers have less impact and therefore the percentage of the variation for each 
##component decreases.

##Write a code that fits a principle component regression ("utime" as response and all scaled numerical variables as
##features) with M components to the training data and estimates the training and test errors, do this for all
##feasible M values. Plot dependence of the training and test errors on M and explain this plot in terms of 
##bias-variance tradeoff.

trainscore=c()
testscore=c()
library(pls)
pcamodel=pcr(utime~., 17, data=train, scale=TRUE)
for (i in 1:17) {
  pred_train=predict(pcamodel, ncomp=i)
  pred_test=predict(pcamodel, newdata=test, ncomp=i)
  trainscore[i]=mean((train$utime-pred_train)^2)
  testscore[i]=mean((test$utime-pred_test)^2)
}

plot(trainscore, xlab="Index", ylab="Error", col="blue", type="b", ylim=c(100,300))
points(testscore, xlab="Index", ylab="Error", col="red", type="b", ylim=c(100,300))
noOfPCA=which(testscore == min(testscore))
print(noOfPCA)

##Answer: When using more and more components the bias decreases and the variance goes up. The model performs better 
##and better on training data. However, at one point the model becomes overfitted and performs worse on the test data
##as more components are added. The point where the model performs best on test data is when using 14 PC:s. 

##Use PCR model with M=8 and report a fitted probabilistic model that shows the connection between the target and the
##principal components.

pcamodel_new=pcr(utime~., 8, data=train, scale=TRUE)
pcamodel_new$Yloadings
mean(pcamodel_new$residuals^2)

##Answer: The formula is given by the loadings of the model and the variance is given by taking the average of the sum
##of squared residuals.

##Use original data to create variable "class" that shows "mpeg" if variable "codec" is equal to "mpeg4", and "other"
##for all other values of "codec". Create a plot of "duration" versus "frames" where cases are colored by "class". 
##Do you think that the classes are easily separable by a linear decision boundary?

Dataframe2=read.csv("video.csv")
Dataframe2=subset(Dataframe2, select=c(codec, frames, duration))
Dataframe2=cbind(Dataframe2, class=ifelse(Dataframe2$codec == 'mpeg4', 'mpeg', 'other'))
plot(Dataframe2$duration, Dataframe2$frames, col=c("red", "blue")[Dataframe2$class], xlab="Duration", ylab="Frames",
     main="Plot of duration vs frames")

##Answer: It seems that a linear decision boundary could separate the two classes rather well with exception of a few
##cases near the origin of the plot. 

##Fit a Linear Discriminant Analysis model with "class" as target and "frames" and "duration" as features to the
##entire dataset (scale features first). Produce the plot showing the classified data and report the training error. 
##Explain why LDA was unable to achieve perfect (or nearly perfect) classification in this case.

#Create function for misclassification rate
missclass=function(conf_matrix, fit_matrix){
  n=length(fit_matrix[,1])
  return(1-sum(diag(conf_matrix))/n)
}

library(MASS)
LDAData=Dataframe2
LDAData$duration=scale(LDAData$duration)
LDAData$frames=scale(LDAData$frames)
ldamodel=lda(class~duration+frames, data=LDAData)
predicted_lda=predict(ldamodel, data=LDAData)
confusion_matrix=table(LDAData$class, predicted_lda$class)
misclass=missclass(confusion_matrix, LDAData)
print(confusion_matrix)
print(misclass)
plot(Dataframe2$duration, Dataframe$frames, col=c("red", "blue")[predicted_lda$class], xlab="Duration", ylab="Frames",
     main="Plot of duration vs frames after LDA")

##Answer: Because the two clusters of data don't have the same covariance matrix which can also be seen in the plot.
##The linear patterns are different for the two classes and have clearly different slopes. 

##Fit a decision tree model with "class" as target and "frames" and "duration" as features to the entire dataset, 
##choose an appropriate tree size by cross-validation. Report the training error. How many leaves are there in the
##final tree? Explain why such a complicated tree is needed to describe such a simple decision boundary.

library(tree)
treemodel=tree(class~duration+frames, data=Dataframe2)
summary(treemodel)
#Since number of terminal nodes is 11 we will check which number of leaves that is optimal in terms of lowest deviance
trainscore=rep(0,11)
for (i in 2:11) {
  prunedTree=prune.tree(treemodel, best=i)
  trainscore[i]=deviance(prunedTree)
}
plot(2:11, trainscore[2:11], type="b", col="red", ylim=c(0,700))
finalTree=prune.tree(treemodel, best=11)
temp=predict(treemodel, type="class")
confusion_matrix_tree=table(Dataframe2$class, temp)
tree_misclass= missclass(confusion_matrix_tree, Dataframe2)
print(confusion_matrix_tree)
print(tree_misclass)

##Answer: As seen in the plot the optimal number of leaves is the maximal one which is 11. 

