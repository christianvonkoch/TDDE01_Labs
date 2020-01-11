##In the following steps, you are asked to use the R package kernlab to learn a SVM for classifying the spam dataset
##that is included with the package. For the C parameter consider values 1 and 5. Consider the radial basis function
##kernel (also known as Gaussian) and the linear kernel. For the former, consider a width of 0.01 and 0.05. This 
##implies that you have to select among six models.

##Use nested cross-validation to estimate the error of the model selection task described above. Use two folds for
##inner and outer cross-validation. Note that you only have to implement the outer cross-validation: The inner
##cross-validation can be performed by using the argument cross=2 when calling the function ksvm. Hint: Recall that
##inner cross-validation estimates the error of the different models and selects the best, which is then evaluated by
##the outer cross-validation. So, the outer cross-validation evaluates the model selection performed by the inner
##cross-validation

RNGversion('3.5.1')
library(kernlab)
set.seed(12345)
data(spam)
n=dim(spam)[1]
id=sample(1:n, floor(n*0.5))
fold1=spam[id,]
fold2=spam[-id,]
C=c(5,1)
width=c(0.01,0.01,0.05,0.05,0,0)
kernel=c("rbfdot", "rbfdot", "rbfdot", "rbfdot", "vanilladot", "vanilladot")

missclass=function(conf_matrix, fit_matrix){
  n=length(fit_matrix[,1])
  return(1-sum(diag(conf_matrix))/n)
}

prediction=function(train, test, C, width, kernel) {
  if (width == 0) {
    svmmodel=ksvm(type~., data=train, kernel=kernel, C=C, cross=2)
  } else {
    svmmodel=ksvm(type~., data=train, kernel=kernel, C=C, cross=2, kpar=list(sigma=width))
  }
  predicted=predict(svmmodel, newdata=test)
  confusion=table(test$type, predicted)
  return(missclass(confusion, test))
}

scores=numeric(6)
scores2=numeric(6)
for (i in 1:6) {
  scores[i]=prediction(fold1, fold2, C[(i %% 2)+1], width[i], kernel[i])
  scores2[i]=prediction(fold2, fold1, C[(i %% 2)+1], width[i], kernel[i])
}

avgScore=(scores+scores2)/2
bestModel=which(avgScore == min(avgScore))
print(bestModel)

##Answer: Optimal model is using a C-value of 5, gaussian kernel with width 0.01.

##Produce the code to select the model that will be returned to the user.

#Final model

finalModel = ksvm(type~., data=spam, kernel="rbfdot", C=5, kpar=list(sigma=0.01), cross=2)