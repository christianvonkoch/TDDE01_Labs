##You are asked to use the function neuralnet of the R package of the same name to train a neural network (NN) to
##mimic the trigonometric sine function. You should run the following code to obtain the training and test data.

##Produce the code to train the NN on the training data tr and test it on the data te. Use a single hidden layer with
##three units. Initialize the weights at random in the interval [-1,1]. Use the default values for the rest of
##parameters in the function neuralnet. You may need to use the function compute. Confirm that you get results similar
##to the following figure. The black dots are the training data. The blue dots are the test data. The red dots are the
##NN predictions for the test data.

library(neuralnet)
set.seed(1234567890)
Var <- runif(50, 0, 3)
tr <- data.frame(Var, Sin=sin(Var))
Var <- runif(50, 3, 9)
te <- data.frame(Var, Sin=sin(Var))
n = dim(tr)[1]
# Random initialization of the weights in the interval [-1, 1] 
winit <- runif(10, -1, 1)
nn=neuralnet(Sin~Var, data=tr, hidden=3, startweights=winit)
pred=predict(nn, newdata=te)
plot(tr$Var, tr$Sin, xlim=c(0,9), ylim=c(-2,2), xlab="Var", ylab="Sin")
points(te$Var, te$Sin, col="blue")
points(te$Var, pred, col="red")

##Answer: The plot resembles the one given so it is confirmed.

##In the previous figure, it is not surprising the poor performance on the range [3,9] because no training point falls
##in that interval. However, it seems that the predictions converge to -2 as the value of Var increases. Why do they
##converge to that particular value ? To answer this question, you may want to look into the weights of the NN
##learned.

sigmoid=function(input){
  return(1/(1+exp(-input)))
}

z1=sigmoid(0.61705*te$Var-1.51988)
z2=sigmoid(1.995*te$Var-1.27708)
z3=sigmoid(-1.61733*te$Var+4.89639)
y=-3.92871*z1+2.67522*z2+0.84607*z3-0.62953
print(y)

##Answer: When Var goes towards infinity the first and second node in the hidden layer is activated and the third
##node in the hidden layer is not. This yields an approximate response value of -3.92871+2.67522-0.62953 (bias)=-1,88302
##which can be seen in the graph. 

library(kernlab)
set.seed(1234567890)
data(spam)

#Create function for misclassification rate
missclass=function(conf_matrix, fit_matrix){
  n=length(fit_matrix[,1])
  return(1-sum(diag(conf_matrix))/n)
}

index=sample(1:4601)
train=spam[index[1:2500],]
valid=spam[index[2501:3501],]
test=spam[index[3502:4601],]

C=seq(0.2,10.2,0.5)
trainScore=numeric(21)
validScore=numeric(21)
for(i in 1:length(C)){
  svmmodel=ksvm(type~., data=train, kernel="rbfdot", kpar=list(sigma=0.05), C=C[i])
  pred=predict(svmmodel, newdata=valid)
  confusion=table(valid$type, pred)
  validScore[i]=missclass(confusion, valid)
}
plot(1:21, validScore, col="red", type="b")
bestModel=which(min(validScore) == validScore)
bestParam=C[bestModel]

finalModel=ksvm(type~., data=spam[index[1:3501],], kernel="rbfdot", kpar=list(sigma=0.05), C=bestParam)
finalPred=predict(svmmodel, newdata=test)
finalConfusion=table(test$type, finalPred)
finalMisclass=missclass(finalConfusion, test)

##Answer: The optimal C-value is 1.2. When C=0 and no support vector was found you still are under the assumption that
##the data is linearly separable. When no support vector is found I assume that the data is not linearly separable.
##The generalization error for the optimal model is approximately 0.08. 



