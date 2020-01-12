#1: Read data and plot carapace length versus rear width (obs coloured by sex). Do you think that this data is easy
#to classify by LDA? Motivate answer. 

RNGversion('3.5.1')
Dataframe=read.csv("australian-crabs.csv")
n = length(Dataframe[,1])
CL = Dataframe$CL
RW = Dataframe$RW
plot(CL, RW, main="Plot of carapace length versus rear width depending on sex", sub="Red = Female, Blue = Male", 
     col=c("red", "blue")[Dataframe$sex], xlab="CL", ylab="RW")

#Create function for misclassification rate
missclass=function(conf_matrix, fit_matrix){
  n=length(fit_matrix[,1])
  return(1-sum(diag(conf_matrix))/n)
}

#Conclusion: Yes the classification seems to be linearly separable. However, the two clusters of data clearly have
#different covariance matrices (since angle of trend is different) which is not optimal for the LDA method.

#2: LDA analysis with target Sex, and features CL and RW and proportional prior by using lda() function in package MASS
#Make a Scatter plot of CL versus RW colored by the predicted Sex and compare it with the plot in step 1. Compute the
#misclassification error and comment on the quality of fit.

library("MASS")
model = lda(sex ~ CL+RW, data=Dataframe)
predicted = predict(model, data=Dataframe)
confusion_matrix = table(Dataframe$sex, predicted$class)
misclass = missclass(confusion_matrix, Dataframe)
print(confusion_matrix)
print(misclass)
plot(CL, RW, main="Plot predicted values of CL and RW depending on sex", sub="Red = Female, Blue = Male", 
     col=c("red", "blue")[predicted$class], xlab="CL", ylab="RW")

#Conclusion: When comparing the graph from step 1 and the graph of the predicted values it is noteable that the
#classifications do not differ that much. With a misclassification rate of only 0.035 and 200 datapoints it can be
#concluded that 7 observations were classified incorrectly. When comparing the graphs it is difficult to find the
#points which have changed color (since they have been classified incorrectly) but one example is the point farthest
#to the let which was classified as male but should have been classified as female. The model classifies the data
#accurately. 

#3: Repeat step 2 but use priors p(Male)=0.9 and p(Female)=0.1

model2 = lda(sex ~ CL+RW, data=Dataframe, prior=c(1,9)/10)
predicted2 = predict(model2, data=Dataframe)
confusion_matrix2 = table(Dataframe$sex, predicted2$class)
misclass2 = missclass(confusion_matrix2, Dataframe)
print(confusion_matrix2)
print(misclass2)
plot(CL, RW, main="Plot predicted values of CL and RW with priors p(Male)=0.9 and p(Female)=0.1"
     , sub="Red = Female, Blue = Male", col=c("red", "blue")[predicted2$class], xlab="CL", ylab="RW")

#Conclusion: From this graph we can see that a few more data points were classified incorrectly. This is due to the
#higher prior set on classifying a data point as male, i.e. 0.9. It is noteable in the confusion matrix that no males
#classified incorrectly. This is also due to the high prior which basically says that is is not that likely that a
#datapoint will be classified as a female. When the model in fact classify a data point as female it has to be sure
#of it, and this can be seen as stated above in the confusion matrix. On the other hand, more females are classified
#as males inaccurately since the higher prior. This also results in a higher misclassification rate of 0.08. 

#4: Repeat step 2 but now with logistic regression (use function glm()). Compare with LDA results. Finally, report the 
#equation of the  decision boundary and draw the decision boundary in the plot of the classified data.

model3 = glm(sex ~ CL+RW, data=Dataframe, family='binomial')
predicted3 = predict(model3, newdata=Dataframe, type='response')
sexvector = c()
for (i in predicted3) {
  if (i>0.9) {
    sexvector = c(sexvector, 'Male')
  } else {
    sexvector = c(sexvector, 'Female')
  }
}
print(sexvector)
sexvector_factor = as.factor(sexvector)
confusion_matrix3 = table(Dataframe$sex, sexvector_factor)
misclass3 = missclass(confusion_matrix3, Dataframe)
print(confusion_matrix3)
print(misclass3)
plot(CL, RW, main="Plot predicted values of CL and RW but with logistic regression",
     col=c("red", "blue")[sexvector_factor], xlab="CL", ylab="RW", xlim=c(0,50), ylim=c(0,20))

boundaryline = function(length, coefficientvector, prior) {
  return(-coefficientvector[1]/coefficientvector[3]-(coefficientvector[2]/coefficientvector[3])*length+
           log(prior/(1-prior))/coefficientvector[3])
}
par(new=TRUE)
curve(boundaryline(x, model3$coefficients, 0.9), xlab="CL", ylab="RW", col="green", from=0, to=50, xlim=c(0,50), 
      ylim=c(0,20),
      sub="Red = Female, Blue = Male, Green = Boundaryline")

#Conclusion: When using logistic regression the results are similar as the first built model with LDA. This is simply
#a coincident and no real conclusion can be drawn regarding the exact same misclassification rate except from that
#the models seem to classify the data in a similar way. When comparing which data points that are classified as
#females and males in the two models it can be concluded that the model using logistic regression classifies the data
#in a way which enables a boundary line more distinctly. This is due to the characteristics of the logistic regression
#model. The equation for the decision boundary line is as follows: RW(hat)=-(beta0+beta1*CL)beta2