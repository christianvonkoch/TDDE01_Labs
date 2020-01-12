#Read data
RNGversion('3.5.1')
Dataframe=read.csv("australian-crabs.csv")
CW=Dataframe$CW
BD=Dataframe$BD
plot(CW, BD, col=c("blue", "orange")[Dataframe$species], main="Plot of CW vs BD", sub="Blue = blue, Orange=orange")

##Answer: It seems like they are since it is a clear distinction between the two clusters of data. A linear predictor
##seems like a good way of classifying the data.

#Create function for misclassification rate
misclass=function(conf_matrix, fit_matrix){
  n=length(fit_matrix[,1])
  return(1-sum(diag(conf_matrix))/n)
}


library(MASS)
library(e1071)
fit_naive=naiveBayes(species~CW+BD, data=Dataframe)
pred=predict(fit_naive, newdata=Dataframe, type="class")
confusion_naive=table(Dataframe$species, pred)
print(confusion_naive)
misclass_naive=misclass(confusion_naive, Dataframe)

##Answer: The quality of the fit seems to be rather bad since the model misclassifies 39,5 % of the data according
##to the misclassification rate. Since the data is strongly correlated and the method of using Naive bayes implies
##an assumption of independent features it is reasonable that the model does not perform that well, again since it
##is clear from the plot that the two features used, CW and BD are strongly correlated. 

glmmodel=glm(species~CW+BD, data=Dataframe, family="binomial")
pred_glm=predict(glmmodel, newdata=Dataframe, type='response')
species_vector=as.factor(c(ifelse(pred_glm>0.5, 'Orange', 'Blue')))
confusion_glm=table(Dataframe$species, species=ifelse(pred_glm>0.5, 'Orange', 'Blue'))
print(confusion_glm)
plot(CW, BD, main="Plot predicted values of CW and BD but with logistic regression",
     col=c("blue", "orange")[species_vector], xlab="CW", ylab="BD", xlim=c(18,53), ylim=c(5,22),
     sub="Red = Female, Blue = Male, Green = Boundaryline")

boundaryline = function(length, coefficientvector, prior) {
  return(-coefficientvector[1]/coefficientvector[3]-(coefficientvector[2]/coefficientvector[3])*length+
           log(prior/(1-prior))/coefficientvector[3])
}

curve(boundaryline(x, glmmodel$coefficients, 0.5), col="green", add=TRUE)
glmmodel$coefficients

##Answer: The quality of the classification is as expected really good (since looking at the plot, a linear
##classifier seemed appropriate). The model only misclassified 4 points which yielded a low misclassification
##rate. The equation for the boundary line is as follows: BD=-0.484810/9.432817+CW*3.624760/9.432817

pcaData=Dataframe[,7:8]
pcaData=scale(pcaData)
response=as.vector(Dataframe$species)
pcaAnalysis=prcomp(pcaData)
lambda=pcaAnalysis$sdev^2
#Eigenvalues
print(lambda)
#Proportion of variation
propVar= lambda/sum(lambda)*100
screeplot(pcaAnalysis)
print(propVar)
summary(pcaAnalysis)
X=pcaData
coeff=pcaAnalysis$rotation
Z=X%*%coeff

##Answer: Since we could se a clear pattern in the plot from step 1, PCA analysis will select the angle of the
##line as its first principle component since across this line we can find the most variation in the data. Since
##the data was so strongly correlated, almost all of the variation in the data can be explained by just one 
##PCA component. The equation expressing principle component coordinates through the original ones is as follows:
##PCACoordinates=X*coefficientsMatrix, i.e. PCACoordinates=X*pcaAnalysis$rotation.
##Equations separately: PC1=CW*0.7071068+BD*0.7071068, PC2=CW*(-0.7071068)+BD*0.7071068

naiveData=as.data.frame(cbind(pcaAnalysis$x, species=response))
naiveModelPCA=naiveBayes(species~PC1+PC2, data=naiveData)
predPCA=predict(naiveModelPCA, newdata=naiveData, type="class")
confusion_naivePCA=table(Dataframe$species, predPCA)
print(confusion_naivePCA)
misclass_naivePCA=misclass(confusion_naivePCA, Dataframe)

##Answer: The classification is now 100 % correct. This is due to the fact that the two PCA components derived
##are mutually independent of each other which makes naive bayes classifier a perfect fit since this is exactly
##what the model is assuming. 