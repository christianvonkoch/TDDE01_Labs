#1: Read data
RNGversion('3.5.1')

data=read.csv2("NIRspectra.csv")
data$Viscosity=c()
n=dim(data)[1] 

#1: Conduct standard PCA using the feature space and provide a plot explaining how much variation is explained by each
#feature. Provide plot that show the scores of PC1 vs PC2. Are there unusual diesel fuels according to this plot. 
pcaAnalysis=prcomp(data)
lambda=pcaAnalysis$sdev^2
#Eigenvalues
print(lambda)
#Proportion of variation
propVar= lambda/sum(lambda)*100
screeplot(pcaAnalysis)
print(propVar)
noOfVars=1
sumOfVariation=propVar[noOfVars]
while(sumOfVariation<99){
  noOfVars=noOfVars+1
  sumOfVariation=sumOfVariation+propVar[noOfVars]
}
#Print number of variables used
print(noOfVars)
#Print PC1 and PC2 in plot
plot(pcaAnalysis$x[,1],pcaAnalysis$x[,2], ylim=c(-10,10), type="p", col="blue", main="PC1 vs PC2", xlab="PC1", ylab="PC2")
#We can see from the graph that the data is very accurately described by PC1.

#2: Make trace plots of the loadings of the components selected in step 1. Is there any principle component that is 
#explaines by mainly a few original features?

U=pcaAnalysis$rotation
plot(U[,1], main="Traceplot, PC1", xlab="index", ylab="PC1", type="b")
plot(U[,2], main="Traceplot, PC2", xlab="index", ylab="PC2", type="b")
#We can see from graph that PC2 is not described by so many original features since it is close to zero for many of
#the features. The last 30 or so variables have an effect on PC2. 

#3: Perform independent Component Analysis (ICA) with no of components selected in step1 (set seed 12345). Check the
#documentation of R for fastICA method and do following:
# Compute W'=K*W and present columns of W' in form of the trace plots. Compare with trace plots in step 2 and make
# conclusions. What kind of measure is represented by the matrix W'.
# Make a plot of the scores of the first two latent features and compare it with the score plot from step 1. 

#Install package fastICa
#install.packages("fastICA")
library("fastICA")

set.seed(12345)
icaModel = fastICA(data, n.comp=2, verbose=TRUE)
W=icaModel$W
K=icaModel$K
W_est=K%*%W
plot(W_est[,1], main="Traceplot, ICA1", xlab="index", ylab="ICA1", type="b", col="red")
plot(W_est[,2], main="Traceplot, ICA2", xlab="index", ylab="ICA2", type="b", col="red")
#Compared to the plots in step 2 the ICA1 follows in roughly the same pattern as PCA2 and ICA2 the same as PCA1.
plot(icaModel$S[,1], icaModel$S[,2], main="ICA1 vs ICA2", xlab="ICA1", ylab="ICA2", type="p", col="blue")
#We can see from the plot that the dat is pretty well described by ICA2 whereas ICA1 is not that significant in
#describing the data (since it is close to 0 most of the cases). Some outliers are however described by ICA1.




