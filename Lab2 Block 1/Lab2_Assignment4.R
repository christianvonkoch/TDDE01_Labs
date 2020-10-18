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
plot(pcaAnalysis$x[,1],pcaAnalysis$x[,2], ylim=c(-10,10), type="p", col="blue", main="PC1 vs PC2", xlab="PC1", 
     ylab="PC2")
# We can see from the graph that the data is very accurately described by PC1.

#Conclusion: From the screeplot it can be conlcuded that the two components captures almost all of the variation in the
#data. Therefore PCA analysis is suitable for the data. Two components capture 99.5957 % of the variation and therefore
#these two components will be used in the following steps. Most of the data points are around 0 for PC1 but there are
#some data points which can be described as outliers located to the farthest right in the score plot. 

#2: Make trace plots of the loadings of the components selected in step 1. Is there any principle component that is 
#explained by mainly a few original features?

U=pcaAnalysis$rotation
plot(U[,1], main="Traceplot, PC1", xlab="index", ylab="PC1", type="b")
plot(U[,2], main="Traceplot, PC2", xlab="index", ylab="PC2", type="b")

#Conclusion: We can see from graph that PC2 is not described by so many original features since it is close to zero
#for many of the features. The last 30 or so variables have an effect on PC2. 

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
plot(icaModel$S[,1], icaModel$S[,2], main="ICA1 vs ICA2", xlab="ICA1", ylab="ICA2", type="p", col="blue")

#Conclusion: When comparing the trace plots of ICA1 and ICA2 with PC1 and PC2 from step 2, it is noteable that for the
#first component the dependency on the features increases as the index increases. It is also noteable that the plots
#for the different components appear to be each others mirrors. This is reasonable because PCA tries to maximize the
#variance, i.e. look for correlation between the different features, whereas ICA tries to do the exact opposite, i.e.
#maximizing the independence between the different features by creating an orthogonal coordinate system. The parameter
#W' which is computed by W(hat)=K*W describes how the features explain the principal components ICA1 and ICA2. 
#When comparing the last score plot with the score plot from step 1 it can also be concluded that the score plot of ICA
#is mirroring the score plot of PCA. However, the axis of the coordinate system of ICA have been standardized wh  ich is
#the difference between the plots. 

