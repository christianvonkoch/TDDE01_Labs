#1: Reorder your data with respect to the increase of MET and plot EX versus MET. Discuss what kind of model can be
#appropriate here. Use the reordered data in steps 2-5.

RNGversion('3.5.1')
#Read data 
set.seed(12345)
Dataframe=read.csv2("State.csv")
Dataframe=Dataframe[order(Dataframe$MET),]
MET=Dataframe$MET
EX=Dataframe$EX

plot(MET, EX, xlab="EX", ylab="MET", type="p", main="Plot of EX vs MET")

#Conclusion: Some kind of squared model might be useful here. 

#2: Use package tree  and fit a regression tree model with target EX and feature MET in which the number of the leaves
#is selected by cross-validation, use the entire data set and set minimum number of observations in a leaf equal to 8
#(setting mincut in tree.control).  Report the selected tree. Plot the original and the fitted data and histogram of
#residuals. Comment on the distribution of the residuals and the quality of the fit.

library(tree)
treemodel=tree(EX~MET, data=Dataframe, control=tree.control(48, mincut=8))
summary(treemodel)
plot(treemodel)
text(treemodel, pretty=0)
set.seed(12345)
cvTreeModel = cv.tree(treemodel)
plot(cvTreeModel$size, cvTreeModel$dev, type="b", col="red", xlab="Size", ylab="Dev")
bestSize = cvTreeModel$size[which.min(cvTreeModel$dev)]
bestTree=prune.tree(treemodel, best=bestSize)
plot(bestTree)
text(bestTree, pretty=0)
title("Optimal tree")
predData=predict(bestTree, newdata=Dataframe)
plot(MET, EX, xlab="EX", ylab="MET", type="p", col="red", main="Plot original vs predicted data")
points(MET, predData, col="blue")
summaryfit=summary(bestTree)
hist(summaryfit$residuals, breaks=10)

#Conclusion: The distribution of the residuals seems to be fairly normally distributed with no bias. The fit is quite
#good considering the simple model that it is. 

library(boot)
# computingbootstrapsamples
f=function(data, ind){
  data1=data[ind,]# extractbootstrapsample
  treeModel=tree(EX~MET, data=data1, control=tree.control(48, mincut=8))
  prunedtree=prune.tree(treeModel, best=3)
  predData=predict(prunedtree,newdata=Dataframe) 
  return(predData)
}
res=boot(Dataframe, f, R=1000) #make bootstrap
confIntNPBoot=envelope(res)
plot(MET, EX, xlab="EX", ylab="MET", pch=21, bg="orange", main="Plot original vs predicted data", ylim=c(100,500))
points(MET, predData, type="l", col="blue")
points(MET, confIntNPBoot$point[2,], type="l")
points(MET, confIntNPBoot$point[1,], type="l")

#Conclusion: The confidence bands are bumpy. This is due to the fact that no distribution is assumed for the data. The
#model will try to accostom as best it can from the data given. The width of the confidence band is rather high which
#indicates that the model used is not that reliable. Furthermore we can almost draw a straight line between the whole
#band which would mean that each EX value would yield the same MET value which again implies that the model is not that
#good. 

mle=prune.tree(treemodel, best=3)
summaryMLE = summary(mle)
rng=function(data, mle) {
  data1=data.frame(EX=data$EX, MET=data$MET)
  n=length(data$EX)
  #generatenew EX
  data1$EX=rnorm(n,predict(mle, newdata=data1), sd(summaryMLE$residuals))
  return(data1)
}

f1=function(data1){
  treemodel=tree(EX~MET, data=data1, control=tree.control(48,mincut=8)) #fit linearmodel
  prunedtree=prune.tree(treemodel, best=3)
  n=length(Dataframe$EX)
  #predictvaluesfor all EX values from the original data
  predData=predict(prunedtree,newdata=Dataframe) 
  predictedEX=rnorm(n, predData, sd(summaryMLE$residuals))
  return(predictedEX)
}
res=boot(Dataframe, statistic=f1, R=1000, mle=mle, ran.gen=rng, sim="parametric")
predIntPBoot=envelope(res)
points(MET, predIntPBoot$point[2,], type="l", col="green")
points(MET, predIntPBoot$point[1,], type="l", col="green")

#Conclusion: NOTE: This code above is wrong. The confidence bands for parametric bootstrap shold be computed separately
#The confidence bands retrieved are more smooth. However when looking at the residuals they do not seem to be normally
#distributed as assumed. Therefore a parametric bootstrap model is not preffered => choose non-parametric. 



