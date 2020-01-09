RNGversion('3.5.1')
#Read data 
set.seed(12345)
Dataframe=read.csv2("State.csv")
Dataframe=Dataframe[order(Dataframe$MET),]
MET=Dataframe$MET
EX=Dataframe$EX

plot(EX, MET, xlab="EX", ylab="MET", type="p", main="Plot of EX vs MET")

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
plot(EX, MET, xlab="EX", ylab="MET", type="p", col="red", main="Plot original vs predicted data")
points(predData, MET, col="blue")
summaryfit=summary(bestTree)
residuals=(EX-predData)^2
hist(summaryfit$residuals)

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
plot(EX, MET, xlab="EX", ylab="MET", pch=21, bg="orange", main="Plot original vs predicted data")
points(predData, MET, type="l", col="blue")
points(confIntNPBoot$point[2,], MET, type="l")
points(confIntNPBoot$point[1,], MET, type="l")

mle=tree(EX~MET, data=Dataframe, control=tree.control(48, mincut=8))
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
  #predictvaluesfor all EX values from the original data
  predData=predict(prunedtree,newdata=Dataframe) 
  return(predData)
}
res=boot(Dataframe, statistic=f1, R=1000, mle=mle, ran.gen=rng, sim="parametric")
predIntPBoot=envelope(res)
points(predIntPBoot$point[2,], MET, type="l", col="green")
points(predIntPBoot$point[1,], MET, type="l", col="green")



