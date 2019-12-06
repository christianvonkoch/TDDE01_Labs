#1: Read data

data=read.csv2("NIRspectra.csv")
n=dim(data)[1] 
pcaAnalysis=prcomp(data)
lambda=pcaAnalysis$sdev^2
#Eigenvalues
print(lambda)
#Proportion of variation
propVar= lambda/sum(lambda)*100
screeplot(pcaAnalysis)
print(propVar)
noOfVars=1
sumOfVariation=propVar[i]
while(sumOfVariation<99){
  noOfVars=noOfVars+1
  sumOfVariation=sumOfVariation+propVar[i]
}
#Print number of variables used
print(i)
#Print PC1 and PC2 in plot
U=pcaAnalysis$rotation
head(U)

