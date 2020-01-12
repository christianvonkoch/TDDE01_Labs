#Read data
RNGversion('3.5.1')
Dataframe=read.csv2("bank.csv")

linear_model=glm(Visitors~., data=Dataframe, family="poisson")
linear_model$coefficients
control=exp(0.1742155+0.4017126*seq(9,12,0.1))
print(control)

##Answer: The probabilistic expression for the target is Visitors=e^(0.1742155+0.4017126*Time). The control vector 
##shows that the response variable resulted from the equation is fairly reasonable and resembles the original data.

library(boot)
rng=function(data, mle) {
  data1=data.frame(Visitors=data$Visitors, Time=data$Time)
  n=length(data$Visitors)
  #generate new Price
  data1$Visitors=rnorm(n,predict(mle, newdata=data1), sd(mle$residuals))
  return(data1)
}

f1=function(data1){
  res=lm(Visitors~., data=data1) #fit linearmodel
  #predictvaluesfor all Visitor valuesfrom the original data
  Visitors=predict(res,newdata=data.frame(Time=seq(12,13,0.05)))
  n=length(seq(12,13,0.05))
  predictedVisitors=rnorm(n, Visitors, sd(linear_model$residuals))
  return(predictedVisitors)
}
res=boot(Dataframe, statistic=f1, R=1000, mle=linear_model, ran.gen=rng, sim="parametric")
e=envelope(res)
plot(Dataframe$Time, Dataframe$Visitors, main="Forecasting of visitors depending on time", xlab="Time",
     ylab="Visitors", xlim=c(9,13), ylim=c(30,500))
points(seq(12,13,0.05), exp(e$point[2,]), type="l", lty=21, col="gray")
points(seq(12,13,0.05), exp(e$point[1,]), type="l", lty=21, col="gray")

min_value_13=exp(e$point[2,21])
max_value_13=exp(e$point[1,21])
cat("The bank should expect between", min_value_13, "and", max_value_13, "customers", sep=" ")

##Answer: The band seems to give a correct forecasting. The bank should expect between approx 177 and 281
##customers at 13:00.

     