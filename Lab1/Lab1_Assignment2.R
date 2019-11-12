#Read data and divide into test and train sets
Dataframe=read.csv2("machines_csv.csv")

#Compute a function for calculating the maximum likelihood of a function
maxlikelihood=function(theta, x){
  n = length(x[,1])
  return(n*log(theta)-theta*sum(x))
}

#Plot curve for different theta values
theta_curve = curve(-maxlikelihood(x, Dataframe), from=min(Dataframe), to=max(Dataframe))

#Find maximum likelihood value of theta
theta_max = function(x){
  n=length(x[,1])
  return(n/sum(x))
}

#Find maxtheta
max_theta = theta_max(Dataframe)
print(max_theta)

#New vector with first 6 values
y = matrix(Dataframe[1:6,1], nrow=length(Dataframe[1:6,1]), ncol=1)
print(y)

#Plot new curve on top of each other
curve(-maxlikelihood(x, Dataframe), from=-1, to=20, add=FALSE, col="red", ylim=c(0,100))
curve(-maxlikelihood(x, y), from=-1, to=20, add=TRUE, col="blue", ylim=c(0,100))
