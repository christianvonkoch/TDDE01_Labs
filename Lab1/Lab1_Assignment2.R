#1: Import data
Dataframe=read.csv2("machines_csv.csv")

#2: Assume probability model p(x|theta) = theta*e^(-theta*x) for x = Length in which observations are independent
#and identically distributed. What is the distribution type of x. Write a function that computes the log-likelihood
#log p(x|theta) for a given theta and a given data vector x. Plot the curve showing the dependence of log-likelihood
#on theta where the entire data is used for fitting. What is the maximum likelihood value of theta according to plot?

#Compute a function for calculating the maximum likelihood of a function
loglikelihood=function(theta, x){
  n = length(x[,1])
  return(n*log(theta)-theta*sum(x))
}

#Plot curve for different theta values
theta_curve = curve(-loglikelihood(x, Dataframe), xlab="Theta", from=min(Dataframe), to=max(Dataframe))

#Find maximum likelihood value of theta
theta_max = function(x){
  n=length(x[,1])
  return(n/sum(x))
}

#Find maxtheta
max_theta = theta_max(Dataframe)
print(max_theta)

#Conclusion: We can see from the probabilistic model that the distribution is of type exponential. The maximum
##likelihood value of theta is: 42.29453 The optimal theta for is: 1.126217

#3: Repeat step 2 but use only 6 first observations from the data, and put the two log-likelihood curves
#(from step 2 and 3) in the same plot. What can you say about reliability of the maximum likelihood solution in
#each case?

#New vector with first 6 values
y = matrix(Dataframe[1:6,1], nrow=length(Dataframe[1:6,1]), ncol=1)
print(y)

#Plot new curve on top of each other
curve(-loglikelihood(x, Dataframe), xlab="Theta", from=0, to=20, add=FALSE, col="red", ylim=c(0,100))
curve(-loglikelihood(x, y), xlab="Theta", from=0, to=20, add=TRUE, col="blue", ylim=c(0,100))

#Conclusion: The graph is increasing at a much slower pace when only using the first six values compared with to the
#graph when we use all data. The model with more data is more reliable since there is a more certain min-value from
#the graph whereas the one with only six values, the theta value could be anything from the minimum value and forward.

#4: Assume now a Bayesian model with p(x|theta)=theta*e^(-theta*x) and a prior p(theta)=lambda*e^(-lambda*x), lambda=10
#Write a function computing l(theta)=log(p(x|theta)*p(theta)). What kind of measure is actually computed by this
#function? Plot the curve showing the dependence of l(theta) on theta computed using the entire data and overlay it
#with a plot from step 2. Find an optimal theta and compare your result with the previous findings. 

#Compute a function for calculating the likelihood of the bayesian function
bayesian_likelihood=function(theta, lambda, x){
  n = length(x[,1])
  return(n*log(theta)-theta*sum(x)-lambda*theta)
}

#Find maximum likelihood value of theta
bayesian_theta_max = function(lambda, x){
  n=length(x[,1])
  return(n/(sum(x)+lambda))
}

#Find maxtheta
bayesian_max_theta = bayesian_theta_max(10, Dataframe)
print(bayesian_max_theta)

#Plot new curve on top of each other
curve(-bayesian_likelihood(x, 10, Dataframe), ylab="-Loglikelihood", xlab="Theta", from=0, to=10, add=FALSE, col="red",
      ylim=c(20,300))
curve(-loglikelihood(x, Dataframe), ylab="-Loglikelihood", xlab="Theta", from=0, to=10, add=TRUE, col="blue",
      ylim=c(20,300))

#Conclusion: When using an bayesian model we have a prior that gives the model information beforehand which helps
#fitting the model. The optimal theta is now 0.91 which is close to the datasets meanvalue, which makes sense that
#this model gives a better predicted value.

#5: Use theta value found in step 2 and generate 50 new observations from p(x|theta)=theta*e^(-theta*x) (use standard
#number generators). Create the histograms of the original and the new data and make conclusions. 

#Generate 50 new observation using theta value from step 2
set.seed(12345)
newdata = rexp(50, rate = max_theta)
print(newdata)

#Plot new data and old data in histogram
olddata = Dataframe$Length
print(olddata)
hist(newdata)
hist(olddata)

#Conclusion: The histogram shows us that the distribution is fairly similar between the actual and predicted data. 
#This concludes model was accurately fitted to the correct distribution model.

