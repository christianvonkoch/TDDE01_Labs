##Train a neural network (NN) to learn the trigonometric sine function. To do so, sample 50 points uniformly at random
##in the interval [0, 10]. Apply the sine function to each point. The resulting pairs are the data available to you.
##Use 25 of the 50 points for training and the rest for validation. The validation set is used for early stop of the
##gradient descent. Consider threshold values i/1000 with i=1,...,10. Initialize the weights of the neural network to
##random values in the interval [-1,1]. Consider two NN architectures: A single hidden layer of 10 units, and two 
##hidden layers with 3 units each. Choose the most appropriate NN architecture and threshold value. Motivate your 
##choice. Feel free to reuse the code of the corresponding lab.
##
##Estimate the generalization error of the NN selected above (use any method of your choice).
##In the light of the results above, would you say that the more layers the better ? Motivate your answer.

RNGversion('3.5.1')
#install.packages("neuralnet")
library(neuralnet) 
set.seed(1234567890)
Var <- runif(50, 0, 10) 
trva <- data.frame(Var, Sin=sin(Var))
train <- trva[1:25,] # Training 
valid <- trva[26:50,] # Validation
n = dim(valid)[1]

# Random initialization of the weights in the interval [-1, 1] for model with 1 hidden layer
winit <- runif(31, -1, 1)
trainScore = rep(0,10)
validScore = rep(0,10)
for(i in 1:10) {
  nn_temp <- neuralnet(Sin~Var, data=train, hidden=10, threshold=i/1000, startweights=winit)
  nn = as.data.frame(nn_temp$net.result)
  pred=predict(nn_temp, newdata=valid)
  trainScore[i] = 1/n*sum((nn[,1]-train$Sin)^2)
  validScore[i] = 1/n*sum((pred-valid$Sin)^2)
}

#Random initialization of the weights in the interval [-1, 1] for model with two hidden layers
winit2=runif(22,-1,1)
trainScore2=rep(0,10)
validScore2=rep(0,10)
#R could not perform neuralnet analysis with thresholds smaller than 7/10. That is why the loop starts at 7.
for(i in 7:10) {
  nn_temp2 <- neuralnet(Sin~Var, data=train, hidden=c(3,3), threshold=i/1000, startweights=winit2)
  nn2 = as.data.frame(nn_temp2$net.result)
  pred2=predict(nn_temp2, newdata=valid)
  trainScore2[i] = 1/n*sum((nn2[,1]-train$Sin)^2)
  validScore2[i] = 1/n*sum((pred2-valid$Sin)^2)
}

plot(1:10, validScore[1:10], type="b", col="red", xlab="Threshold index", ylab="MSE")
plot(7:10, validScore2[7:10], type="b", col="blue", xlab="Threshold index", ylab="MSE")
min1=min(validScore[1:10])
min2=min(validScore2[7:10])
finalModel=ifelse(min1<min2, "1", "2")
optimal_i=ifelse(finalModel == '1', which(validScore[1:10] == min1, which(validScore2[7:10] == min2)))
print(finalModel)
print(optimal_i)

##Answer: The most appropriate model is using a one layer architecture with 10 units and using a threshold index of 
##4/1000. This is because this model yields the lowest MSE when applied to the validation data. 

#Generating new data for testing.

Var = runif(50, 0, 10)
test = data.frame(Var, Sin=sin(Var))
n=dim(test)[1]
winit = runif(31, -1, 1)
finalModel = neuralnet(Sin~Var, data=trva, hidden=10, threshold=4/1000, startweights=winit)
results=as.data.frame(finalModel$net.result)
pred = predict(finalModel, newdata = test)
generror = 1/n*sum((pred-test$Sin)^2)
print(generror)
plot(prediction(finalModel)$rep1) 
points(test, col = "red")