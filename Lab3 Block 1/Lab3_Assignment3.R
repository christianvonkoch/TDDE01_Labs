## Assignment3:
## Train a neural network to learn the trigonometric sine function. To do so, sample 50 points
## uniformly at random in the interval [0,10]. Apply the sine function to each point. The resulting
## pairs are the data available to you. Use 25 of the 50 points for training and the rest for validation. 
## The validation set is used for early stop of the gradient descent. That is, you should
## use the validation set to detect when to stop the gradient descent and so avoid overfitting.
## Stop the gradient descent when the partial derivatives of the error function are below a given
## threshold value. Check the argument threshold in the documentation.Consider threshold
## values i/1000 with i = 1,...,10. Initialize the weights of the neural network to random values in
## the interval [-1, 1].  Use a neural network with a single hidden layer of 10 units. Use the default values
## for the arguments not mentioned here. Choose the most appropriate value for
## the threshold. Motivate your choice. Provide the final neural network learned with the chosen
## threshold. Feel free to use the following template.

RNGversion('3.5.1')
#install.packages("neuralnet")
library(neuralnet) 
set.seed(1234567890)
Var <- runif(50, 0, 10) 
trva <- data.frame(Var, Sin=sin(Var))
train <- trva[1:25,] # Training 
valid <- trva[26:50,] # Validation
n = dim(valid)[1]
# Random initialization of the weights in the interval [-1, 1] 
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
plot(1:10, trainScore[1:10], type="b", col="red", xlab="Threshold index", ylab="MSE")
points(1:10, validScore[1:10], type="b", col="blue")
min_error=min(validScore[1:10])
print(min_error)
optimal_i=which(validScore[1:10] == min_error)
print(optimal_i)

##Conclusion: As seen in the graph, naturally the train data performs the best when the threshold value is as small as
##possible, i.e. 1/1000, and the performance decreases as this threshold increases for the train data. From the graph
##we can see that the threshold value 4/1000 performs the best on the validation data (since it results in the lowest
##MSE) and therefore this threshold will be used moving forward in the assignment. 

optimal_nn = neuralnet(Sin~Var, data=train, hidden=10, threshold=optimal_i/1000, startweights=winit)
plot(optimal_nn)
# Plot of the predictions (black dots) and the data (red dots) 
plot(prediction(optimal_nn)$rep1) 
points(trva, col = "red")

##Conclusion: The optimal neural network with threshold 4/1000 is chosen which results in the neural network shown
##above. The last two graphs briefly show how similar the predicted values from the model are in comparison to the real
##sinus curve. One can conclude that the neural network created resembles the shape of the sinus curve with quite a 
##precision. 
