#install.packages("neuralnet")
library(neuralnet) 
set.seed(1234567890)
Var <- runif(50, 0, 10) 
trva <- data.frame(Var, Sin=sin(Var))
train <- trva[1:25,] # Training 
valid <- trva[26:50,] # Validation
# Random initialization of the weights in the interval [-1, 1] 
winit <- 
for(i in 1:10) {
  nn <- neuralnet()
# Your code here
}
plot(nn <- neuralnet())
# Plot of the predictions (black dots) and the data (red dots) plot(prediction(nn)$rep1) points(trva, col = "red")