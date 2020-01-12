#1: Read data and plot Moisture vs Protein
Dataframe=read.csv2("tecator_csv.csv")
n = length(Dataframe[,1])
print(Dataframe)
moisture = Dataframe$Moisture
protein = Dataframe$Protein
fat = Dataframe$Fat
plot(moisture, protein, type="p", ylab="Protein", xlab="Moisture", col="red")

#Conclusion: Looks like a linear relation so a linear regression model is appropriate

#2: Consider model Mi in  which Moisture is normally distributed, and the expected Moisture is a polynomial function
#of Protein including the polynomial terms up to power of i (i.e. M1 is a linear model, M2 is a quadratic model and
#so on). Report a probabilistic model that describes Mi. Why is it appropriate to use MSE criterion when fitting 
#this model to a training data?

#Conclusion: A probabilistic model describing M(i) is: M(i) = w0 + w1 * X + w2 * X2 + ... + wi * Xi (3) The MSE
#criterion is a suitable method since it punishes outliers to a larger extent. This creates a better fitted model
#compared to when you punish the absolute value. This reduces the risk of an overfitted model.

#3: Divide the data into training and validation sets (50%/50%) and fit models Mi, i=1,...,6.  For each model, record
#the training and the validation MSE and present a plot showing how training and validation MSE depend on i (write
#some R code to make this plot). Which model is best according to the plot? How do the MSE values change and why?
#Interpret this picture in terms of bias-variance tradeoff.

colno_protein = which(colnames(Dataframe)=="Protein")
colno_moisture = which(colnames(Dataframe)=="Moisture")
moisture_protein = Dataframe[colno_protein:colno_moisture]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=Dataframe[id,]
test=Dataframe[-id,]
moisture_train = train$Moisture
moisture_test = test$Moisture

#Create function for fitting linear regression models
fit_moisture_model = function(x) {
  return(lm(formula = Moisture ~ poly(Protein, degree=x), data=train))
}

#Create predict function for training set
predict_train = function(model){
  return(predict(model, newdata = train))
}

#Create predict function for test set
predict_test = function(model){
  return(predict(model, newdata = test))
}

#Create function for calculating MSE, input parameters are vectors containing original data and predicted data
calcMSE = function(y, yhat){
  return(sum((y-yhat)^2)/length(y))
}

#Create models and predictions and store MSE values in a vector for training and test data
vector_train = c()
vector_test = c()
for (i in 1:6){
  fit = fit_moisture_model(i)
  predicted_train = predict_train(fit)
  predicted_test = predict_test(fit)
  vector_train[i] = calcMSE(moisture_train, predicted_train)
  vector_test[i] = calcMSE(moisture_test, predicted_test)
}

#Create numeric vector 1 through 6
models = c(1:6)

#Plot MSE for each model
plot(models, vector_train, col="blue", xlab="Model", ylab="MSE")
par(new=TRUE)
plot(models, vector_test, col="red", xlab="Model", ylab="MSE")

#Print MSE values
print(vector_train)
print(vector_test)

#Conclusion: Shown in the graphs we can see that for the tested values, M(3) has the lowest MSE and therefore being
#the model with the smallest error. In terms of bias, what we can see is that in the training model the predicted
#error, and MSE, is descending for a more complex model, because bias is defined by the ability for a model to fit
#data. This gives us an overfitted model, where we can see that the variance increases the more complex models
#because the MSE increases for the tested data and the MSE decreases for the training data. Variance is defined by
#the difference in predictions on different data sets.

#Use the entire data set in the following computations:

#4: Fat response and Channel1-100 are predictors. Use stepAIC. How many variables were selected?

#Fetch package stepAIC
#install.packages("MASS")
library("MASS")

#Perform stepAIC on fat
colno_fat = which(colnames(Dataframe)=="Fat")
channel_values = Dataframe[1:(colno_fat-1)]
fit_fat = lm(fat~., data = channel_values)
step = stepAIC(fit_fat, direction="both")
step$anova
summary(step)

#Conclusion: When we use StepAIC in total 63 variables were selected. These were chosen because not all variables are
#needed to predict a dependent variable.

#5: Fit a Ridge regression model with same predictor and response variables. Plot model coefficient depend on the log
#of the penalty factor lambda and report how the coefficients change with lambda. 

#install.packages("glmnet")
library("glmnet")
covariates = scale(Dataframe[,2:(colno_fat-1)])
response = scale(Dataframe[,colno_fat])
ridge_model = glmnet(as.matrix(covariates), response, alpha=0, family="gaussian")
plot(ridge_model, xvar="lambda", label=TRUE)

#Conclusion: Coefficients goes towards 0 when lambda goes towards infinity

#6: Repeat last step but with LASSO instead of Ridge. Differences?

lasso_model = glmnet(as.matrix(covariates), response, alpha=1, family="gaussian")
plot(lasso_model, xvar="lambda", label=TRUE)

#Conclusion 5 and 6: The graphs below shows us that when we increase lambda fewer variables are selected to the
#models, since the coefficients goes towards zero. In the Lasso model, the penalty is the absolute value, and in the
#Ridge model the penalty is squared (penalizes large values more). 

#7: Choose the best model by cross validation for LASSO model. Report optimal lambda and how many variables that
#chosen by the model and make conclusions. Present also a plot showing the dependence of the CV score and comment how
#the CV score changes with lambda. 

lasso_model_optimal = cv.glmnet(as.matrix(covariates), response, alpha=1, family="gaussian", lambda=seq(0,1,0.001))
lasso_model_optimal$lambda.min
plot(lasso_model_optimal)
coef(lasso_model_optimal, s="lambda.min")
print(lasso_model_optimal$lambda.min)

#Conclusion: When the lambda increases in value, the MSE seems to strictly increase. From this model, we can make
#the conclusion that the optimal lambda = 0. This means, that all variables should be included for a better
#predicted model. Compared to the results from stepAIC, all variables where chosen since lambda = 0 instead of 63
#that were chosen.



