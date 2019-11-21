#Read data and plot Moisture vs Protein
Dataframe=read.csv2("tecator_csv.csv")
n = length(Dataframe[,1])
print(Dataframe)
moisture = Dataframe$Moisture
protein = Dataframe$Protein
plot(moisture, protein, type="p", ylab="Protein", xlab="Moisture", col="red")

#Conclusion: Looks like a linear relation so a linear regression model is appropriate

#Divide data into training and validation sets
colno_protein = which(colnames(Dataframe)=="Protein")
colno_moisture = which(colnames(Dataframe)=="Moisture")
moisture_protein = Dataframe[colno_protein:colno_moisture]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=Dataframe[id,]
test=Dataframe[-id,]

#Create function for fitting linear regression models
fit_moisture_model = function(x) {
  return(lm(formula = Moisture ~ poly(Protein, degree=x), data=train))
}

#Create function for calculating MSE
calcMSE = function(fit){
  return(sum(fit$residuals^2)/length(fit$residuals))
}

#Create models for i = 1..6
fit1 = fit_moisture_model(1)
fit2 = fit_moisture_model(2)
fit3 = fit_moisture_model(3)
fit4 = fit_moisture_model(4)
fit5 = fit_moisture_model(5)
fit6 = fit_moisture_model(6)

print(calcMSE(fit1))


