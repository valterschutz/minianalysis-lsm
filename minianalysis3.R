# TODO:
# Use leaps and regsubsets to calculate all possible models, calculate pMSE for each one.
# Plot y_test against y_pred against y_test for a good model and a bad model.
#   Add a line with slope 1.

library(leaps)
library(chron)
library(ggplot2)
library(GGally)

bd = read.csv("bikesharing.csv")
bd$timestamp <- as.chron(bd$timestamp)
bd$hrs <- hours(bd$timestamp)

# Split data into training and testing
y = bd$cnt
N = length(y)
testratio = 0.5
Ntest = floor(N*testratio)
allindices = seq(1,N)
testindices = sort(sample(allindices, Ntest))
trainindices = allindices[!allindices %in% testindices]
ytest = y[testindices]

# Training "bad" model
model1 <- lm(bd$cnt[trainindices] ~ bd$hum[trainindices] + bd$wind_speed[trainindices] + bd$t1[trainindices], x=TRUE)
summary(model1)
# Prediction for "bad" model 1
Xtest = cbind(rep(1,Ntest),as.matrix(bd[testindices, c(3,5,6)]))
coeffs = as.matrix(as.vector(model1$coefficients))
ypred = Xtest%*%coeffs
plot(ypred,ytest)

# Training "good" model
model4 <- lm(bd$cnt ~ bd$hum + bd$wind_speed + bd$t1)
summary(model4)
# Prediction for "good" model
Xtest = cbind(rep(1,Ntest),as.matrix(bd[testindices, c(3,5,6)]))
coeffs = as.matrix(as.vector(model1$coefficients))
ypred = Xtest%*%coeffs
plot(ypred,ytest)