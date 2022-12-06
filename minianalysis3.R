# TODO:
# Use leaps and regsubsets to calculate all possible models, calculate pMSE for each one.
# Plot y_test against y_pred against y_test for a good model and a bad model.
#   Add a line with slope 1.
# k-fold cross validation

library(leaps)
library(chron)
library(ggplot2)
library(GGally)
library(MASS)

rm(list = ls())

pmse = function(trainmodel, testmodel, ytest) {
  Xtest = testmodel$x
  coeffs = as.matrix(as.vector(trainmodel$coefficients))
  ypred = Xtest%*%coeffs
  pmse = sum((ytest-ypred)^2)/Ntest
  R2train = summary(trainmodel)$r.squared
  R2test = 1-sum((ytest-ypred)^2)/sum((ytest-mean(ytest))^2)
  
  print(ggplot() +
    geom_point(aes(x=ypred, y=ytest), alpha=0.1, color="blue") +
    geom_abline(aes(intercept=0, slope=1), color="red"))
  
  c(pmse, R2train, R2test)
}

bd <- read.csv("bikesharing.csv")
bd <- bd[bd$cnt>0,]
bd$cnt <- bd$cnt+5
bd$chrons <- as.chron(bd$timestamp)
bd$hrs <- hours(bd$chrons)
bd$weekday <- weekdays(bd$chrons)
bd$months <- months(bd$chrons)

bd$hrs_cat <- factor(bd$hrs)
bd$weekday_cat <- factor(bd$weekday)
bd$time_of_day = bd$hrs_cat
levels(bd$time_of_day)[levels(bd$time_of_day)%in%c("23", "0", "1", "2", "3", "4", "5","6")] <- "Night"
levels(bd$time_of_day)[levels(bd$time_of_day)%in%c("7", "8", "9")] <- "Morning"
levels(bd$time_of_day)[levels(bd$time_of_day)%in%c("10", "11", "12", "13")] <- "Forenoon"
levels(bd$time_of_day)[levels(bd$time_of_day)%in%c("14", "15", "16", "17")] <- "Afternoon"
levels(bd$time_of_day)[levels(bd$time_of_day)%in%c("18", "19", "20", "21", "22")] <- "Evening"
bd$time <- bd$hrs_cat
levels(bd$time)[levels(bd$time)%in%c("0", "1", "2", "3", "4", "5")] <- "Night"
bd$time_of_day <- relevel(bd$time_of_day, ref="Night")
is_rushhour <- (bd$hrs >= 6 & bd$hrs <= 9) | (bd$hrs >= 16 & bd$hrs <= 19)
bd$is_rushhour <- factor(is_rushhour, levels=c(FALSE, TRUE), labels=c("Not rushhour", "Rushhour"))
bd$season <- factor(bd$season, levels=c(0,1,2,3), labels=c("Spring","Summer","Fall","Winter"))
bd$weather_code <- factor(bd$weather_code, levels=c(1,2,3,4,7,10,26,94), labels=c("Clear","Scattered clouds","Broken clouds","Cloudy","Rain","Rain with thunderstorm","Snowfall","Freezing fog"))
workday <- !(bd$is_holiday | bd$is_weekend)
bd$workday <- factor(workday, levels=c(FALSE,TRUE), labels=c("Not workday","Workday"))
bd$hrs_cat <- relevel(bd$hrs_cat, ref="4")
bd$time_of_day <- relevel(bd$time_of_day, ref="Night")
bd$time <- relevel(bd$time, ref="Night")
bd$season <- relevel(bd$season, ref="Winter")
bd$weather_code <- relevel(bd$weather_code, ref="Clear")
bd$workday <- relevel(bd$workday, ref="Workday")

# Only keep some data
bd <- bd[bd$months %in% c("Feb","Mar","Apr"),]

# Simplify stuff
bd$mod_weather_code <- bd$weather_code
levels(bd$mod_weather_code)[levels(bd$mod_weather_code)%in%c("Cloudy", "Clear", "Broken clouds", "Scattered clouds")] <- "No precipitation"
levels(bd$mod_weather_code)[levels(bd$mod_weather_code)%in%c("Cloudy", "Clear", "Broken clouds", "Scattered clouds")] <- "No precipitation"
levels(bd$weather_code)[levels(bd$weather_code)%in%c("Rain", "Snowfall", "Rain with thunderstorm", "Freezing fog")] <- "Precipitation"

# Split data into training and testing
y = bd$cnt
N = length(y)
testratio = 0.5
Ntest = floor(N*testratio)
allindices = seq(1,N)
testindices = sort(sample(allindices, Ntest))
trainindices = allindices[!allindices %in% testindices]
bdtrain = bd[trainindices,]
bdtest = bd[testindices,]
y_test = y[testindices]
y_train = y[trainindices]

# Training "bad" model
trainmodel1 <- lm(cnt ~ t1 + hum + wind_speed + time + workday + weather_code, data=bdtrain)
testmodel1 <- lm(cnt ~ t1 + hum + wind_speed + time + workday + weather_code, data=bdtest, x=TRUE)
stats1 = pmse(trainmodel1, testmodel1, y_test)
aic1 = AIC(trainmodel1)
bic1 = BIC(trainmodel1)

# Training "good" model
# Prediction for "good" model
trainmodel2 <- lm(cnt ~ hum + t1 + time*workday, data=bdtrain)
# Prediction for "good" model
testmodel2 <- lm(cnt ~ hum + t1 + time*workday, data=bdtest, x=TRUE)
stats2 = pmse(trainmodel2, testmodel2, y_test)
bic2 = BIC(trainmodel2)
aic2 = AIC(trainmodel2)

# Overfitted model
trainmodel3 <- lm(cnt ~ hum * t1 * time*weekday_cat+ mod_weather_code, data=bdtrain)
testmodel3 <- lm(cnt ~ hum*t1*time*weekday_cat + mod_weather_code, data=bdtest, x=TRUE)
stats3 = pmse(trainmodel3, testmodel3, y_test)
aic3 = AIC(trainmodel3)
bic3 = BIC(trainmodel3)

ggplot() +
  geom_qq(aes(sample = trainmodel2$residuals), alpha=0.2) +
  geom_abline(aes(intercept = 0, slope = 200))
