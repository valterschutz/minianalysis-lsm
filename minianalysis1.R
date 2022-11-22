bikedata = read.csv("bikesharing.csv")
library(chron)
bikedata$hours <- hours(as.chron(bikedata$timestamp))
#transform bikedata!
#b <- boxcox(lm((bikedata$cnt+1)~1))
#lambda <- b$x[which.max(b$y)]
#bikedata$cnt <- (bikedata$cnt ^ lambda - 1) / lambda

par(cex.main=1.8, cex.axis=1.3, cex.lab=1.5, pch=16, col="black", col.main=rgb(red=.1, green=.5, blue=.5, alpha=0.01))
hist(bikedata$cnt, xlab="bikes per hour", main="Histogram of bikes per hour")
boxplot(bikedata$cnt~bikedata$season, xlab="Season", ylab="Rented bikes per hour", xaxt='n'); axis(1, at = c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter"))
vioplot(bikedata$cnt~bikedata$season, xlab="Season", ylab="Rented bikes per hour", xaxt='n'); axis(1, at = c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter"))
#pairs(bikedata[,c(2,3,5,6)], col=rgb(red=.1, green=.5, blue=.5, alpha=0.01), gap=0)
#pairs(bikedata[, c(2,3,5,6)])
hours = substr(bikedata$timestamp, 12, 13)
par(col=rgb(red=.1, green=.5, blue=.5, alpha=.2))
#create models, check Q-Q-plots
t1m = lm(bikedata$cnt~bikedata$t1)
qqnorm(t1m$residuals)
qqline(t1m$residuals)

humm = lm(log(bikedata$cnt+1)~bikedata$hum)
qqnorm(humm$residuals)
qqline(humm$residuals)

windm = lm(bikedata$cnt~bikedata$wind_speed)
qqnorm(windm$residuals)
qqline(windm$residuals)
#plot results
boxplot(bikedata$cnt~hours, ylab="Rented bikes per hour", xlab="Hour during the day")

par(col=rgb(red=.1, green=.5, blue=.5, alpha=.2), lwd=3)
plot(bikedata$cnt~bikedata[,3], ylab="Rented bikes per hour", xlab="Temperature [C]")
abline(t1m, col=rgb(.2,.2,.2))
plot(bikedata$cnt~bikedata[,5], ylab="Rented bikes per hour", xlab="Humidity [%]")
abline(humm, col=rgb(.2,.2,.2))
plot(bikedata$cnt~bikedata[,6], ylab="Rented bikes per hour", xlab="Wind speed [km/h]")
abline(windm, col=rgb(.2,.2,.2))

par(col=rgb(red=.5, green=.1, blue=.1, alpha=.1))
plot(bikedata$t1,t1m$residuals, ylab="Residual [Rented bikes per hour]", xlab="Temperature [C]")
plot(bikedata$hum,humm$residuals, ylab="Residual [Rented bikes per hour]", xlab="Humidity [%]")
plot(bikedata$wind_speed,windm$residuals, ylab="Residual [Rented bikes per hour]", xlab="Wind speed [km/h]")

hist(bikedata$cnt)
library(MASS)
dev.off()
par(mfrow=c(4, 6))
betas_weekend = rep(0,24)
betas_weekday = rep(0,24)
for (index in 1:24) {
    hour = index - 1
    selected_bikedata <- bikedata[bikedata$is_holiday==0 
                                  & bikedata$is_weekend==1
                                  & bikedata$hours==hour,
                                  ]
    model = lm(selected_bikedata$cnt~selected_bikedata$hum)
    betas_weekend[index] <- model$coefficients[2]/mean(selected_bikedata$cnt)
    selected_bikedata <- bikedata[bikedata$is_holiday==0 
                                  & bikedata$is_weekend==0
                                  & bikedata$hours==hour,
                                  ]
    model = lm(selected_bikedata$cnt~selected_bikedata$hum)
    betas_weekday[index] <- model$coefficients[2]/mean(selected_bikedata$cnt)
    #print(selected_bikedata$hours[1]
    #print(model$coefficients)
    #par(col=rgb(red=.1, green=.5, blue=.5, alpha=.2), lwd=3, mai=c(.2, 0.05, 0.05, 0.05))
    #plot(selected_bikedata$cnt~selected_bikedata$t1, xlab=hour, ylim = c(0, 5000), ylab="", xaxt="none", yaxt="none")
    #abline(model, col=rgb(.5, .5, .5))
    #title(xlab=hour, line=0.6, cex.lab=1.5)
}
plot(0:23, betas_weekend, col='red', pch=16, cex=2, ylab="Slope", xlab="Hour of day", ylim=c(-.10, .130))
points(0:23, betas_weekday, col='green', pch=16, cex=2)
legend(0,.126, legend=c("weekends", "weekdays"), col=c('red', 'green'), pch=c(16, 16), cex=1.5)
selected_bikedata <- bikedata[bikedata$is_holiday==0 
                              & bikedata$is_weekend==1,
                              ]
par(mfrow=c(1, 1))
selected_bikedata$hours = substr(selected_bikedata$timestamp, 12, 13)
boxplot(selected_bikedata$cnt~selected_bikedata$hours, xlab="Hours", ylab="Bike shares", ylim=c(0, 8000))

  #b <- boxcox(lm((selected_bikedata$cnt+1)~1))
#lambda <- b$x[which.max(b$y)]
#new_bikedata <- (selected_bikedata$cnt ^ lambda - 1) / lambda
new_bikedata <- selected_bikedata$cnt
plot(new_bikedata~selected_bikedata$t2)

