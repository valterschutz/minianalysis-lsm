library(chron)
library(ggplot2)
library(GGally)

bikedata = read.csv("bikesharing.csv")
bikedata$timestamp <- as.chron(bikedata$timestamp)
bikedata$season = factor(bikedata$season, levels=c(0,1,2,3), labels=c("Spring","Summer","Fall","Winter"))
bikedata$weather_code = factor(bikedata$weather_code, levels=c(1,2,3,4,7,10,26,94), labels=c("Clear","Scattered clouds","Broken clouds","Cloudy","Rain","Rain with thunderstorm","Snowfall","Freezing fog"))
bikedata$is_holiday_or_weekend = factor(bikedata$is_holiday | bikedata$is_weekend, levels=c(FALSE,TRUE), labels=c("Not holiday/weekend","Holiday/weekend"))

ggplot(data = bikedata, aes(x = season, y = cnt)) +
  geom_violin()
ggplot(data = bikedata, aes(x = is_holiday_or_weekend, y = cnt)) +
  geom_violin()
#ggpairs(bikedata)
