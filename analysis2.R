library(chron)
library(ggplot2)
library(GGally)

bikedata = read.csv("bikesharing.csv")
bikedata$timestamp <- as.chron(bikedata$timestamp)
bikedata$hrs <- hours(bikedata$timestamp)
bikedata$hrs_cat <- factor(bikedata$hrs)
is_rushhour <- (bikedata$hrs >= 6 & bikedata$hrs <= 9) | (bikedata$hrs >= 16 & bikedata$hrs <= 19)
bikedata$is_rushhour <- factor(is_rushhour, levels=c(FALSE, TRUE), labels=c("Not rushhour", "Rushhour"))
bikedata$season <- factor(bikedata$season, levels=c(0,1,2,3), labels=c("Spring","Summer","Fall","Winter"))
bikedata$weather_code <- factor(bikedata$weather_code, levels=c(1,2,3,4,7,10,26,94), labels=c("Clear","Scattered clouds","Broken clouds","Cloudy","Rain","Rain with thunderstorm","Snowfall","Freezing fog"))
is_holiday_or_weekend <- bikedata$is_holiday | bikedata$is_weekend
bikedata$is_holiday_or_weekend <- factor(is_holiday_or_weekend, levels=c(FALSE,TRUE), labels=c("Not holiday/weekend","Holiday/weekend"))

# Violin plots
ggplot(data = bikedata, aes(x = season, y = cnt)) +
  geom_violin()
ggplot(data = bikedata, aes(x = is_holiday_or_weekend, y = cnt)) +
  geom_violin()

# See that t1 and t2 are linearly dependent
ggpairs(bikedata)

# During rushhours
ggplot(bikedata[is_rushhour,], aes(x = t1, y = cnt, color = hrs_cat)) +
  geom_point(alpha=0.2) +
  facet_grid(rows = vars(is_holiday_or_weekend), cols = vars(season))

# During non-rushhours
ggplot(bikedata[!is_rushhour,], aes(x = t1, y = cnt, color = hrs_cat)) +
  geom_point(alpha=0.2) +
  facet_grid(rows = vars(is_holiday_or_weekend), cols = vars(season))

# TODO:
# Greedy backward search on final model, either manually or using step(data, direction="backward")
# Split into training/testing, calculate pMSE.
# Interpretation of coefficients.