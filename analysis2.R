library(chron)
library(ggplot2)
library(GGally)

bd = read.csv("bikesharing.csv")
bd$timestamp <- as.chron(bd$timestamp)
bd$hrs <- hours(bd$timestamp)
bd$hrs_cat <- factor(bd$hrs)
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

# Violin plots
ggplot(data = bd, aes(x = season, y = cnt)) +
  geom_violin()
ggplot(data = bd, aes(x = is_holiday_or_weekend, y = cnt)) +
  geom_violin()

# See that t1 and t2 are linearly dependent
#ggpairs(bd)

# If split into time of day, not hour
ggplot(bd, aes(x = t1, y = cnt, color = time_of_day)) +
  geom_point(alpha=0.2) +
  guides(colour = guide_legend(override.aes = list(alpha=1))) +
  facet_grid(rows = vars(workday), cols = vars(season)) +
  ylim(0,6000)

# If split into hour
ggplot(bd, aes(x = t1, y = cnt, color = hrs_cat)) +
  geom_point(alpha=0.2) +
  guides(colour = guide_legend(override.aes = list(alpha=1))) +
  facet_grid(rows = vars(workday), cols = vars(season)) +
  ylim(0,6000)

# If split into combo
ggplot(bd, aes(x = t1, y = cnt, color = time)) +
  geom_point(alpha=0.2) +
  guides(colour = guide_legend(override.aes = list(alpha=1))) +
  facet_grid(rows = vars(workday), cols = vars(season)) +
  ylim(0,6000)

# Initial model
model1 <- lm(bd$cnt ~ bd$hum   +  bd$wind_speed  +  bd$t1 + bd$season  +  bd$hrs_cat + bd$workday  +  bd$weather_code)
summary(model1)

# Simplify stuff
bd$mod_weather_code <- bd$weather_code
levels(bd$mod_weather_code)[levels(bd$mod_weather_code)%in%c("Cloudy", "Clear", "Broken clouds", "Scattered clouds")] <- "No precipitation"

# Second model
model2 <- lm(bd$cnt ~ bd$hum   +  bd$wind_speed  +  bd$t1 + bd$season  +  bd$time + bd$workday  +  bd$mod_weather_code)
summary(model2)

# Third model
model3 <- lm(bd$cnt ~ bd$hum   +  bd$t1  +  bd$time + bd$workday  +  bd$mod_weather_code)
summary(model3)

# Fourth model
model4 <- lm(bd$cnt ~ bd$hum   +  bd$t1  +  bd$time*bd$workday  +  bd$mod_weather_code)
summary(model4)

# Comparison of RSS
c(summary(model1)$r.squared,summary(model2)$r.squared,summary(model3)$r.squared,summary(model4)$r.squared)

# TODO:
# Greedy backward search on final model, either manually or using step(data, direction="backward")
# Split into training/testing, calculate pMSE.
# Interpretation of coefficients.