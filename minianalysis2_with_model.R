library(chron)
library(ggplot2)
library(GGally)

bd = read.csv("bikesharing.csv")
bd$timestamp <- as.chron(bd$timestamp)
bd$hrs <- hours(bd$timestamp)
bd$hrs_cat <- factor(bd$hrs)
is_rushhour <- (bd$hrs >= 6 & bd$hrs <= 9) | (bd$hrs >= 16 & bd$hrs <= 19)
bd$is_rushhour <- factor(is_rushhour, levels=c(FALSE, TRUE), labels=c("No", "Yes"))
bd$season <- factor(bd$season, levels=c(0,1,2,3), labels=c("Spring","Summer","Fall","Winter"))
bd$weather_code <- factor(bd$weather_code, levels=c(1,2,3,4,7,10,26,94), labels=c("Clear","Scattered clouds","Broken clouds","Cloudy","Rain","Rain with thunderstorm","Snowfall","Freezing fog"))
levels(bd$weather_code)[levels(bd$weather_code)%in%c("Cloudy", "Clear", "Broken clouds", "Scattered clouds")] <- "No Precipitation"
levels(bd$weather_code)[levels(bd$weather_code)%in%c("Rain", "Snowfall", "Rain with thunderstorm", "Freezing fog")] <- "Precipitation"
workday <- bd$is_holiday | bd$is_weekend
bd$workday <- factor(workday, levels=c(FALSE,TRUE), labels=c("Yes","No"))
levels(bd$hrs_cat)[levels(bd$hrs_cat)%in%c("23", "0", "1", "2", "3", "4", "5")] <- "Night"
levels(bd$hrs_cat)[levels(bd$hrs_cat)%in%c("6", "7", "8", "9")] <- "Morning"
levels(bd$hrs_cat)[levels(bd$hrs_cat)%in%c("10", "11", "12", "13")] <- "Forenoon"
levels(bd$hrs_cat)[levels(bd$hrs_cat)%in%c("14", "15", "16", "17")] <- "Afternoon"
levels(bd$hrs_cat)[levels(bd$hrs_cat)%in%c("18", "19", "20", "21", "22")] <- "Evening"
levels(bd$weather_code)[levels(bd$weather_code)%in%c("Cloudy", "Clear", "Broken clouds", "Scattered clouds")] <- "No Precipitation"
levels(bd$weather_code)[levels(bd$weather_code)%in%c("Rain", "Snowfall", "Rain with thunderstorm", "Freezing fog")] <- "Precipitation"
bd$hrs_cat <- relevel(bd$hrs_cat, ref="Night")
bd$season <- relevel(bd$season, ref="Winter")
bd$weather_code <- relevel(bd$weather_code, ref="Clear")
bd$workday <- relevel(bd$workday, ref="No")

# Violin plots
ggplot(data = bd, aes(x = season, y = cnt)) +
  geom_violin()
ggplot(data = bd, aes(x = workday, y = cnt)) +
  geom_violin()

# See that t1 and t2 are linearly dependent
#ggpairs(bd)

# During rushhours
ggplot(bd[is_rushhour,], aes(x = t1, y = cnt, color = hrs_cat)) +
  geom_point(alpha=0.2) +
  facet_grid(rows = vars(workday), cols = vars(season))

# During non-rushhours
ggplot(bd[!is_rushhour,], aes(x = t1, y = cnt, color = hrs_cat)) +
  geom_point(alpha=0.2) +
  facet_grid(rows = vars(workday), cols = vars(season))

#Modell som tar hänsyn till att (temperatur, säsong) samt (tid, arbetsdag eller ej) korrelerar
model <- lm(bd$cnt ~ bd$hum  +  bd$wind_speed  +  bd$t1*bd$season  +  bd$hrs_cat*bd$workday  +  bd$weather_code)
summary(model)
#Modell som EJ tar hänsyn till att (temperatur, säsong) samt (tid, arbetsdag eller ej) korrelerar
model <- lm(bd$cnt ~ bd$hum  +  bd$wind_speed  +  bd$t1+bd$season  +  bd$hrs_cat+bd$workday  +  bd$weather_code)
summary(model)
#ggplot(bd, aes(x = t1, y = cnt, color = hrs_cat)) +
#  geom_point(alpha=0.2) +
#  facet_grid(rows = vars(workday), cols = vars(season))

