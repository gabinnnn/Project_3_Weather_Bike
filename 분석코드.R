
library(dplyr)
library(ggplot2)
library(car)

data <- read.csv(choose.files(),fileEncoding="CP949")
data %>% dim()
data %>% head()
data %>% names()

data %>% sapply(class)

data$Date <- data$Date %>% as.Date("%d/%m/%Y")
data %>% sapply(class)

data %>% summary()

daily_data <- data %>%
  group_by(Date) %>%  
  summarise(Rented.Bike.Count = sum(Rented.Bike.Count),
            Temperature = mean(Temperature),
            Humidity = mean(Humidity...),
            Wind.speed = mean(Wind.speed..m.s.),
            Visibility = mean(Visibility..10m.),
            Dew.point.temperature = mean(Dew.point.temperature),
            Solar.Radiation = mean(Solar.Radiation..MJ.m2.),
            Rainfall = sum(Rainfall.mm.),
            Snowfall = sum(Snowfall..cm.),
            Seasons = first(Seasons),
            Holiday = first(Holiday),
            Functioning.Day = first(Functioning.Day))

daily_data %>% sapply(class)

boxplot(daily_data$Rented.Bike.Count, outline=TRUE)

hist(daily_data$Rented.Bike.Count)

outliers <- boxplot.stats(daily_data$Rented.Bike.Count)$out
outliers

n_missing <- colSums(is.na(daily_data))
n_missing

model <- lm(Rented.Bike.Count ~ Seasons + Holiday + Functioning.Day + Temperature + Humidity + Wind.speed, data = daily_data)

summary(model)

residuals <- resid(model)

par(mfrow=c(2,2)) 
plot(model, 1)
plot(model, 2)
plot(model, 3)
plot(model, 5)

shapiro.test(residuals(model))

durbinWatsonTest(model)

qqPlot(model, main="Normal Q-Q Plot", id.method="identify")

daily_data$log_Rented.Bike.Count <- log(daily_data$Rented.Bike.Count + min(daily_data$Rented.Bike.Count[daily_data$Rented.Bike.Count > 0]) - 1)

daily_data <- daily_data[complete.cases(daily_data),]

log_model <- lm(log_Rented.Bike.Count ~ ., data=daily_data)

par(mfrow=c(2,2)) 
plot(log_model, 1)
plot(log_model, 2)
plot(log_model, 3)
plot(log_model, 5)

shapiro.test(residuals(log_model))

daily_data$Rented.Bike.Count <- daily_data$Rented.Bike.Count

daily_data <- daily_data[, !(names(daily_data) %in% c("log_Rented.Bike.Count"))]

daily_data$sqrt_Rented.Bike.Count <- sqrt(daily_data$Rented.Bike.Count)

daily_data <- daily_data[complete.cases(daily_data),]

sqrt_model <- lm(sqrt_Rented.Bike.Count ~ ., data = daily_data)
summary(sqrt_model)

shapiro.test(residuals(sqrt_model))

plot(sqrt_model, 1)
plot(sqrt_model, 2)
plot(sqrt_model, 3)
plot(sqrt_model, 5)

daily_data <- daily_data[, !(names(daily_data) %in% c("sqrt_Rented.Bike.Count"))]

vif(model)

vif(log_model)

vif(sqrt_model)


daily_data <- daily_data[, !(names(daily_data) %in% c("Seasons"))]

library(MASS)

full_model <- lm(Rented.Bike.Count ~ ., data = daily_data)
step_model <- stepAIC(full_model, direction = "both")
summary(step_model)

rsq <- summary(step_model)$r.squared
rsq


