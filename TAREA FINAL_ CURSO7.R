library(rJava)
url<- 'https://dax-cdn.cdn.appdomain.cloud/dax-noaa-weather-data-jfk-airport/1.1.4/noaa-weather-sample-data.tar.gz'
download.file(url, destfile = "noaa-weather-sample-data.tar.gz")
untar("noaa-weather-sample-data.tar.gz", tar = "internal")
weather<-read.csv("noaa-weather-sample-data/jfk_weather_sample.csv")
head(weather)
summary(weather)
dim(weather)
new_variable<-selected_columns <- weather[,c("HOURLYRelativeHumidity","HOURLYDRYBULBTEMPF","HOURLYPrecip","HOURLYWindSpeed","HOURLYStationPressure")]
new_variable
head(new_variable,10)
unique_values <- unique(selected_columns$HOURLYPrecip)
unique_values
modified_columns <- selected_columns %>%
  mutate(HOURLYPrecip = ifelse(HOURLYPrecip == "T", "0.0", str_replace(HOURLYPrecip, "s$", "")))
print(unique(modified_columns$HOURLYPrecip))
glimpse(modified_columns)
modified_columns$HOURLYPrecip <- as.numeric(modified_columns$HOURLYPrecip)
glimpse(modified_columns)

final_dataframe <- modified_columns %>%
  rename(
    relative_humidity = HOURLYRelativeHumidity,
    dry_bulb_temp_f = HOURLYDRYBULBTEMPF,
    precip = HOURLYPrecip,
    wind_speed = HOURLYWindSpeed,
    station_pressure = HOURLYStationPressure
  )
final_dataframe
index <- sample(1:nrow(final_dataframe), 0.8 * nrow(final_dataframe))


final_dataframe %>%
  gather(key = "variable", value = "value", -c(1)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Histograms of Variables in Training Set",
       x = "Value",
       y = "Frequency")

lm_model <- lm(precip ~ relative_humidity, data = final_dataframe)
lm_model2 <- lm(precip ~ dry_bulb_temp_f, data = final_dataframe)
lm_model3 <- lm(precip ~ wind_speed, data = final_dataframe)
lm_model4 <- lm(precip ~ station_pressure, data = final_dataframe)

ggplot(final_dataframe, aes(x = relative_humidity, y = precip)) +
  geom_point(color = "skyblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatter Plot and Linear Regression Model",
       x = "Relative Humidity",
       y = "Precipitation")

# DEFINIR EL MODELO CON MAS PREDICTORES
model_with_more_predictors <- lm(precip ~ ., data = final_dataframe)

# IMPRIMIR EL RESUMEN DEL MODELO
print(summary(model_with_more_predictors))

# EVALUAR EL RENDIMIENTO DEL MODELO EN EL CONJUNTO DE ENTRENAMIENTO
predictions_more_predictors <- predict(model_with_more_predictors, newdata = final_dataframe)
mse_more_predictors <- mean((final_dataframe$precip - predictions_more_predictors)^2)
rmse_more_predictors <- sqrt(mse_more_predictors)
rsquared_more_predictors <- summary(model_with_more_predictors)$r.squared

# IMPRIMA LAS METRICAS DE RENDIMIENTO DEL MODELO
print(paste("MSE with more predictors:", mse_more_predictors))
print(paste("RMSE with more predictors:", rmse_more_predictors))
print(paste("R-squared with more predictors:", rsquared_more_predictors))

# DEFINIR EL MODELO CON UN COMPONENTE POLINOMICO
model_with_polynomial_tidymodels <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression") %>%
  fit(precip ~ poly(relative_humidity, degree = 2) + poly(dry_bulb_temp_f, degree = 2), data = final_dataframe)

# IMPRIMIR EL RESUMEN DEL MODELO
summary(model_with_polynomial_tidymodels$fit)

# EVALUAR EL RENDIMIENTO DEL MODELO EN EL CONJUNTO DE ENTRENAMIENTO
predictions_polynomial_tidymodels <- predict(model_with_polynomial_tidymodels, new_data = final_dataframe) %>%
  as.vector()
mse_polynomial_tidymodels <- mean((final_dataframe$precip - predictions_polynomial_tidymodels)^2)
rmse_polynomial_tidymodels <- sqrt(mse_polynomial_tidymodels)
rsquared_polynomial_tidymodels <- rsquare(predictions_polynomial_tidymodels, final_dataframe$precip)

# IMPRIMA LAS METRICAS DE RENDIMIENTO DEL MODELO
print(paste("MSE with polynomial component (tidymodels):", mse_polynomial_tidymodels))
print(paste("RMSE with polynomial component (tidymodels):", rmse_polynomial_tidymodels))
print(paste("R-squared with polynomial component (tidymodels):", rsquared_polynomial_tidymodels))

Define the models
lm_model <- lm(precip ~ relative_humidity, data = final_dataframe)
lm_model2 <- lm(precip ~ dry_bulb_temp_f, data = final_dataframe)
lm_model3 <- lm(precip ~ wind_speed, data = final_dataframe)
lm_model4 <- lm(precip ~ station_pressure, data = final_dataframe)
model_with_more_predictors <- lm(precip ~ ., data = final_dataframe)
model_with_polynomial_tidymodels <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression") %>%
  fit(precip ~ poly(relative_humidity, degree = 2) + poly(dry_bulb_temp_f, degree = 2), data = final_dataframe)

# Function to calculate RMSE
calculate_rmse <- function(model, data) {
  predictions <- predict(model, newdata = data)
  rmse <- sqrt(mean((data$precip - predictions)^2))
  return(rmse)
}

# Calculate RMSE for each model on both training and testing sets
model_names <- c("lm_model", "lm_model2", "lm_model3", "lm_model4", "model_with_more_predictors", "model_with_polynomial_tidymodels")
train_rmse <- sapply(list(lm_model, lm_model2, lm_model3, lm_model4, model_with_more_predictors, model_with_polynomial_tidymodels), calculate_rmse, data = final_dataframe)
test_rmse <- sapply(list(lm_model, lm_model2, lm_model3, lm_model4, model_with_more_predictors, model_with_polynomial_tidymodels), calculate_rmse, data = testing_set)

# Create the comparison table
comparison_df <- data.frame(model_names, train_rmse, test_rmse)

# Print the comparison table
print(comparison_df)