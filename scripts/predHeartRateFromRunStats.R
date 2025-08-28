library(neuralnet)
library(tidyverse)

# import running stats from garmin
training_data <- read_csv("data/runningStats.csv")
# select columns - last one is the one we want to predict
training_data <- training_data %>% 
  select(Distance, Time, `Min Temp`, `Max Temp`, `Min Elevation`, 
         `Max Elevation`, `Moving Time`, `Elapsed Time`, `Best Pace`, 
         `Max HR`,`Avg Stride Length`,`Avg Run Cadence`, `Max Run Cadence`, 
         `Total Ascent`, `Total Descent`, `Avg HR`)

# convert times into units that make more sense (seconds)
training_data$TimeInSec <- as.numeric(training_data$Time)
training_data$TotalTimeInMin <- round(as.numeric(training_data$TimeInSec / 60),1)

training_data$MovingTimeInSec <- as.numeric(training_data$`Moving Time`)
training_data$MovingTimeInMin <- round(as.numeric(training_data$TimeInSec / 60),1)

training_data$ElapsedTimeInSec <- as.numeric(training_data$`Elapsed Time`)
training_data$ElapsedTimeInMin <- round(as.numeric(training_data$TimeInSec / 60),1)

training_data$BestPace <- as.numeric(training_data$`Best Pace`/60)
training_data$BestPaceMPH <- round(as.numeric(training_data$BestPace/60),1)

#View(training_data)

trim_training_data <- training_data %>% 
  select(Distance, TotalTimeInMin, `Min Temp`, `Max Temp`, `Min Elevation`, `Max Elevation`, 
        MovingTimeInMin, ElapsedTimeInMin, BestPaceMPH, `Max HR`, `Total Ascent`, 
        `Total Descent`, `Avg HR`)
# got rid of `Avg Stride Length`, `Avg Run Cadence`, `Max Run Cadence`

names(trim_training_data)[which(names(trim_training_data) == "Min Temp")] <- "MinTemp"
names(trim_training_data)[which(names(trim_training_data) == "Max Temp")] <- "MaxTemp"
names(trim_training_data)[which(names(trim_training_data) == "Min Elevation")] <- "MinElevation"
names(trim_training_data)[which(names(trim_training_data) == "Max Elevation")] <- "MaxElevation"
names(trim_training_data)[which(names(trim_training_data) == "Max HR")] <- "MaxHR"
names(trim_training_data)[which(names(trim_training_data) == "Total Ascent")] <- "TotalAscent"
names(trim_training_data)[which(names(trim_training_data) == "Total Descent")] <- "TotalDescent"
names(trim_training_data)[which(names(trim_training_data) == "Avg HR")] <- "AvgHR"

#gsub(",", "", trim_training_data$MinElevation)

#get rid of missing data (elevation)
trim_training_data <- trim_training_data %>%
  filter(MinElevation != "--") %>%
  filter(MaxElevation != "--") %>%
  filter(TotalAscent != "--") %>%
  filter(TotalDescent != "--")

cols <- names(trim_training_data)

trim_training_data[cols] <- lapply(trim_training_data[cols], function(x) {
  x <- gsub(",", "", x)
  as.numeric(x)
})

#View(training_data)
View(trim_training_data)

# # ignoring negatives
# trim_training_data <- trim_training_data[grep("-", trim_training_data$MinElevation, invert = TRUE),]
# trim_training_data <- trim_training_data[grep("-", trim_training_data$MaxElevation, invert = TRUE),]

# all numeric
trim_training_data[] <- lapply(trim_training_data, function(x) as.numeric(as.character(x)))
#View(trim_training_data)

# Normalize the data
maxs <- apply(trim_training_data, 2, max)
mins <- apply(trim_training_data, 2, min)
scaled_data <- as.data.frame(scale(trim_training_data, center = mins, scale = maxs - mins))

scaled_data[] <- lapply(scaled_data, function(x) as.numeric(as.character(x)))
#View(scaled_data)

# Build formula dynamically
predictors <- setdiff(names(scaled_data), "AvgHR")
formula <- as.formula(paste("AvgHR ~", paste(predictors, collapse = " + ")))

# Train the model
nn <- neuralnet::neuralnet(formula,
                data = scaled_data,
                hidden = 2,
                act.fct = "logistic",
                linear.output = TRUE)

plot(nn)

# Make predictions
pred <- neuralnet::compute(nn, scaled_data[, predictors])

# check predictions to see how they compare
predicted_AvgHR <- pred$net.result

results <- scaled_data
results$Predicted_AvgHR <- predicted_AvgHR

actual <- scaled_data$AvgHR
predicted <- results$Predicted_AvgHR

# Mean Squared Error
mse <- mean((actual - predicted)^2)
mse
# Correlation
correlation <- cor(actual, predicted)
correlation

#new_pred <- compute(nn, new_scaled_data[, predictors])$net.result
#cat(names(scaled_data), sep = ", ")


predict_heart_rate <- function(Distance, TotalTimeInMin, MinTemp, MaxTemp,
                               MinElevation, MaxElevation, MovingTimeInMin,
                               ElapsedTimeInMin, BestPaceMPH, MaxHR, TotalAscent,
                               TotalDescent, nn_model, mins, maxs, output_min, output_max) {
  
  # Create new input row
  new_data <- data.frame(Distance, TotalTimeInMin, MinTemp, MaxTemp, MinElevation,
                         MaxElevation, MovingTimeInMin, ElapsedTimeInMin, BestPaceMPH,
                         MaxHR, TotalAscent, TotalDescent)
  
  # Normalize using training mins and maxs
  scaled_input <- as.data.frame(scale(new_data,
                                      center = mins[colnames(new_data)],
                                      scale = maxs[colnames(new_data)] - mins[colnames(new_data)]))
  
  # Predict using trained model
  pred <- neuralnet::compute(nn_model, scaled_input)
  
  # Rescale prediction to original heart rate range
  scaled_result <- pred$net.result
  original_result <- scaled_result * (output_max - output_min) + output_min
  
  return(round(original_result, 1))
}

predict_heart_rate(Distance = 4.1,
                   TotalTimeInMin = 48,
                   MinTemp = 100,
                   MaxTemp = 110, 
                   MinElevation = 1200,
                   MaxElevation = 1350,
                   MovingTimeInMin = 47, 
                   ElapsedTimeInMin = 49,
                   BestPaceMPH = 7.5,
                   MaxHR = 170,
                   TotalAscent = 200,
                   TotalDescent = 190,
                   nn_model = nn, mins = mins, maxs = maxs,
                   output_min = min(training_data$`Avg HR`),
                   output_max = max(training_data$`Avg HR`))
