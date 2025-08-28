#install.packages("neuralnet")
library(neuralnet)

# Sample data
data <- data.frame(
  x1 = runif(100),
  x2 = runif(100),
  y = sample(0:1, 100, replace = TRUE)
)


# Train a neural net
nn <- neuralnet(y ~ x1 + x2, data = data, hidden = c(5, 3), linear.output = FALSE)

# Plot it
plot(nn)

pred <- compute(nn, data[, c("x1", "x2")])
round(pred$net.result)

# 
# library(keras)
# 
# # Define model
# model <- keras_model_sequential() %>%
#   layer_dense(units = 128, activation = 'relu', input_shape = c(784)) %>%
#   layer_dropout(rate = 0.4) %>%
#   layer_dense(units = 10, activation = 'softmax')
# 
# # Compile
# model %>% compile(
#   loss = 'categorical_crossentropy',
#   optimizer = optimizer_rmsprop(),
#   metrics = c('accuracy')
# )


# AND logic table
training_data <- data.frame(
  input1 = c(0, 0, 1, 1),
  input2 = c(0, 1, 0, 1),
  output = c(0, 0, 0, 1)
)
# One hidden layer with 2 neurons
nn <- neuralnet(output ~ input1 + input2,
                data = training_data,
                hidden = 2,
                act.fct = "logistic",
                linear.output = FALSE)

plot(nn)
# Test the model on the same inputs
pred <- compute(nn, training_data[, c("input1", "input2")])
round(pred$net.result)



## with running times
# note linear.output = TRUE
training_data <- data.frame(
  input1 = c(8.47, 4.55, 3.11, 9.00), #distance
  input2 = c(99.3, 50, 29.5, 104), #timeToCompleteInMin
  output = c(156, 148, 166, 155) #avgHeartRate
)

# Normalize the data
maxs <- apply(training_data, 2, max)
mins <- apply(training_data, 2, min)
scaled_data <- as.data.frame(scale(training_data, center = mins, scale = maxs - mins))

# Train the neural network
nn <- neuralnet(output ~ input1 + input2,
                data = scaled_data,
                hidden = 2,
                act.fct = "logistic",
                linear.output = TRUE)

plot(nn)

# Make predictions
pred <- compute(nn, scaled_data[, c("input1", "input2")])

# Rescale predictions back to original range
predicted_scaled <- pred$net.result
predicted_original <- predicted_scaled * (max(training_data$output) - min(training_data$output)) + min(training_data$output)
round(predicted_original)


# new values to predict...
# New input values
new_data <- data.frame(input1 = 9.68,  # new distance
                       input2 = 119.6)   # new time

# Normalize new data using training mins and maxs
new_scaled <- as.data.frame(scale(new_data, center = mins[c("input1", "input2")], 
                                  scale = maxs[c("input1", "input2")] - mins[c("input1", "input2")]))

# Predict using the trained model
new_pred <- compute(nn, new_scaled)

# Rescale prediction back to original heart rate range
predicted_hr_scaled <- new_pred$net.result
predicted_hr_original <- predicted_hr_scaled * 
  (max(training_data$output) - min(training_data$output)) + 
  min(training_data$output)

round(predicted_hr_original)



new_data <- data.frame(input1 = 4.75,  # new distance
                       input2 = 48)   # new time

# Normalize new data using training mins and maxs
new_scaled <- as.data.frame(scale(new_data, center = mins[c("input1", "input2")], 
                                  scale = maxs[c("input1", "input2")] - mins[c("input1", "input2")]))

# Predict using the trained model
new_pred <- compute(nn, new_scaled)

# Rescale prediction back to original heart rate range
predicted_hr_scaled <- new_pred$net.result
predicted_hr_original <- predicted_hr_scaled * 
  (max(training_data$output) - min(training_data$output)) + 
  min(training_data$output)

round(predicted_hr_original)

