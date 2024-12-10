library(tidyverse)
library(tidymodels)
library(vroom)
library(forecast)
library(patchwork)
library(modeltime)
library(timetk)

# Load data
train <- vroom("train.csv")
test <- vroom("test.csv")

# Filter for specific store-item combinations
storeItemTrain_1 <- train %>%
  filter(store == 3, item == 5)
storeItemTest_1 <- test %>%
  filter(store == 3, item == 5)

storeItemTrain_2 <- train %>%
  filter(store == 8, item == 41)
storeItemTest_2 <- test %>%
  filter(store == 8, item == 41)

cv_split <- time_series_split(storeItemTrain_1, assess = "3 months", cumulative = TRUE)

# Recipe
arima_recipe <- recipe(sales ~ ., data = training(cv_split)) %>%
  step_rm(item, store) %>%
  step_date(date, features = c("doy", "decimal")) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy)) %>%
  step_rm(date_doy)

# ARIMA model
arima_model <- arima_reg() %>%
  set_engine("auto_arima")

# Workflow
arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data = training(cv_split))
  

# Cross-validation
cv_results <- modeltime_calibrate(
  arima_wf,
  new_data = testing(cv_split)
)
  
plot_1 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = training(cv_split)
  ) %>%
  plot_modeltime_forecast(.interactive = FALSE)
  
# Refit to the whole dataset
full_fit <- cv_results %>%
  modeltime_refit(data = storeItemTrain_1)

plot_2 <- full_fit %>%
  modeltime_forecast(
    new_data = storeItemTest_1,
    actual_data = storeItemTrain_1
  ) %>%
  plot_modeltime_forecast(.interactive = FALSE)

## Item 2

cv_split2 <- time_series_split(storeItemTrain_2, assess = "3 months", cumulative = TRUE)

# Recipe
arima_recipe2 <- recipe(sales ~ ., data = training(cv_split2)) %>%
  step_rm(item, store) %>%
  step_date(date, features = c("doy", "decimal")) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy)) %>%
  step_rm(date_doy)

# ARIMA model
arima_model2 <- arima_reg() %>%
  set_engine("auto_arima")

# Workflow
arima_wf2 <- workflow() %>%
  add_recipe(arima_recipe2) %>%
  add_model(arima_model2) %>%
  fit(data = training(cv_split2))


# Cross-validation
cv_results2 <- modeltime_calibrate(
  arima_wf2,
  new_data = testing(cv_split2)
)

plot_3 <- cv_results2 %>%
  modeltime_forecast(
    new_data = testing(cv_split2),
    actual_data = training(cv_split2)
  ) %>%
  plot_modeltime_forecast(.interactive = FALSE)

# Refit to the whole dataset
full_fit2 <- cv_results2 %>%
  modeltime_refit(data = storeItemTrain_2)

plot_4 <- full_fit2 %>%
  modeltime_forecast(
    new_data = storeItemTest_2,
    actual_data = storeItemTrain_2
  ) %>%
  plot_modeltime_forecast(.interactive = FALSE)

paneled_plot <- plotly::subplot(
  plot_1, plot_3, plot_2, plot_4,
  nrows = 2
) %>%
  plotly::layout(showlegend = FALSE)  # Disable legend

paneled_plot

plot_1

plot_2

plot_3

plot_4
