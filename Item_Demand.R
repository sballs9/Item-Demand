library(tidyverse)
library(tidymodels)
library(vroom)
library(forecast)
library(patchwork)

train <- vroom("train.csv")
test <- vroom("test.csv")

storeItem1 <- train %>%
  filter(store==3, item==5)

storeItem2 <- train %>% 
  filter(store == 8, item==41)


ts_plot1 <- storeItem1 %>%
  ggplot(aes(x = as.Date(date), y = sales)) +
  geom_line(color = "blue") +
  labs(title = "Store 3, Item 5 - Time Series", x = "Date", y = "Sales") +
  theme_minimal()
acf1_month1 <- forecast::ggAcf(storeItem1$sales, lag.max = 30) +
  ggtitle("ACF - 1 Month Lag") +
  theme_minimal()
acf2_years1 <- forecast::ggAcf(storeItem1$sales, lag.max = 2 * 365) +
  ggtitle("ACF - 2 Years Lag") +
  theme_minimal()

ts_plot2 <- storeItem2 %>%
  ggplot(aes(x = as.Date(date), y = sales)) +
  geom_line(color = "red") +
  labs(title = "Store 8, Item 41 - Time Series", x = "Date", y = "Sales") +
  theme_minimal()
acf1_month2 <- forecast::ggAcf(storeItem2$sales, lag.max = 30) +
  ggtitle("ACF - 1 Month Lag") +
  theme_minimal()
acf2_years2 <- forecast::ggAcf(storeItem2$sales, lag.max = 2 * 365) +
  ggtitle("ACF - 2 Years Lag") +
  theme_minimal()

panel_plot <- (ts_plot1 | acf1_month1 | acf2_years1) /
  (ts_plot2 | acf1_month2 | acf2_years2)

panel_plot

my_recipe <- recipe(sales ~ ., data = train) %>%
  step_date(date, features="doy") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))

rf_mod <- rand_forest(min_n = tune(),
                      trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rf_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(rf_mod) 

tree_grid <- grid_regular(min_n(),            
  levels = 5                             
)

cv_folds <- vfold_cv(storeItem1, v = 5)

tuned_results <- rf_wf |>
  tune_grid(resamples = cv_folds, 
            grid = tree_grid, 
            metrics = metric_set(smape))


best_params <- tuned_results |> 
  show_best(metric = "smape", n = 1)

best_params

rf_final_wf <- rf_wf %>% 
  finalize_workflow(best_params) 

final_cv_results <- fit_resamples(
  rf_final_wf,
  resamples = cv_folds,
  metrics = smape)

final_metrics <- collect_metrics(final_cv_results)

print(final_metrics)

