library(modeltime)
library(timetk)
library(tidyverse)
library(tidymodels)
library(embed)
library(vroom)
library(workflows)
library(forecast)
library(patchwork)
library(ranger)
library(plotly)
library(prophet)

train <- vroom("train.csv")
test <- vroom("test.csv")

subset_items <- train %>%
  filter(item %in% c(1, 7, 12, 28, 37))

my_recipe <- recipe(sales ~ ., data = subset_items) %>%
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

cv_folds <- vfold_cv(subset_items, v = 5)

tuned_results <- rf_wf |>
  tune_grid(resamples = cv_folds, 
            grid = tree_grid, 
            metrics = metric_set(smape))


best_params <- tuned_results |> 
  show_best(metric = "smape", n = 1)

rf_final_wf <- rf_wf %>% 
  finalize_workflow(best_params) 

rf_final_model <- fit(rf_final_wf, data = subset_items)

predictions <- predict(rf_final_model, new_data = test)

# predictions <- predictions %>%
#   filter(!is.na(.model_id)) %>%
#   mutate(id = test$id) %>%
#   select(id, .value) %>%
#   rename(sales = .value)

vroom_write(predictions, "submission.csv", delim = ',')
