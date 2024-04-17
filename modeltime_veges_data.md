Time Series
================
Mwangi N. George
2023-03-18

``` r
#load packages
pacman::p_load(tidyverse, timetk, trelliscopejs, modeltime, naniar, tidymodels) 
```

``` r
# data preparation
veges_df <- read_csv("datasets/tsdata.csv", show_col_types = F) %>% 
  janitor::clean_names() %>% 
  mutate_if(is.character, as.factor) %>% 
  select(commodity, date, average) %>% 
  set_names(c("id", "date", "value"))
```

``` r
# Some data visualization
veges_df %>% 
  group_by(id) %>% 
  plot_time_series(date, value, .interactive = F, .facet_scales = "free_y")+
  facet_trelliscope(~id, as_plotly = T)
```

``` r
# select some time series to visualize 
vege_sub_df <- veges_df %>% 
  filter(
    id  %in% c(
    "Asparagus", "Bitter Gourd", "Capsicum", "Apple(Jholey)",
    "Coriander Green", "Garlic Dry Chinese", "Gundruk", "Lime", 
    "Pomegranate", "Onion Green")
    ) 
```

``` r
vege_sub_df %>% 
  group_by(id) %>% 
  plot_time_series(
    date, value, .interactive = F, .y_lab = "Price in USD", .smooth_alpha = .5, .title = ""
    )+
  facet_trelliscope(~id, as_plotly = T)
```

``` r
# fit linear models to each timeseries if the dataset has other features other than value
# veges_df %>%
#   group_by(id) %>%
#   plot_time_series_regression(
#     .date_var = date,
#     .formula = value ~ feature1 + feature2 + feature3,
#     .facet_ncol = 2,
#     .show_summary = TRUE,
#     .interactive = FALSE
#     )+
#   facet_trelliscope(~id, ncol = 2, as_plotly = T)
```

``` r
# prepare data for forecasting
veges_extended_df <- vege_sub_df %>% 
  extend_timeseries(.id_var = id, .date_var = date, .length_future = 365) 


# print last 6 rows
tail(veges_extended_df)
```

``` r
# visualize
veges_extended_df %>% 
  group_by(id) %>% 
  # visualize data
  plot_time_series(
    date, value, .facet_ncol = 2, 
    .interactive = F, .title = "", .y_lab = "Price"
    )+
  # visualize missing the extended
  geom_miss_point()+
  # take a closer look
  facet_trelliscope(~id)
```

``` r
# mode data preparation for forecasting
nested_veges_df <- veges_extended_df %>% 
  # look 365 days into the future, assess using the last 3 years of the data(look back period)
  nest_timeseries(.id_var = id, .length_future = 365, .length_actual = 365*3) %>% 
  # split into train and test sets 
  split_nested_timeseries(.length_test = 365)


# extract training and testing sets
veges_train <- extract_nested_train_split(nested_veges_df)
veges_test <- extract_nested_test_split(nested_veges_df)
```

``` r
# create recipes and workflows for model fitting

# 1. Arima model

# recipe
arima_recipe <- recipe(value ~ date, data = veges_train)

# workflow 
arima_wrkflw <- workflow() %>% 
  # specify model
  add_model(
    arima_reg(seasonal_period = 365) %>% 
      set_mode("regression") %>% 
      set_engine("auto_arima")
    ) %>% 
  # adding recipe
  add_recipe(arima_recipe)


# 2. Prophet model

# recipe 
prophet_recipe <- recipe(value ~ date, data = veges_train)

# workflow
prophet_wrkflw <- workflow() %>% 
  add_model(
    prophet_reg(seasonality_yearly = TRUE) %>% 
      set_mode("regression") %>% 
      set_engine("prophet")
  ) %>% 
  add_recipe(prophet_recipe)


# 3. XG Boost (Machine Learning)

# recipe 
xgb_recipe <- recipe(value ~ date, data = veges_train) %>% 
  step_timeseries_signature(date) %>% 
  step_rm(date) %>% 
  step_zv(all_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# workflow
xgb_wrkflow <- workflow() %>% 
  add_model(
    boost_tree() %>% 
      set_mode("regression") %>% 
      set_engine("xgboost")
  ) %>% 
  add_recipe(xgb_recipe)


# 4. GLMNET (penalized regression machine learning)

# recipe
glmnet_recipe <- recipe(value ~ date, data = veges_train) %>% 
  step_timeseries_signature(date) %>% 
  step_rm(date) %>% 
  step_zv(all_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# workflow
glmnet_wrkflow <- workflow() %>% 
  add_model(
    linear_reg(penalty = 0.01) %>% 
      set_mode("regression") %>% 
      set_engine("glmnet")
  ) %>% 
  add_recipe(glmnet_recipe)
```

``` r
# fit models on data with modeltime workflow
nested_modeltime_df <- modeltime_nested_fit(
  
    # specify the nested modeltime dataframe
    nested_data = nested_veges_df,
  
    # specify workflows
    arima_wrkflw,
    prophet_wrkflw,
    xgb_wrkflow,
    glmnet_wrkflow,
    
    # control verbosity and parallel processing
    control = control_nested_fit(verbose = TRUE)
  )
```

``` r
# extract error report from the fit
nested_modeltime_df %>% 
  extract_nested_error_report()
```

``` r
# extract accuracy metrics
nested_modeltime_df %>% 
  extract_nested_test_accuracy() %>% 
  group_by(id) %>% 
  # make results interactive
  table_modeltime_accuracy()
```

``` r
# extract forecast on the test data
nested_modeltime_df %>% 
  extract_nested_test_forecast() %>% 
  group_by(id) %>% 
  # visualize 
  plot_modeltime_forecast(.facet_ncol = 2, .interactive = F) +
  facet_trelliscope(~id, as_plotly = T)
```

``` r
# selecting the best model
nested_modeltime_bestmodel_df <- nested_modeltime_df %>% 
  modeltime_nested_select_best(metric = "rmse", minimize = TRUE)
```

``` r
# extract the accuracy metrics for the best selected models
nested_modeltime_bestmodel_df %>% 
  extract_nested_best_model_report() %>% 
  # make results interactive
  DT::datatable()
```

``` r
# Refit the best selected models on the whole dataset and forecast the determined future period
nested_modeltime_bestmodel_fit <- nested_modeltime_bestmodel_df %>% 
  modeltime_nested_refit(
    # control verbosity and parallel processing
    control = control_nested_refit(verbose = TRUE)
  )
```

``` r
# extract forecast 
nested_modeltime_bestmodel_fit %>% 
  extract_nested_future_forecast() %>% 
  group_by(id) %>% 
  # visualize
  plot_modeltime_forecast(
    .interactive = F
  )+
  facet_trelliscope(~id, as_plotly = T)
```