make_resampling <- function(x) {
  x |> 
    timetk::time_series_cv(
      date,
      initial = CV_INITIAL,
      assess = CV_ASSESS,
      skip = CV_ASSESS,
      lag = CV_LAG,
      cumulative = TRUE
    )
}

make_recipe <- function(x) {
  well_id <- x |> 
    slice(1) |> 
    pull(well_id)
  
  locale_set <- case_when(
    well_id == "Germany" ~ "DE",
    TRUE ~ "World"
  )
  
  recipe(gwl ~ ., data = x) |> 
    step_normalize(all_numeric_predictors()) |>
    update_role(well_id, new_role = "id") |> 
    step_lag(all_numeric_predictors(), lag = 1:5) |> 
    step_naomit() |> 
    # timetk::step_timeseries_signature(date) |> 
    step_rm(date, well_id)
    # step_rm(date, well_id, all_of(c("date_month.lbl", "date_wday.lbl")))
    # timetk::step_holiday_signature(date, locale_set = locale_set)
}

make_tune_grid_xgboost <- function() {
  grid_regular(
    learn_rate(),
    tree_depth(),
    levels = 5
  )
}

make_model_grid_xgboost <- function(tune_grid) {
  tune_grid |> 
    create_model_grid(
      f_model_spec = boost_tree,
      engine_name  = "xgboost",
      mode = "regression"
    )
}

make_workflow_set <- function(recipes, models) {
  workflow_set(
    preproc = recipes,
    models = models, 
    cross = TRUE
  )
}

fit_models <- function(data, workflow_set) {
  workflow_set %>%
    modeltime_fit_workflowset(
      data = data,
      control = control_fit_workflowset(
        verbose   = TRUE,
        allow_par = ALLOW_PAR,
        cores = 6
      )
    )
}