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

summarise_by_week <- function(x) {
  x %>%
    summarise_by_time(.date_var = date, .by = "week")
}

make_recipe_basic <- function(x) {
  recipe(gwl ~ tg + rr + date, data = x) |>
    step_normalize(all_numeric_predictors())
}

make_recipe <- function(x) {
  recipe(gwl ~ ., data = x) |>
    step_rm(any_of(c("tn", "tx", "pp", "hu", "fg", "qq", "et", "prcp", "tmax", "tmin", "stage_m", "et_2"))) |>
    step_normalize(all_numeric_predictors()) |>
    update_role(well_id, new_role = "id") |>
    step_lag(contains("previous_week"), lag = 1:5) |>
    timetk::step_ts_clean(all_numeric()) |>
    timetk::step_timeseries_signature(date) |>
    step_rm(well_id) |>
    step_pca(all_numeric_predictors()) |>
    recipes::step_zv(all_predictors())
}

make_recipe_1 <- function(x) {
  # well_id <- x |>
  #   slice(1) |>
  #   pull(well_id)
  # 
  # locale_set <- case_when(
  #   well_id == "Germany" ~ "DE",
  #   TRUE ~ "World"
  # )

  recipe(gwl ~ ., data = x) |>
    # step_rm(any_of(c("tn", "tx", "pp", "hu", "fg", "qq", "et", "prcp", "tmax", "tmin", "stage_m", "et_2"))) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::step_lag(dplyr::contains("previous_week"), lag = 1:5) |>
    timetk::step_ts_clean(recipes::all_numeric()) |>
    # timetk::step_timeseries_signature(date) |>
    recipes::step_rm(well_id) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_date(date, features = c("month", "quarter")) |>
    # embed::step_embed(recipes::all_nominal_predictors())
    recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = TRUE)
}

make_tune_grid_xgboost <- function() {
  grid_regular(
    learn_rate(),
    tree_depth(),
    loss_reduction(),
    levels = 8
  )
}

make_model_grid_xgboost <- function(tune_grid) {
  tune_grid |>
    create_model_grid(
      f_model_spec = boost_tree,
      engine_name = "xgboost",
      mode = "regression"
    )
}

make_tune_grid_svm <- function() {
  grid_regular(
    rbf_sigma(),
    levels = 5
  )
}

make_model_grid_svm <- function(tune_grid) {
  tune_grid |>
    create_model_grid(
      f_model_spec = svm_rbf,
      engine_name = "kernlab",
      mode = "regression"
    )
}

make_tune_grid_prophet <- function() {
  grid_regular(
    changepoint_num(),
    changepoint_range(),
    seasonality_yearly(),
    levels = 3
  )
}

make_model_grid_prophet <- function(tune_grid) {
  tune_grid |>
    create_model_grid(
      f_model_spec = prophet_reg,
      engine_name = "prophet",
      mode = "regression"
    )
}


make_model_automl <- function() {
  auto_ml(mode = "regression")
}

make_workflow_set <- function(recipes, models) {
  workflow_set(
    preproc = recipes,
    models = models,
    cross = TRUE
  )
}

fit_models <- function(data, workflow_set) {
  workflow_set |>
    modeltime_fit_workflowset(
      data = data,
      control = control_fit_workflowset(
        verbose = TRUE,
        allow_par = ALLOW_PAR
        # cores = CORES
      )
    )
}
