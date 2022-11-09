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

make_recipe_pure <- function(x) {
  recipe(gwl ~ tg + rr + date, data = x) |>
    step_normalize(all_numeric_predictors())
}

make_recipe_pure_but_all_standard_predictors <- function(x) {
  recipe(gwl ~ ., data = x) |>
    # step_rm(any_of(c("tn", "tx", "pp", "hu", "fg", "qq", "et", "prcp", "tmax", "tmin", "stage_m", "et_2"))) |>
    step_normalize(all_numeric_predictors()) |>
    update_role(well_id, new_role = "id")
    # step_lag(contains("previous_week"), lag = 1:5) |>
    # timetk::step_ts_clean(all_numeric()) |>
    # timetk::step_timeseries_signature(date) |>
    # step_rm(well_id) |>
    # step_pca(all_numeric_predictors()) |>
    # recipes::step_zv(all_predictors())
}

make_recipe_recipe_lag_pca_zv_dateext <- function(x) {
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
    recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = TRUE) |> 
    step_pca(all_predictors())
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
    levels = HYPPAR_LEVELS
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

make_tune_grid_nnetar <- function() {
  grid_regular(
    # non_seasonal_ar(range = c(1L, 5L)),
    # seasonal_ar(range = c(1L, 5L)),
    hidden_units(),
    penalty(),
    epochs(),
    levels = HYPPAR_LEVELS
  )
}

make_model_grid_nnetar <- function(tune_grid) {
  tune_grid |>
    create_model_grid(
      f_model_spec = nnetar_reg,
      engine_name = "nnetar",
      mode = "regression"
    )
}

make_model_automl <- function() {
  # agua::h2o_start()
  
  auto_ml() %>%
    set_engine("h2o", max_runtime_secs = 120, seed = 1) %>%
    set_mode("regression")
}

make_workflow_set <- function(recipes, models) {
  workflow_set(
    preproc = recipes,
    models = models,
    cross = TRUE
  )
}

fit_models <- function(data, workflow_set) {
  if(ALLOW_PAR) parallel_start(CORES, .method = "parallel")

  fitted_models <- workflow_set |>
    modeltime_fit_workflowset(
      data = data,
      control = control_fit_workflowset(
        verbose = TRUE,
        allow_par = ALLOW_PAR
      )
    )

  if(ALLOW_PAR) parallel_stop()
  
  return(fitted_models)
}

nse_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  
  nse_impl <- function(truth, estimate) {
    1 - (sum((truth - estimate) ^ 2) / sum((truth - mean(truth)) ^ 2))
  }
  
  metric_vec_template(
    metric_impl = nse_impl,
    truth = truth, 
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
  
}

nse <- function(data, ...) {
  UseMethod("nse")
}

nse <- yardstick::new_numeric_metric(nse, direction = "maximize")

nse.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
  
  metric_summarizer(
    metric_nm = "nse",
    metric_fn = nse_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate), 
    na_rm = na_rm,
    ...
  )
}

tune_resampling <- function(object, resamples) {
  
  if(ALLOW_PAR) {
    cl <- parallel::makePSOCKcluster(CORES)
    parallel::clusterExport(cl, varlist = ls(pattern = "nse"))
    doParallel::registerDoParallel(cl)
    
    metrics_set <- metric_set(rmse, mae, mase, rsq)
  } else {
    metrics_set <- metric_set(rmse, mae, mase, rsq, nse)
  }
  
  result <- workflow_map(
    object,
    fn = "fit_resamples",
    resamples = resamples,
    metrics = metrics_set,
    control = stacks::control_stack_resamples()
  )
  
  if(ALLOW_PAR) parallel::stopCluster(cl)
  
  return(result)
}

fit_best_model <- function(workflowset, ranked_performances, split) {
  
  if(ALLOW_PAR) {
    metrics_set <- metric_set(rmse, mae, mase, rsq)
  } else {
    metrics_set <- metric_set(rmse, mae, mase, rsq, nse)
  }
  
  id_best_model <- ranked_performances |> 
    slice(1) |> 
    pull(wflow_id)
  
  workflowset |> 
    extract_workflow(id_best_model) |>
    last_fit(split, metrics = metrics_set)
}

fit_model_ensemble <- function(model_ensemble) {
  
  model_ensemble |> 
    fit_members()
}


select_best_models <- function(x, y) {
  best_models_wflow_ids <- x |> 
    filter(.metric == "rsq") |> 
    group_by(model) |> 
    slice_head(n = 2) |> 
    pull(wflow_id)
  
  y |> 
    filter(wflow_id %in% best_models_wflow_ids)
}
