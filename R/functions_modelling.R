make_resampling <- function(x) {
  nrow = nrow(x)
  well_id <- x |> 
    pull(well_id) |> 
    unique()
  
  x |>
    timetk::time_series_cv(
      date,
      initial = floor(nrow * CV_INITIAL),
      assess = CV_ASSESS,
      skip = CV_SKIP,
      slice_limit = CV_SLICE_MAX,
      lag = if_else(str_detect(well_id, "Sweden"), CV_LAG[2], CV_LAG[1]),
      cumulative = TRUE
    )
}

summarise_by_week <- function(x) {
  x %>%
    summarise_by_time(.date_var = date, .by = "week")
}

make_recipe_pure <- function(x) {
  recipe(gwl ~ tg + rr + date, data = x) |>
    step_rm(contains("_lag")) |> 
    step_normalize(all_numeric_predictors())
}

make_recipe_wolag_logtrans_linimp_norm <- function(x) {
  recipe(gwl ~ ., data = x) |>
    step_rm(contains("_lag")) |> 
    # timetk::step_ts_impute(all_numeric_predictors(), all_outcomes()) |> 
    # timetk::step_slidify_augment(rr, tg, period = c(30, 90), partial = TRUE, .f = ~mean(.x), align = "right") |>
    step_log(et, rr, offset = 1E-5) |>
    step_impute_linear(all_numeric_predictors()) |> 
    # timetk::step_ts_impute(all_numeric_predictors()) |> 
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

make_recipe_wolag_logtrans_linimp_norm_zv_augmdate_corr_pca <- function(x) {
  # well_id <- x |>
  #   slice(1) |>
  #   pull(well_id)
  # 
  # locale_set <- case_when(
  #   well_id == "Germany" ~ "DE",
  #   TRUE ~ "World"
  # )

  recipe(gwl ~ ., data = x) |>
    step_rm(contains("_lag")) |> 
    # step_rm(any_of(c("tn", "tx", "pp", "hu", "fg", "qq", "et", "prcp", "tmax", "tmin", "stage_m", "et_2"))) |>
    update_role(well_id, new_role = "id") |>
    # step_lag(contains("previous_week"), lag = 1:5) |>
    step_log(et, rr, offset = 1E-5) |>
    step_impute_linear(all_numeric_predictors(), -rr) |>
    # step_slidify_augment(rr, tg, period = seq(10, 100, by = 10), partial = TRUE, .f = ~ mean(.x), align = "right") |>
    step_normalize(all_numeric_predictors()) |> 
    # timetk::step_ts_clean(recipes::all_numeric()) |>
    # timetk::step_timeseries_signature(date) |>
    step_zv(all_predictors()) |>
    step_date(date, features = c("month", "quarter")) |>
    # embed::step_embed(recipes::all_nominal_predictors()) |> 
    # step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
    # step_impute_linear(all_numeric_predictors()) |> 
    step_corr(all_numeric_predictors()) |> 
    # timetk::step_fourier(date, period = 365, K = 5) |> 
    step_pca(all_numeric_predictors())
}

make_recipe_wolag_logtrans_linimp_norm_zv_corr <- function(x) {

  recipe(gwl ~ ., data = x) |>
    step_rm(contains("_lag")) |> 
    update_role(well_id, new_role = "id") |>
    # step_lag(contains("previous_week"), lag = seq(7, 7 * 25, 7)) |>
    # step_impute_linear(contains("lag")) |> 
    step_log(et, rr, offset = 1E-5) |>
    step_impute_linear(all_numeric_predictors(), -rr) |>
    step_normalize(all_numeric_predictors()) |> 
    step_zv(all_predictors()) |>
    step_corr(all_numeric_predictors())
}

make_recipe_lag_logtrans_linimp_norm_zv_corr <- function(x) {

  recipe(gwl ~ ., data = x) |>
    update_role(well_id, new_role = "id") |>
    # step_lag(contains("previous_week"), lag = seq(7, 7 * 25, 7)) |>
    # step_impute_linear(contains("lag")) |> 
    step_log(et, rr, offset = 1E-5) |>
    step_impute_linear(all_numeric_predictors(), -rr) |>
    step_normalize(all_numeric_predictors()) |> 
    step_zv(all_predictors()) |>
    step_corr(all_numeric_predictors())
}

make_recipe_wolag_logtrans_linimp_norm_zv_augmdate_corr_pca_nodate <- function(x) {

  recipe(gwl ~ ., data = x) |>
    step_rm(contains("_lag")) |> 
    # step_rm(any_of(c("tn", "tx", "pp", "hu", "fg", "qq", "et", "prcp", "tmax", "tmin", "stage_m", "et_2"))) |>
    update_role(well_id, new_role = "id") |>
    # step_lag(contains("previous_week"), lag = 1:5) |>
    step_impute_linear(all_numeric_predictors(), -rr) |>
    # step_slidify_augment(rr, tg, period = seq(10, 100, by = 10), partial = TRUE, .f = ~ mean(.x), align = "right") |>
    step_normalize(all_numeric_predictors()) |> 
    # timetk::step_ts_clean(recipes::all_numeric()) |>
    # timetk::step_timeseries_signature(date) |>
    step_zv(all_predictors()) |>
    step_date(date, features = c("month", "quarter")) |>
    # embed::step_embed(recipes::all_nominal_predictors()) |> 
    # step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
    # step_impute_linear(all_numeric_predictors()) |> 
    step_corr(all_numeric_predictors()) |> 
    # timetk::step_fourier(date, period = 365, K = 5) |> 
    step_pca(all_numeric_predictors()) |> 
    step_rm(all_nominal_predictors(), date)
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

make_tune_grid_mlp <- function() {
  grid_regular(
    hidden_units(),
    # penalty(),
    dropout(range = c(1E-3, 0.7)),
    epochs(),
    # activation(),
    learn_rate(),
    levels = HYPPAR_LEVELS
  )
}

make_model_grid_mlp <- function(tune_grid) {
  tune_grid |>
    create_model_grid(
      f_model_spec = mlp,
      engine_name = "nnet",
      mode = "regression"
      # activation = "relu"
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

remove_failed_workflows <- function(x) {
  wflows_to_remove <- x |>
    unnest(info) |>
    pull(result) |>
    map_lgl(is_tibble) |>
    magrittr::equals(FALSE) |>
    which()
  
  print(wflows_to_remove)

  x |>
    filter(!(row_number() %in% wflows_to_remove))
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
