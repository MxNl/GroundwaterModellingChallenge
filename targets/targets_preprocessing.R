targets_preprocessing <- list(
  tar_target(
    data_no_empty_cols,
    data %>%
      janitor::remove_empty("cols"),
    pattern = map(data),
    iteration = "list"
  ),
  tar_target(
    data_previous_week_predictors,
    data_no_empty_cols %>%
      aggregated_predictors(),
    pattern = map(data_no_empty_cols),
    iteration = "list"
  ),
  # tar_target(
  #   data_imputed,
  #   data_no_empty_cols %>%
  #     mutate(gwl = zoo::na.approx(gwl)),
  #   pattern = map(data_no_empty_cols),
  #   iteration = "list"
  # ),
  tar_target(
    data_modelling,
    slice_modelling_period(data_previous_week_predictors, split_dates),
    pattern = map(data_previous_week_predictors, split_dates),
    iteration = "list"
  ),
  tar_target(
    data_prediction,
    data_previous_week_predictors %>%
      filter(date > split_dates$train_end),
    pattern = map(data_previous_week_predictors, split_dates),
    iteration = "list"
  )
)
