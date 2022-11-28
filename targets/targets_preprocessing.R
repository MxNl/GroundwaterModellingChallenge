targets_preprocessing <- list(
  tar_target(
    data_native_gwl_interval,
    data %>%
      map_if(
        .p = ~.x |> drop_na(gwl) |>
          pull(date) |> 
          padr::get_interval() |> 
          magrittr::equals("week"),
        .f = aggregate_to_gwl_interval
      ),
    # pattern = map(data),
    iteration = "list"
  ),
  tar_target(
    data_no_empty_cols,
    data_native_gwl_interval %>%
      janitor::remove_empty("cols"),
    pattern = map(data_native_gwl_interval),
    iteration = "list"
  ),
  tar_target(
    data_corrected,
    data_no_empty_cols %>%
      mutate(et = if_else(et < 0, 0, et)) |> 
      mutate(gwl = imputeTS::na_interpolation(gwl, option = "linear")) |> 
      mutate(rr = replace_na(rr, 0)),
    pattern = map(data_no_empty_cols),
    iteration = "list"
  ),
  tar_target(
    data_previous_week_predictors,
    data_corrected %>%
      aggregated_predictors(),
    pattern = map(data_corrected),
    iteration = "list"
  ),
  tar_target(
    data_lagged_predictors,
    data_previous_week_predictors %>%
      add_lagged_predictors(seq(7, 7 * 25, 7)),
    pattern = map(data_previous_week_predictors),
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
    slice_modelling_period(data_lagged_predictors, split_dates),
    pattern = map(data_lagged_predictors, split_dates),
    iteration = "list"
  ),
  tar_target(
    data_prediction,
    data_lagged_predictors %>%
      filter(date > split_dates$train_end),
    pattern = map(data_lagged_predictors, split_dates),
    iteration = "list"
  )
)
