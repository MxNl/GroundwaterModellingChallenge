targets_preprocessing <- list(
  tar_target(
    data_no_empty_cols,
    data %>%
      janitor::remove_empty("cols"),
    pattern = map(data),
    iteration = "list"
  ),
  tar_target(
    data_imputed,
    data_no_empty_cols %>%
      mutate(gwl = zoo::na.approx(gwl)),
    pattern = map(data_no_empty_cols),
    iteration = "list"
  ),
  tar_target(
    data_modelling,
    data_imputed %>%
      filter(date >= split_dates %>%
        pull(train_start) %>%
        lubridate::as_date()) %>%
      filter(date <= split_dates %>%
        pull(train_end) %>%
        lubridate::as_date()),
    pattern = map(data_imputed, split_dates),
    iteration = "list"
  ),
  tar_target(
    data_prediction,
    data_imputed %>%
      filter(date > split_dates %>%
        pull(train_end) %>%
        lubridate::as_date()),
    pattern = map(data_imputed, split_dates),
    iteration = "list"
  )
)