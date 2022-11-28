prepare_submission_table <- function(train_split, test_split) {
  train_split <- train_split |>
    reduce(bind_rows) |>
    mutate(split = "train")
  test_split <- test_split |>
    reduce(bind_rows) |>
    mutate(split = "test")

  train_split |>
    bind_rows(test_split) |>
    select(well_id, date, gwl, .pred, split) |>
    group_by(date, well_id, gwl, split) |>
    summarise(
      lb = quantile(.pred, probs = c(0.05), na.rm = TRUE),
      ub = quantile(.pred, probs = c(0.95), na.rm = TRUE),
      mean = mean(.pred, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(well_id) %>%
    arrange(date) %>%
    rename(
      Location = well_id,
      Date = date,
      `Simulated Head` = mean,
      `95% Lower Bound` = lb,
      `95% Upper Bound` = ub
    ) |>
      select(
        Location, Date, `Simulated Head`,
        `95% Lower Bound`,
        `95% Upper Bound`
      )
}