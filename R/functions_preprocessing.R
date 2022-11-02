slice_by_wellid <- function(x, nth = 1) {
  x |>
    group_by(well_id) |>
    group_split() |>
    magrittr::extract(nth) |>
      reduce(bind_rows)
}

slice_modelling_period <- function(x, split_dates) {
  x |> filter(between(date, split_dates$train_start, split_dates$train_end))
}

aggregated_predictors <- function(.data) {
  .data |>
    mutate(
      across(
        -all_of(c("well_id", "date", "gwl", "rr")),
        data.table::frollmean, n = 7, align = "right",
        .names = '{.col}_previous_week_mean'
      ),
      rr_previous_week_sum = data.table::frollsum(rr, n = 7, align = "right")
    )
}
