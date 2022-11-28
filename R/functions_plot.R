make_plot_results_obs_and_preds <- function(x) {
  x <- x |>
    reduce(bind_rows)

  x |>
    select(well_id, date, gwl, .pred) |>
    group_by(date, well_id, gwl) |>
    summarise(
      lb = quantile(.pred, probs = c(0.05), na.rm = TRUE),
      ub = quantile(.pred, probs = c(0.95), na.rm = TRUE),
      mean = mean(.pred, na.rm = TRUE),
      .groups = "drop"
    ) |>
    ggplot() +
    geom_ribbon(aes(date, ymin = lb, ymax = ub),
      fill = "orange", alpha = .5
    ) +
    geom_line(aes(date, gwl), colour = "grey50") +
    geom_line(aes(date, mean), colour = "red") +
    geom_vline(
      data = x |>
        group_by(well_id) |>
        summarise(min = min(date)),
      aes(xintercept = lubridate::ymd(min)),
      linetype = "dashed",
      colour = "grey"
    ) +
    facet_wrap(~well_id, ncol = 1, scales = "free_y") +
    scale_x_date(
      date_breaks = "year", labels = scales::label_date("%Y"),
      minor_breaks = NULL
    ) +
    labs(y = "Groundwater level [m asl]") +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      axis.title.x = element_blank()
    )

  # x |>
  #   reduce(bind_rows) |>
  #   pivot_longer(cols = all_of(c("gwl", ".pred"))) |>
  #   ggplot(aes(date, value, colour = name)) +
  #   geom_line() +
  #   scale_colour_discrete(
  #     labels = c("predicted", "observed"),
  #     guide = guide_legend(
  #       direction = "horizontal",
  #       reverse = TRUE
  #     )
  #   ) +
  #   labs(y = "Groundwater level [m asl]") +
  #   facet_wrap(~well_id, ncol = 1, scales = "free") +
  #   theme_minimal() +
  #   theme(
  #     legend.position = "top",
  #     legend.title = element_blank(),
  #     axis.title.x = element_blank()
  #   )
}

make_plot_results_obs_and_preds_wtrain <- function(train_split, test_split) {
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
    ) |>
    ggplot() +
    geom_ribbon(aes(date, ymin = lb, ymax = ub),
      fill = "orange", alpha = .5
    ) +
    geom_line(aes(date, gwl), colour = "grey50") +
    geom_line(aes(date, mean), colour = "red") +
    geom_vline(
      data = test_split |>
        group_by(well_id) |>
        summarise(min = min(date)),
      aes(xintercept = lubridate::ymd(min)),
      linetype = "dashed",
      colour = "grey"
    ) +
    facet_wrap(~well_id, ncol = 1, scales = "free_y") +
    scale_x_date(
      date_breaks = "year", labels = scales::label_date("%Y"),
      minor_breaks = NULL
    ) +
    labs(y = "Groundwater level [m asl]") +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      axis.title.x = element_blank()
    )
}

make_plot_results_obs_and_preds_wtrain_full <- function(train_split, test_split, full) {
  train_split <- train_split |>
    reduce(bind_rows) |>
    mutate(split = "train")
  test_split <- test_split |>
    reduce(bind_rows) |>
    mutate(split = "test")
  full <- full |>
    reduce(bind_rows) |>
    mutate(split = "full")

  train_split |>
    bind_rows(test_split, full) |>
    select(well_id, date, gwl, .pred, split) |>
    group_by(date, well_id, gwl, split) |>
    summarise(
      lb = quantile(.pred, probs = c(0.05), na.rm = TRUE),
      ub = quantile(.pred, probs = c(0.95), na.rm = TRUE),
      mean = mean(.pred, na.rm = TRUE),
      .groups = "drop",
      split = unique(split)
    ) |>
    mutate(gwl = if_else(split == "full", NA_real_, gwl)) |>
    ggplot() +
    geom_ribbon(aes(date, ymin = lb, ymax = ub),
      fill = "orange", alpha = .5
    ) +
    geom_line(aes(date, gwl), colour = "grey50") +
    geom_line(aes(date, mean), colour = "red") +
    geom_vline(
      data = test_split |>
        group_by(well_id) |>
        summarise(min = min(date)),
      aes(xintercept = lubridate::ymd(min)),
      linetype = "dashed",
      colour = "grey"
    ) +
    geom_vline(
      data = full |>
        group_by(well_id) |>
        summarise(min = min(date)),
      aes(xintercept = lubridate::ymd(min)),
      linetype = "dashed",
      colour = "grey"
    ) +
    facet_wrap(~well_id, ncol = 1, scales = "free_y") +
    scale_x_date(
      date_breaks = "year", labels = scales::label_date("%Y"),
      minor_breaks = NULL,
      expand = c(0, 0)
    ) +
    labs(y = "Groundwater level [m asl]") +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      axis.title.x = element_blank()
    )
}

make_plot_results_obs_and_preds_train_test_pred_full <- function(test_split, full) {
  date_start_test <- test_split |>
    reduce(bind_rows) |>
    group_by(well_id) |>
    summarise(start = min(date))
  date_start_prediction <- test_split |>
    reduce(bind_rows) |>
    group_by(well_id) |>
    summarise(start = max(date))
  full <- full |>
    reduce(bind_rows)

  full |>
    select(well_id, date, gwl, .pred) |>
    group_by(date, well_id, gwl) |>
    summarise(
      lb = quantile(.pred, probs = c(0.05), na.rm = TRUE),
      ub = quantile(.pred, probs = c(0.95), na.rm = TRUE),
      mean = mean(.pred, na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(date_start_prediction, by = "well_id") |>
    mutate(gwl = if_else(date >= start, NA_real_, gwl)) |>
    ggplot() +
    geom_ribbon(aes(date, ymin = lb, ymax = ub),
      fill = "orange", alpha = .5
    ) +
    geom_line(aes(date, gwl), colour = "grey50") +
    geom_line(aes(date, mean), colour = "red") +
    geom_vline(
      data = date_start_test,
      aes(xintercept = start, colour = "Start test split"),
      linetype = "dashed"
      ) +
    geom_vline(
      data = date_start_prediction,
      aes(xintercept = start, colour = "Start prediction"),
      linetype = "dashed"
      ) +
    scale_colour_manual(values = c("grey60", "grey80"),
                        guide = guide_legend(reverse = TRUE)) +
    facet_wrap(~well_id, ncol = 1, scales = "free_y") +
    scale_x_date(
      date_breaks = "year", labels = scales::label_date("%Y"),
      minor_breaks = NULL,
      expand = c(0, 0)
    ) +
    labs(y = "Groundwater level [m asl]") +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      axis.title.x = element_blank()
    )
}

make_performance_table <- function(x) {
  x |>
    reduce(bind_rows) |>
    group_by(well_id) |>
    dplyr::summarise(
      nse = nse_vec(gwl, .pred),
      rmse = rmse_vec(gwl, .pred),
      rsq = rsq_vec(gwl, .pred),
      mae = mae_vec(gwl, .pred),
      mape = mape_vec(gwl, .pred)
    ) |>
    arrange(-nse)
}