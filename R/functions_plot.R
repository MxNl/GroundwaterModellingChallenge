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
    geom_ribbon(aes(date, ymin = lb, ymax = ub, fill1 = "95% confidence interval"), 
                alpha = .5
    ) %>% 
    relayer::rename_geom_aes(new_aes = c("fill" = "fill1")) +
    geom_line(aes(date, gwl, colour1 = "Observed")) %>% 
    relayer::rename_geom_aes(new_aes = c("colour" = "colour1")) +
    geom_line(aes(date, mean, colour2 = "Predicted")) %>% 
    relayer::rename_geom_aes(new_aes = c("colour" = "colour2")) +
    geom_vline(
      data = date_start_test,
      aes(xintercept = start, colour3 = "Start test split"),
      linetype = "dashed"
      ) %>% 
    relayer::rename_geom_aes(new_aes = c("colour" = "colour3")) +
    geom_vline(
      data = date_start_prediction,
      aes(xintercept = start, colour3 = "Start prediction"),
      linetype = "dashed"
      ) +
    scale_colour_manual(aesthetics = "fill1", values = c("orange")) +
    scale_colour_manual(aesthetics = "colour1", values = c("grey50")) +
    scale_colour_manual(aesthetics = "colour2", values = c("red")) +
    scale_colour_manual(aesthetics = "colour3", values = c("grey60", "grey80"),
                        guide = guide_legend(reverse = TRUE)) +
    facet_wrap(~well_id, ncol = 1, scales = "free_y") +
    scale_x_date(
      date_breaks = "year", labels = scales::label_date("%Y"),
      minor_breaks = NULL,
      expand = c(0, 0)
    ) +
    labs(y = "Groundwater level [m asl]") +
    guides(
      colour1 = guide_legend(order = 1),
      colour2 = guide_legend(order = 2),
      fill1 = guide_legend(order = 3)
      ) +
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