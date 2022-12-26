tar_read(data_no_empty_cols) %>%
  chuck(1) %>%
  pivot_longer(cols = -all_of(c("well_id", "date"))) %>%
  # group_by(name) |> 
  # group_split() |> 
  ggplot(aes(value, name)) +
  ggdist::stat_slabinterval(fill = "red", alpha = .5) +
  facet_wrap(~name, ncol = 3, scales = "free") +
  theme_minimal()

n_samples <- tar_read(data_modelling) %>%
  chuck(1) %>%
  nrow()


tar_read(data_modelling) %>%
  chuck(1) %>%
  modeltime.resample::time_series_split(initial = "9 years", assess = "5 years")

m750 <- m4_monthly %>% filter(id == "M750")
m750 %>%
  rename(asda = id) %>%
    time_series_split(
        initial = "12 years",
        assess  = "5 years"         # <- Returns 2nd slice, 3-years back
    )

m750 %>% pull(date) %>% range()


tar_read(best_performance_cl)

tar_read(best_workflow_cl) |> 
  chuck(1) |> 
  collect_predictions() |>
  # nse(gwl, .pred)
  pivot_longer(cols = all_of(c("gwl", ".pred"))) |> 
  ggplot(aes(.row, value, colour = name)) +
  geom_line()



tar_read(best_workflow_cl) |> 
  chuck(1) |> 
  collect_predictions() |>
  # nse(gwl, .pred)
  ggplot(aes(gwl, .pred)) +
  geom_point() +
  coord_equal()

fitted_models_cl <- tar_read(fitted_models_cl) |> chuck(1)
test <- stacks::stacks() |> 
  stacks::add_candidates(tar_read(best_performance_cl) |> chuck(1))


test_blend <- test |> 
  stacks::blend_predictions()

test_blend |> autoplot()


rnorm(100) |> 
  c(seq(1.5, 2.5, 0.01)) |> 
  as_tibble() |> 
  ggplot(aes(sample = value)) +
  stat_qq() +
  stat_qq_line()

tar_read(best_performance_cl) |> 
  chuck(1) |> 
  filter(.metric == "rsq") |> 
  group_by(model) |> 
  slice_head(n = 2)

tar_read(predictions_ensemble_test) |>
  chuck(1) |> 
  rsq(gwl, .pred)

tar_read(predictions_ensemble_test) |>
  reduce(bind_rows) |> 
  pivot_longer(cols = all_of(c("gwl", ".pred"))) |> 
  ggplot(aes(date, value, colour = name)) +
  geom_line() +
  scale_colour_discrete(
    labels = c("predicted", "observed"),
    guide = guide_legend(
    direction = "horizontal",
    reverse = TRUE
    )
    ) +
  labs(y = "Groundwater level [m asl]") +
  facet_wrap(~well_id, ncol = 1, scales = "free") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.title.x = element_blank()
  )

tar_read(predictions_ensemble_test) |>
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

fitted_models_cl <- tar_read(fitted_models_cl, branches = 1) |> 
  purrr::chuck(1)

fitted_models_cl |> 
  unnest(info) |> 
  unnest(result) |> 
  unnest(.notes) |> 
  slice(1) |> 
  pull(note)

test |> 
  chuck(1) |> 
  unnest(info) |> 
  # filter(row_number() == 2105) |> 
  pull(result) |>
  map_lgl(is_tibble) |> 
  magrittr::equals(FALSE) |> 
  which()
  magrittr::extract(2104:2106)
  unnest(result) |> 
  pull(result)
  unnest(.notes) |> 
  slice(1) |> 
  pull(note)

fitted_models_cl <- tar_read(fitted_models_cl, branches = 1)

fitted_models_cl |> 
  map(
    ~.x |> 
      unnest(info) |> 
      unnest(result) |> 
      unnest(.notes) |> 
      select(wflow_id, model, note) |> 
      distinct(note, model, .keep_all = TRUE)
      # slice(1:3) |>
      # pull(note) |> 
      # unique()
  ) |> chuck(1) |> View()

"A correlation computation is required, but `estimate` is constant and has 0 standard deviation, resulting in a divide by 0 error. `NA` will be returned."

test <- tar_read(recipe_lag_logtrans_linimp_norm_zv_corr_umap, branches = 1) |> 
  purrr::chuck(1) |>
  prep() |> 
  juice()
  purrr::map(recipes::prep) |> 
  purrr::map(juice)

test |> 
  map(
    ~ .x |> 
      filter(is.na(et) == TRUE) |>
      select(date, gwl, where(~.x |> is.na() |> any())) 
  )

test |> 
  map(
    ~.x |> select(well_id, date, gwl, everything())
  )

tar_read(data_modelling) |> 
  chuck(5) |> 
  filter(date %in% lubridate::ymd(c("2004-01-10")))

{test |> 
  chuck(1) |> 
  select(date, contains("tg")) |> 
  pivot_longer(cols = -date) |> 
  ggplot(aes(date, value, group = name, colour = name)) +
  geom_line() +
    geom_point()
  } |> 
  plotly::ggplotly()

test |> 
  # magrittr::extract(3:4) |> 
  reduce(bind_rows) |> 
  select(well_id, date, gwl) |> 
  drop_na(gwl) |> 
  ggplot(aes(date, gwl, group = well_id)) +
  geom_line() +
  facet_wrap(~well_id, ncol = 1, scales = "free")

tar_read(data_no_empty_cols) |> 
  reduce(bind_rows) |> 
  select(well_id, date, gwl) |> 
  drop_na(gwl) |> 
  ggplot(aes(date, gwl, group = well_id)) +
  geom_line() +
  facet_wrap(~well_id, ncol = 1, scales = "free")

tar_read(data_no_empty_cols) |> 
  chuck(3) |> View()

test <- tar_read(recipe_wolag_logtrans_linimp_norm_zv_augmdate_corr_pca_lag) |>
  map(prep) |> 
  map(juice)

test <- tar_read(data)

recipe_test <- recipe(gwl ~ ., test |> chuck(2)) |> 
  step_log(et, rr, offset = 1E-5) |> 
  step_normalize(all_numeric_predictors())

recipe_test |> 
  prep() |> 
  juice() |> 
  # chuck(1) |> 
  # select(well_id, date, rr) |>
  # group_by(well_id) |> 
  # mutate(rr = log(rr)) |> 
  pivot_longer(cols = -all_of(c("well_id", "gwl", "date"))) |> 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol = 2, scales = "free")



tar_read(recipe_wolag_logtrans_linimp_norm) |> chuck(1) |> prep()
tar_read(recipe_wolag_logtrans_linimp_norm_zv_augmdate_corr_pca) |> chuck(1) |> prep()


{tar_read(data_modelling) |> 
  chuck(4) |> 
  mutate(
    gwl_na = is.na(gwl),
    gwl = imputeTS::na_interpolation(gwl, option = "spline")) |> 
  ggplot(aes(date, gwl, colour = gwl_na)) +
  geom_line()} |> 
  plotly::ggplotly()

{tar_read(data_no_empty_cols) |> 
  chuck(4) |> 
    drop_na(gwl) |> 
    # mutate(et = if_else(et < 0, 0, et)) |> 
    # mutate(gwl = imputeTS::na_interpolation(gwl, option = "linear")) |> 
  ggplot(aes(date, gwl)) +
  geom_line()} |> 
  plotly::ggplotly()


test <- tar_read(fitted_ensemble) |> 
  chuck(1) 

tar_read(initial_split) |> chuck(1) |> training()

{test$data_stack |> 
  mutate(row = row_number()) |> 
  pivot_longer(cols = -all_of(c("row"))) |> 
  ggplot(aes(row, value, colour = name, group = name)) +
  geom_line()} |> plotly::ggplotly()


recipe_spec <- recipe(gwl~., tar_read(initial_split) |> chuck(1) |> training() |> select(gwl, rr, tg)) %>%
  timetk::step_slidify_augment(
    rr, tg,
    period = c(2, 4, 5),
    .f = ~ mean(.x),
    align = "right",
    partial = TRUE
  )

recipe_spec |> 
  prep() |> 
  juice()



tar_read(data_corrected, branches = 1) |> 
  chuck(1) |>
  mutate(across(any_of(c("tn")), slider::slide_mean, before = 7, .names = "{.col}_previous"))


#Extract fitted data
test <- tar_read(fitted_ensemble, branches = 1) |> 
  chuck(1)
tar_read(initial_split, branches = 1) |> chuck(1)
tar_read(resampling, branches = 1) |> chuck(1)
test$data_stack |> nrow()

test |> stacks::axe_fitted()
test |> butcher::weigh()
test

test$member_fits$recipe_1_svm_rbf_629_1_1$fit$fit$fit
test$member_fits$recipe_3_nnetar_reg_720_1_1$fit$fit$fit$data |> 
  pivot_longer(cols = -c(date, .residuals)) |> 
  ggplot() +
  geom_line(aes(date, value, colour = name))

test2 <- test |> 
  magrittr::extract2("member_fits") |> 
  map(magrittr::extract2, "fit") |> 
  map(magrittr::extract2, "fit") |> 
  map(magrittr::extract2, "fit") |> 
  magrittr::extract(2:6) |>
  map(magrittr::extract2, "data")
  
# test3 <- test |>
#   magrittr::extract2("member_fits") |>
#   chuck(1) |>
#   extract_fit_parsnip()

tar_read(predictions_train_test_pred_full) |> 

test2$recipe_1_svm_rbf_629_1_1

########
test$member_fits

test |> 
  magrittr::extract2("data_stack") |> 
  select(gwl, all_of(members)) |> 
  mutate(n = row_number()) |> 
  pivot_longer(cols = -all_of(c("n", "gwl"))) |> 
  ggplot() +
    geom_line(aes(n, gwl)) +
    geom_line(aes(n, value, colour = name, group = name)) +
  facet_wrap(~name, ncol = 1)

tar_read(initial_split, branches = 1) |> 
  chuck(1) |> 
  testing()

test <- tar_read(fitted_models_cl, branches = 5) |> 
  chuck(1)

test |> 
  filter(str_detect(member, "recipe_3_rand_forest_11_1_1"))


tar_read(blended_ensemble)
tar_read(resampling, branches = 1)
tar_read(predictions_ensemble_test, branches = 1)
tar_read(data_previous_week_predictors)
tar_read(data_no_empty_cols) |> map(~.x |> drop_na(gwl))
tar_read(data_all)
tar_read(plot_resamplingstrategy)
tar_read(recipe_wolag_logtrans_linimp_norm, branches = 1) |> chuck(1) |> prep() |> juice() |>  select(date)
tar_read(recipe_wolag_logtrans_linimp_norm_zv_augmdate_corr_pca, branches = 1) |> chuck(1) |> prep() |> juice() |>  select(date)
tar_read(plot_results_obs_and_preds)
tar_read(plot_results_obs_and_preds_wtrain)
tar_read(plot_results_obs_and_preds_wtrain_full)
tar_read(plot_results_obs_and_preds_train_test_pred_full)
tar_read(plot_results_obs_and_preds_train_test_pred_full) |> plotly::ggplotly()
tar_read(recipe_wolag_logtrans_linimp_norm_zv_corr)
tar_read(performance_table_training)
tar_read(performance_table) 
tar_read(fitted_ensemble)
test <- tar_read(fitted_models_cl, branches = 1)


tar_read(data_train_test_pred_full_repeats, branches = 31:40) |> 
  chuck(1) |> View()


showtext::showtext_opts(dpi = 300)
{tar_read(plot_results_obs_and_preds_train_test_pred_full) +
  theme(text = element_text(size = 16))} |> 
  ggplot2::ggsave(
    filename = "data_submission/plot_results_obs_and_preds_train_test_pred_full2.pdf", device = "pdf",
    scale = 3, units = "cm", width = 18, height = 12, dpi = 300
  )
showtext::showtext_opts(dpi = 96)

pivot_longer(-well_id) |> 
  group_by(well_id)
  # GGally::ggpairs(
  #   # aes(colour = well_id),
  #   columns = 2:6
  #   )
  ggplot(aes(value, value)) +
  geom_point() +
  facet_grid(name ~ name)

  tar_read(data) |> 
    map_at(
      c(3, 4),
      ~ .x |> 
        mutate(
          across(c(everything(), -rr), 
                 slider::slide_mean, before = 7, after = 0, step = 7, na_rm = TRUE)
        )
      )
  
  restult <- tar_read(data) |> 
    chuck(3)

    tar_read(data) %>%
    map_if(
      .p = ~.x |> drop_na(gwl) |>
        pull(date) |> 
        padr::get_interval() |> 
        magrittr::equals("week"),
      .f = aggregate_to_gwl_interval
    )

  tar_read(data) %>%
    chuck(1) |> 
    mutate_if(
      .predicate = ~.x |> drop_na(gwl) |>
        pull(date) |> 
        padr::get_interval() |> 
        magrittr::equals("week"),
      .f = aggregate_to_gwl_interval
    )
    
  tar_read(data) |> 
    chuck(3) |> 
    drop_na(gwl) |>
    pull(date) |> 
    padr::get_interval()

  
  tar_read(initial_split) |> 
    map(training) 
  
  tar_read(initial_split) |> 
    chuck(1) |> 
    training() %>%
    recipe(gwl ~ ., data = .) |>
    update_role(well_id, new_role = "id") |>
    step_lag(contains("previous_week"), lag = seq(7, 7 * 25, 7)) |> 
    prep() |> 
    juice() |> View()
  
  
  tar_read(data_previous_week_predictors, branches = 1) |> 
    chuck(1)
    
  test |> magrittr::extract(10:11)
  
  tar_read(initial_split) |> 
    chuck(1) |> 
    list() |> 
    list() |> 
    map(rep, each = 2) |> 
    unlist(recursive = FALSE)
    # unlist(recursive = FALSE)
  
test <- tar_read(predictions_ensemble_test) |>
  reduce(bind_rows) |> 
  mutate(split = "test")
test_cal <- tar_read(predictions_ensemble_training) |>
  reduce(bind_rows) |> 
  mutate(split = "train")


test |>
  group_by(well_id) |>
  summarise(
    start = min(date),
    end = max(date)
  )
test_cal |>
  group_by(well_id) |>
  summarise(
    start = min(date),
    end = max(date)
  )
SPLIT_DATES

plot <- test_cal |> 
  bind_rows(test) |> 
  # group_by(well_id) |> 
  # summarise(
  #   start = min(date),
  #   end = max(date)
  # )
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
              fill = "orange", alpha = .5) +
  geom_line(aes(date, gwl), colour = "grey50") +
  geom_line(aes(date, mean), colour = "red") +
  geom_vline(
    data = test |> group_by(well_id) |> summarise(min = min(date)), 
    aes(xintercept = lubridate::ymd(min)),
    linetype = "dashed",
    colour = "grey"
    ) +
  facet_wrap(~ well_id, ncol = 1, scales = "free_y") +
  scale_x_date(date_breaks = "year", labels = scales::label_date("%Y"),
               minor_breaks = NULL) +
  theme_minimal()

plot

plot |> plotly::ggplotly()



test_cal |>
  bind_rows(test) |>
  select(well_id, date, gwl, .pred, split) |>
  group_by(date, well_id, gwl, split) |>
  summarise(
    lb = quantile(.pred, probs = c(0.05), na.rm = TRUE),
    ub = quantile(.pred, probs = c(0.95), na.rm = TRUE),
    mean = mean(.pred, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    x = if_else(str_detect(well_id, "Sweden"), lubridate::week(date), lubridate::yday(date)),
    y = lubridate::year(date)
  ) |>
  group_by(well_id) |>
  group_split() |>
  map(~ .x |>
    ggplot(aes(x, y, fill = mean - gwl)) +
    geom_tile(
      colour = NA
    ) +
    scale_y_reverse() +
    scico::scale_fill_scico(
      palette = "vik",
      midpoint = 0,
      guide = guide_colourbar(
        direction = "horizontal",
        barheight = unit(2, "mm"),
        title.position = "top"
      )
    ) +
    facet_wrap(~well_id) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      axis.title = element_blank()
    )) |>
  patchwork::wrap_plots() +
  patchwork::plot_layout() +
  patchwork::plot_annotation(title = "Residuals") &
  theme(plot.title = element_text(hjust = 0.5))


test_cal |>
  bind_rows(test) |>
  select(well_id, date, gwl, .pred, split) |>
  group_by(date, well_id, gwl, split) |>
  summarise(
    residuals = mean(.pred, na.rm = TRUE) - gwl,
    .groups = "drop"
  ) |>
  ggplot(aes(residuals)) +
  geom_histogram(
    colour = NA,
    fill = "steelblue3",
    alpha = .5
  ) +
  facet_wrap(~well_id, scales = "free") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.title = element_blank()
  )


tar_read(predictions_ensemble_training_full) |> 
  reduce(bind_rows) |> 
  group_by(well_id) |> 
  summarise(start = min(date),
            end = max(date))
tar_read(predictions_ensemble_test_full) |> 
  reduce(bind_rows) |> 
  drop_na(gwl) |>
  group_by(well_id) |> 
  summarise(start = min(date),
            end = max(date))



# Check start dates Sweden
train <- tar_read(data_modelling) |> reduce(bind_rows) |> mutate(split = "train")
pred <- tar_read(data_prediction) |> reduce(bind_rows) |> mutate(split = "pred")

train |> 
  bind_rows(pred) |> 
  select(well_id, date, split, gwl) |>
  filter(str_detect(well_id, "Sweden")) |> 
  group_by(well_id, split) %>%
  slice(c(1:2, (n() - 1):n())) |> 
  arrange(well_id, split, date) |> 
  group_split()

tar_read(data, branches = 3) |> 
  chuck(3) |>
  filter(date >= lubridate::ymd("2000-12-20")) |> View()

tar_read(data_native_gwl_interval, branches = 3) |> 
  chuck(3) |> View()

tar_read(data, branches = 4) |> 
  chuck(4) |> View()
  filter(date >= lubridate::ymd("2000-12-20")) |>
  mutate(date_orig = date, .after = date) |> 
  # summarise(
  #   well_id = slider::slide_chr(well_id, .f = unique, .before = 6, .after = 0),
  #   across(c(date_orig, date, gwl), ~ .x |> slider::slide_vec(.f = last, .before = 6, .after = 0)),
  #   across(c(everything(), -rr, -well_id, -date_orig, -date, -gwl), slider::slide_mean, before = 6, after = 0, na_rm = TRUE),
  #   rr = slider::slide_sum(rr, before = 6, after = 0, na_rm = TRUE)
  #   ) |>
  # mutate(across(c(everything(), -rr, -well_id, -date_orig, -date), slider::slide_mean, before = 6, after = 0, na_rm = TRUE)) |>
  # mutate(across(rr, slider::slide_sum, before = 6, after = 0, na_rm = TRUE)) |>
  # padr::thicken(interval = "week", by = "date", start_val = lubridate::ymd("2001-01-02")) |>
  # relocate(well_id, date_week) |> 
  mutate(date = lubridate::ceiling_date(date, "week", week_start = -5, change_on_boundary = FALSE), .after = well_id) |>
  # mutate(date = lead(date, 6)) |> 
  print(n = 20) |> 
  View()

tar_read(data_train_test_pred_full_repeats, branches = 31:40) |> 
  chuck(1) |> View()

make_targets_runtime_table <-
  function() {
    # branch_seconds <-
      targets::tar_meta() %>%
      select(name, type, bytes, seconds, parent) %>%
      drop_na(parent) %>%
      group_by(parent) %>%
      summarize(sum_seconds = sum(seconds))
    
    targets::tar_meta() %>%
      select(name, type, bytes, seconds, parent) %>%
      arrange(-seconds) %>%
      filter(!type %in% c("function", "object")) %>%
      left_join(branch_seconds, by = c("name" = "parent")) %>%
      mutate(seconds = if_else(is.na(sum_seconds), seconds, sum_seconds)) %>%
      mutate(
        minutes = seconds / 60,
        hours = minutes / 60,
        days = hours / 24,
        mb = bytes / 1E6
      ) %>%
      mutate(across(c("seconds", "minutes", "hours", "days", "mb"), round, 1)) %>%
      rename("target name" = name) %>%
      select(-parent, -sum_seconds, -type, -bytes) %>%
      set_names(str_to_sentence(names(.))) %>%
      janitor::adorn_totals("row")
  }

make_targets_runtime_table() |> 
  filter()

tar_meta(targets_only = TRUE) |>
  filter(time >= lubridate::ymd("2022-11-23")) |> 
  select(name, type, bytes, seconds, parent, command, depend) |>
  drop_na(parent) |> 
  mutate(target_group = str_c(command))
  group_by(target_group) %>%
  mutate(pattern_groups = 1:n()) |>
  summarise(max = max(pattern_groups)) %>%
  print(n = nrow(.))
  mutate(target_groups = if_else(
    n() > 5,
    rep(1:(n() / 5), each = (n() / 5)),
    row_number()
  ))
  
  arrange(-seconds) |> mutate(seconds = seconds / 60 / 60) |>  View()

  
  
  
  