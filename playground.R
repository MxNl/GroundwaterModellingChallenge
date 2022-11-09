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

tar_read(predictions_ensemble) |> 
  chuck(1) |> 
  nse(gwl, .pred)

tar_read(predictions_ensemble) |> 
  chuck(1) |> 
  pivot_longer(cols = all_of(c("gwl", ".pred"))) |> 
  ggplot(aes(date, value, colour = name)) +
  geom_line()
