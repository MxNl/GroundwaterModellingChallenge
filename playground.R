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