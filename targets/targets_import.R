targets_import <- list(
  tar_target(
    data_all,
    import_data_allwells_github(PATH_GITHUB_GROUNDWATERCHALLENGE)
  ),
  if (SINGLE_WELL) {
    tar_target(
      data,
      data_all %>%
        slice_by_wellid(SINGLE_WELL_SITE) %>%
        group_by(well_id) %>%
        group_split(),
      iteration = "list"
    )
  },
  if (!SINGLE_WELL) {
    tar_target(
      data,
      data_all %>%
        group_by(well_id) %>%
        group_split(),
      iteration = "list"
    )
  },
  if (SINGLE_WELL) {
    tar_target(
      split_dates,
      SPLIT_DATES %>%
        mutate(across(starts_with("train_"), as_date)) %>%
        slice_by_wellid(SINGLE_WELL_SITE) %>%
        group_by(well_id) %>%
        group_split(),
      iteration = "list"
    )
  },
  if (!SINGLE_WELL) {
    tar_target(
      split_dates,
      SPLIT_DATES %>%
        mutate(across(starts_with("train_"), as_date)) %>%
        group_by(well_id) %>%
        group_split(),
      iteration = "list"
    )
  }
)
