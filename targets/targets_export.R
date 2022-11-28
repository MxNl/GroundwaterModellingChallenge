targets_export <- list(
  tar_target(
    submission_table,
    prepare_submission_table(
      predictions_ensemble_training_full,
      predictions_ensemble_test_full
    ) |>
      group_by(Location) |>
      group_split(),
    iteration = "list"
  ),
  tar_target(
    write_submission_tables,
    submission_table |>
      select(-Location) |>
      write_csv(
        here::here(
          "data_submission",
          str_glue("submission_form_{unique(submission_table$Location)}.csv")
        )
      ),
    pattern = map(submission_table)
  )
)