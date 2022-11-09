targets_modelling_cl <- list(
  tar_target(
    fitted_models_cl,
    tune_resampling(
      workflow_set,
      resampling
    ),
    pattern = map(workflow_set, resampling),
    iteration = "list"
  ),
  tar_target(
    best_performance_cl,
    fitted_models_cl |>
      rank_results(rank_metric = "rsq", select_best = FALSE),
    pattern = map(fitted_models_cl),
    iteration = "list"
  ),
  tar_target(
    best_models_selection,
    best_performance_cl |>
      select_best_models(fitted_models_cl),
    pattern = map(best_performance_cl, fitted_models_cl),
    iteration = "list"
  ),
  tar_target(
    model_ensemble,
    stacks() |> stacks::add_candidates(best_models_selection),
    pattern = map(best_models_selection),
    iteration = "list"
  ),
  tar_target(
    blended_ensemble,
    model_ensemble |> 
      blend_predictions(),
    pattern = map(model_ensemble),
    iteration = "list"
  ),
  tar_target(
    fitted_ensemble,
    blended_ensemble |> 
      fit_members(),
    pattern = map(blended_ensemble),
    iteration = "list"
  ),
  tar_target(
    predictions_ensemble,
    testing(initial_split) %>%
      bind_cols(predict(fitted_ensemble, .)),
    pattern = map(initial_split, fitted_ensemble),
    iteration = "list"
  )
)