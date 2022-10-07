# targets_modelling <- list(
#   tar_target(
#     control_grid_stack,
#     make_stack_control_grid()
#   ),
#
#   tar_target(
#     train_test_split,
#     make_train_test_split(
#       data_features_target
#     ),
#     pattern = map(data_features_target),
#     iteration = "list"
#   ),
#
#   tar_target(
#     resampling_strategy_cv,
#     make_resampling_strategy(
#       train_test_split
#     ),
#     pattern = map(train_test_split),
#     iteration = "list"
#   ),
#
#   tar_target(
#     preprocessing_recipe,
#     make_recipe(
#       train_test_split
#     ),
#     pattern = map(train_test_split),
#     iteration = "list"
#   )
# )