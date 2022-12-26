# Program Configuration
YML_CONFIG <- yaml::read_yaml('config.yml')

PARALLEL <- purrr::chuck(YML_CONFIG, "parallel")
CORES <- purrr::chuck(YML_CONFIG, "cores")
PATH_GITHUB_GROUNDWATERCHALLENGE <- "https://api.github.com/repos/gwmodeling/challenge/git/trees/main?recursive=1"
PATH_GITHUB_REPO <- "https://github.com/gwmodeling/challenge/"
SPLIT_DATES <- tibble::tribble(
  ~well_id, ~train_start, ~train_end,
  "Germany", "2002-05-01", "2016-12-31",
  "Netherlands", "2000-01-01", "2015-09-10",
  "Sweden_1", "2001-01-02", "2015-12-29",
  "Sweden_2", "2001-01-02", "2015-12-29",
  "USA", "2002-03-01", "2016-12-26"
)
SINGLE_WELL <- purrr::chuck(YML_CONFIG, "single_well")
INITIAL_PROP <- purrr::chuck(YML_CONFIG, "initial_prop")
CV_INITIAL <- purrr::chuck(YML_CONFIG, "cv_initial")
CV_ASSESS <- purrr::chuck(YML_CONFIG, "cv_assess")
CV_SKIP <- purrr::chuck(YML_CONFIG, "cv_skip")
CV_SLICE_MAX <- purrr::chuck(YML_CONFIG, "cv_slice_max")
REG_CV_FOLDS <- purrr::chuck(YML_CONFIG, "reg_cv_folds")
REG_CV_REPEATS <- purrr::chuck(YML_CONFIG, "reg_cv_repeats")
CV_LAG <- purrr::chuck(YML_CONFIG, "cv_lag") |> stringr::str_split(", ", simplify = TRUE) |> as.numeric()
ALLOW_PAR <- purrr::chuck(YML_CONFIG, "allow_par")
CORES <- purrr::chuck(YML_CONFIG, "cores")
HYPPAR_COMBINATIONS <- purrr::chuck(YML_CONFIG, "hyppar_combinations")
HYPPAR_LEVELS <- purrr::chuck(YML_CONFIG, "hyppar_levels")
SINGLE_WELL_SITE <- purrr::chuck(YML_CONFIG, "single_well_site")
REPEATS <- purrr::chuck(YML_CONFIG, "repeats")
