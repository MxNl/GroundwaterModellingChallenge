# Program Configuration
YML_CONFIG <- yaml::read_yaml('config.yml')

PARALLEL <- purrr::chuck(YML_CONFIG, "parallel")
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
