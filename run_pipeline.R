source("R/constants.R")

# tar_renv()

if(PARALLEL) {
  targets::tar_make_future(workers = ceiling(future::availableCores()*0.6))
} else {
  targets::tar_make()
}
# targets::tar_make()
message('targets::tar_visnetwork(label = c("time", "size"), targets_only = TRUE)')