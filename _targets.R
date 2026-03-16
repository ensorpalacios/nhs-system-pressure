#' _targets.R file
#' 
#' @author Ensor Palacios, email{erp65@bath.ac.uk}
#' @date 2025-12-03

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.
tar_source("src/packages.R")
tar_source("target/R/target_functions.R")
tar_source("target/R/target_helper.R")
tar_source("target/R/target_global_var.R")

# # Set target options:
# tar_option_set(
  # packages = c("tibble") # Packages that your targets need for their tasks.
#   # format = "qs", # Optionally set the default storage format. qs is fast.
#   #
#   # Pipelines that take a long time to run may benefit from
#   # optional distributed computing. To use this capability
#   # in tar_make(), supply a {crew} controller
#   # as discussed at https://books.ropensci.org/targets/crew.html.
#   # Choose a controller that suits your needs. For example, the following
#   # sets a controller that scales up to a maximum of two workers
#   # which run as local R processes. Each worker launches when there is work
#   # to do and exits if 60 seconds pass with no tasks to run.
#   #
#   #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
#   #
#   # Alternatively, if you want workers to run on a high-performance computing
#   # cluster, select a controller from the {crew.cluster} package.
#   # For the cloud, see plugin packages like {crew.aws.batch}.
#   # The following example is a controller for Sun Grid Engine (SGE).
#   #
#   #   controller = crew.cluster::crew_controller_sge(
#   #     # Number of workers that the pipeline can scale up to:
#   #     workers = 10,
#   #     # It is recommended to set an idle time so workers can shut themselves
#   #     # down if they are not running tasks.
#   #     seconds_idle = 120,
#   #     # Many clusters install R as an environment module, and you can load it
#   #     # with the script_lines argument. To select a specific verison of R,
#   #     # you may need to include a version string, e.g. "module load R/4.3.2".
#   #     # Check with your system administrator if you are unsure.
#   #     script_lines = "module load R"
#   #   )
#   #
#   # Set other options as needed.
# )

# tar_source("other_functions.R") # Source other scripts as needed.


# Check paths
save_path <- here("target/data")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}


# Data paths
path_weight <- 
  "target/data/weights_training.RDS"
ls_model_comb <-
  list(
    "BRI" = 
      c("arima_dadpl_rec", "arima_dadp_rec", "rf_int", 
        "var_paed", "var_ad2", "xgb"),
    "Southmead" = 
      c("arima_dadpl_rec", "arima_dadp_rec", "rf_int", 
        "var_paed", "var_h", "xgb")
  )


# Target list
list(
  tar_target(
    data_hosp,
    load_hosp(),
    format = "file"
  ),
  tar_target(
    data_weights,
    path_weight,
    format = "file"
  ),
  tar_target(
    weights,
    readRDS(data_weights)
  ),
  tar_target(
    ts_file,
    prepare_data(data_hosp)
  ),
  tar_target(
    thr_file,
    compute_threshold(ts_file)
  ),
  tar_target(
    fc_file,
    forecast_occ(ts_file)
  ),
  tar_target(
    fcc_file,
    fc_combination(fc_file, weights, ls_model_comb)
  ),
  tar_target(
    risk_file,
    compute_risk(fcc_file, thr_file)
  ),
  tar_target(
    output_file_pred,
    prepare_output_pred(risk_file),
    format = "file"
  ),
  tar_target(
    output_file_hist,
    prepare_output_hist(ts_file),
    format = "file"
  )
)
