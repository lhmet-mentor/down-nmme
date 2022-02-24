
pcks <- c(
  "tidyverse", "here",
  "checkmate", "lubridate",
  "tictoc", "openair", "ggh4x"
)
easypackages::libraries(pcks)

source(here("R/data-proc-rds.R"))
source(here("R/tidy-basin-data.R"))
source(here("R/utils.R"))



nmme_cru_basin_data <- join_cru_nmme_basin_data(
  sp_average = "weighted",
  ext = "qs",
  var_name = "prec",
  stat = "identity"
)
#11.528 sec elapsed
nmme_cru_basin_data[["data"]][[1]]

(models_summary <- import_bin_file("output/qs/model_counts.qs"))
top6()






