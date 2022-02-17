
pcks <- c(
  "tidyverse", "here",
  "checkmate", "lubridate",
  "tictoc"
)
easypackages::libraries(pcks)

source(here("R/data-proc-rds.R"))
source(here("R/tidy-basin-data.R"))


nmme_cru_basin_data <- join_cru_nmme_basin_data(
  sp_average = "weighted",
  ext = "qs",
  var_name = "prec",
  stat = "identity",
  out_dir = here(glue::glue("output/{ext}/basin-avgs/{sp_average}"))
)

