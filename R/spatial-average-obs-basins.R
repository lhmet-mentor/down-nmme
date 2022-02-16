pcks <- c(
  "raster", "terra", "tidyverse", "here",
  "checkmate", "metR", "fs", "lubridate",
  "tictoc", "furrr"
)
easypackages::libraries(pcks)


source(here("R", "data-proc-basin.R"))


calc_basin_avg_cru(
  path_pols_bhs = here("input", "poligonos-bacias-incrementais.RDS"),
  path_obs_nc_file = "~/Dropbox/datasets/climate_datasets/superficie/CRU-TS4.04/cru_ts4.04.1901.2019.pre.dat.nc",
  ext = "qs",
  sp_average = "weighted",
  var_name = "pre"
)
