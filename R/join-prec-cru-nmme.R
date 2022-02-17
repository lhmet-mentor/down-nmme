
pcks <- c(
  "tidyverse", "here",
  "checkmate", "lubridate",
  "tictoc"
)
easypackages::libraries(pcks)

source(here("R/data-proc-rds.R"))

sp_average = "weighted"
ext = "qs"
var_name = "prec"
stat = "identity"
out_dir = here(glue::glue("output/{ext}/basin-avgs/{sp_average}"))



source(here("R/tidy-basin-data.R"))

# nmme -------------------------------------------------------------------------
nmme_basin_data <- tidy_nmme_basin_data(
  var_name = "prec",
  out_dir = here(glue::glue("output/{ext}/basin-avgs/{sp_average}")),
  sp_average = "weighted",
  ext = "qs"
)



# cru obs ----------------------------------------------------------------
cru_basin_data <- tidy_cru_basin_data(
  var_name = "prec",
  out_dir = here(glue::glue("output/{ext}/basin-avgs/{sp_average}")),
  sp_average = "weighted",
  ext = "qs"
){
  
  basin_avg_cru_file <- here(
    out_dir,
    glue::glue("cru-{var_name}-basins-{sp_average}-avg.{ext}")
  )  
  checkmate::assert_file_exists(basin_avg_cru_file)
}
  


cru_basin_data <- import_bin_file(basin_avg_cru_file) %>%
  dplyr::mutate(
    ID = NULL,
    # ajuste das datas, pq no CRU as datas sao do meio do mes
    date = lubridate::floor_date(date, unit = "months")
  )
str(cru_basin_data)
summary(cru_basin_data)

# check how to join-----
pred <- nmme_basin_data[["data"]][[1]]
#summary(pred)
#dplyr::arrange(pred, codONS, member)

comb_nmme_obs <- dplyr::inner_join(pred, cru_basin_data,
  by = c("date", "codONS")
)
# filter(comb_nmme_obs, codONS == 1, date == as_date("2000-12-01"), L == 1.5) %>%
#   as.data.frame()
# tail(as.data.frame(comb_nmme_obs), 30)


# Combinacao de dados prec Obs e Model-----------------------------------------
prec_nmme_avg_basin <- prec_nmme_avg_basin %>%
  mutate(data = map(
    data,
    ~ .x %>%
      dplyr::inner_join(
        prec_cru_avg_basin,
        by = c("date", "codONS")
      )
  ))
prec_nmme_avg_basin[["data"]][[8]]

readr::write_rds(prec_nmme_avg_basin,
  file = here(
    out_dir,
    str_replace(
      "nmme-cru-mly-average_type-avg-basins-ons.RDS",
      "average_type",
      sp_average
    )
  )
)
summary(prec_nmme_avg_basin[["data"]][[1]])
