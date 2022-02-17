
pcks <- c("tidyverse", "here", 
          "checkmate", "lubridate",
          "tictoc")
easypackages::libraries(pcks)

source(here("R/data-proc-rds.R"))

sp_average = "weighted"
ext = "qs"
var_name = "prec"
stat = "identity"
out_dir = here(glue::glue("output/{ext}/basin-avgs/{sp_average}"))
basin_avg_nmme_file = here(out_dir, 
                           glue::glue("nmme-models-{sp_average}-avg-basins-ons.{ext}")
                      )



# --- nmme 
models_summary <- import_bin_file(glue::glue("output/{ext}/model_counts.{ext}"))

prec_nmme_avg_basin <- import_bin_file(basin_avg_nmme_file) %>%
  mutate(data = map(
    data, ~ .x %>%
      rename("date" = date_lead)
  ))

# prec_nmme_avg_basin[["data"]][[1]] # wide

prec_nmme_avg_basin <- prec_nmme_avg_basin %>%
  inner_join(dplyr::select(models_summary, modelo, M), 
             by = c("model" = "modelo")) %>%
  dplyr::rename("n_members" = "M")


if (stat == "identity") {
  # pivota membros nas linhas
  tictoc::tic()
  prec_nmme_avg_basin_long <- prec_nmme_avg_basin %>%
    mutate(data = map(
      data,
      ~ .x %>%
        pivot_longer(
          cols = contains("prec"),
          names_to = "member", 
          names_prefix = "prec_", 
          names_transform = list(member = as.integer),
          values_to = "prec_model"
        ) %>%
        mutate(prec_model = prec_model * 30) #%>% # 30 eh o num de dias no calendario dos modelos
        #arrange(S, L, codONS, member)
    ))
  tictoc::toc()
  
} else {
  prec_nmme_avg_basin_long <- prec_nmme_avg_basin %>%
    mutate(data = map(
      data,
      ~ .x %>%
        rename("prec_model" = prec) %>%
        mutate(prec_model = prec_model * 30,
               member = 0L
               ) #%>% # 30 eh o num de dias no calendario dos modelos
        #arrange( S, L, codONS, member)
    )
    ) 
}
  




# obs ----------------------------------------------------------------
basin_avg_cru_file <- here(out_dir, 
                           glue::glue("cru-prec-basins-{sp_average}-avg.{ext}")
                           )

prec_cru_avg_basin <- import_bin_file(basin_avg_cru_file) %>%
  mutate(
    ID = NULL,
    # ajuste das datas
    date = lubridate::floor_date(date, unit = "months")
  )
str(prec_cru_avg_basin)
summary(prec_cru_avg_basin)

# check how to join-----
pred <- prec_nmme_avg_basin[["data"]][[1]]
summary(pred)
comb_nmme_obs <- dplyr::inner_join(pred, prec_cru_avg_basin,
                                   by = c("date", "codONS")
                                   )
filter(comb_nmme_obs, codONS == 1, date == as_date("1991-01-01"), L == 0.5) %>%
  as.data.frame()
tail(as.data.frame(comb_prec_obs), 30)


# Combinacao de dados prec Obs e Model-----------------------------------------
prec_nmme_avg_basin <- prec_nmme_avg_basin %>%
  mutate(data = map(data, 
                    ~.x %>% 
                      dplyr::inner_join(
                        prec_cru_avg_basin, 
                        by = c("date", "codONS")
                      )
                    )
         )
prec_nmme_avg_basin[["data"]][[8]]

readr::write_rds(prec_nmme_avg_basin, 
                 file = here(out_dir, 
                             str_replace("nmme-cru-mly-average_type-avg-basins-ons.RDS",
                                         "average_type",
                                         sp_average
                                         )
                 ))
summary(prec_nmme_avg_basin[["data"]][[1]])
