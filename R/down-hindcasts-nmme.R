pcks <- c("terra", 
          "tidync", 
          "tidyverse", 
          "fs", 
          "tictoc", 
          "data.table",
          "here",
          "metR"
          )

easypackages::libraries(pcks)
options(timeout = 150)

#------------------------------------------------------------------------------
# scripts
source(here("R/models-nmme.R"))
#names_vars_models()
source(here("R/down-nmme.R"))
# para import/export_bin_file()
source("https://raw.githubusercontent.com/lhmet-mentor/proc-nmme/main/R/data-proc-rds.R")

#------------------------------------------------------------------------------
# lista de args para down_nmme_by_ymv() na ordem exigida pela funcao
args_l <- tab_mod_year_vname_type %>%
  dplyr::filter(type == "FORECAST", vname_ref == "prec") 

#View(args_l)
#info_nmme <- import_bin_file(here("output/qs/model_counts.qs"))
#info_nmme

baixados_temp <- purrr::pmap(as.list(args_l),
                             function(year, model, vname_ref, type) {
                               down_nmme_by_ymv(year, model, vname_ref, type)
                             }
)

export_bin_file(baixados_temp, here("output/"))