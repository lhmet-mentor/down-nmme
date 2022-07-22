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
options(timeout = 600)

#------------------------------------------------------------------------------
# scripts
source(here("R/models-nmme.R"))
#names_vars_models()
source(here("R/down-nmme.R"))
# para import/export_bin_file()
source(here("../proc-NMME/R/data-proc-rds.R"))

#------------------------------------------------------------------------------
# lista de args para down_nmme_by_ymv() na ordem exigida pela funcao
# nmme_models_span()
# nmme_models_span(by_type = TRUE)
# names_vars_models()

args_l <- tab_mod_year_vname_type(priority = "none")  %>% 
  dplyr::filter(vname_ref == "tmax", model == "CanCM4i", year == 2000)


baixados <- purrr::pmap(as.list(args_l),
                             function(year, model, vname_ref, type) {
                               down_nmme_by_ymv(year, model, vname_ref, type, overwrite = TRUE)
                             }
)

baixados
#export_bin_file(unlist(baixados_temp), here("output/qs/lista-prec-forecast"))