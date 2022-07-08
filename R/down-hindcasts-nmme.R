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


#------------------------------------------------------------------------------
# lista de args para down_nmme_by_ymv() na ordem exigida pela funcao
args_l <- tab_mod_year_vname_type[c(1594, 1675),] #%>%
  # dplyr::filter(type == "FORECAST", vname_ref == "prec")

baixados_temp <- purrr::pmap(args_l,
                             function(year, model, vname_ref, type) {
                               down_nmme_by_ymv(year, model, vname_ref, type)
                             }
)