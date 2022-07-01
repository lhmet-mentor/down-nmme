pcks <- c("terra", 
          "tidync", 
          "tidyverse", 
          "fs", 
          "tictoc", 
          "data.table",
          "here")

easypackages::libraries(pcks)
#options(timeout = 150)

#------------------------------------------------------------------------------
# scripts
source(here("R/models-nmme.R"))
#names_vars_models()
source(here("R/down-nmme.R"))


#------------------------------------------------------------------------------
# lista de args para down_nmme_by_ymv() na ordem exigida pela funcao
args_l <- c(tab_mod_year_vname_type[])
baixados_temp <- purrr::pmap(args_l,
                             function(year, model, variable, type) {
                               down_nmme_by_ymv(year, model, variable, type)
                             }
)