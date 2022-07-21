pcks <- c("terra", "tidyverse", "here", "checkmate", "metR", "fs", "glue", "qs")
easypackages::libraries(pcks)

## Funções para processamento dos dados netcdf----------------------------------
source(here("R/models-nmme.R"))
source(here("R/data-proc-nc.R"))
source(here("../proc-NMME/R/data-proc-rds.R"))

# lista de modelos e periodos---------------------------------------------------
nmme_models_span(by_type = TRUE, priority_type = "none")
nmme_models_span(by_type = TRUE, priority_type = "FORECAST")
# modelos que HIND e FORECAST para o mesmo ano
plot_nmme_models_span()
# NASA-GEOSS2S (2017)
# GFDL-SPEAR (2020)
# CanCM4i (2016:2018)



## Processamento para uma variavel e um modelo---------------------------------
var_name = "prec"
nc_dir = here::here("output", "ncdf")
model = "CanCM4i"


# count_ncs <- fs::dir_ls(nc_dir) %>%
#   fs::path_ext() %>%
#   table()

nc_files <- fs::dir_ls(path = nc_dir, 
                       glob = glue::glue("*{var_name}_{model}*.nc"), 
                       recurse = TRUE)

metadata_model_files <- nmme_metadata(nc_files, summary = TRUE) 
metadata_model_files_full <- nmme_metadata(nc_files) 

# arquivos não íntegros (sem as dimensões esperadas para um arquivo anual)
metadata_model_files_full  %>%
  dplyr::filter(year %in% year[!nc_integrity])

#browseURL("https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CanCM4i/.FORECAST/.MONTHLY/.prec/")



# como o nome dos arquivos especifica o nome dos modelos
#model_nm <- "CanCM4i"
(model_nms <- unique(model_counts$modelo))
#files_model <- nc_files[grep(model_nms, nc_files)]


# PAREI AQUI

# looping para processar dados 
# model_nms <- model_nms[c(2, 7)]
map(model_nms,
    function(imodel){
      # imodel = model_nms[1]
      cat(imodel, "-------\n", "\n")
      gc()
      proc_ncs_by_lt(model = imodel, 
                     variavel = "prec",
                     lead_time = seq(0.5, 11.5, by = 1), 
                     input_d = nc_dir,
                     output_d = here("output", "qs"),
                     overwrite = FALSE
                    )
    })
