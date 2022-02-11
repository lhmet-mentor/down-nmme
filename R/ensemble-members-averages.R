

pcks <- c("raster", "terra", "tidyverse", "here", 
          "checkmate", "metR", "fs", "lubridate",
          "tictoc")
easypackages::libraries(pcks)


source(here("R", "data-proc-rds.R"))

##------------------------------------------------------------------------------


path_rds_files <- here("output", "rds")
path_qs_files <- here("output", "qs")

model_counts <- readr::read_rds(here(path_rds_files, "model_counts.RDS"))
models <- model_counts$modelo

# files_bin <- dir_ls(
#   path_qs_files, 
#   regexp = "nmme_prec.*_lt[0-9]{1,2}\\.[0-9]{1}"
#   )
# 

#-------------------------------------------------------------------------------
# selecao de files dos modelos acrescentados
# files_bin <- files_bin %>%  grep("CanSIPS-IC3|GFDL-SPEAR", ., value = TRUE)


#-------------------------------------------------------------------------------
# looping nos modelos para calculo da media ou mediana do ensemble

tic()
res_ens_memb <- map(models, 
    ensemble_model_refrcst, 
    path_files = path_qs_files,
    var_name = "prec",
    stat = "mean")
toc()

unlist(res_ens_memb)

#files_ens <- dir_ls(path_rds_files, regexp = "ensemble.*RDS")
#ens_check <- read_rds(res_ens_memb[1]])



## Uma forma de visualizar prec dos membros -------------------------------------
## para obter a media nas bacias para cada membro 
## ao inves de apenas a media dos membros
#
# here("output", "prec", "nmme_prec_CMC1-CanCM3_1981.nc") %>%
#   metR::ReadNetCDF(., "prec") %>%
#   group_by(S, L, Y, X) %>%
#   pivot_wider(names_from = "M", values_from = prec, names_prefix = "prec")
#   #summarise(prec = mean(prec)) %>%
#   arrange(desc(X))
# 
# 
## readRDS(here("output", "rds", "nmme_prec_CMC1-CanCM3_lt0.5.RDS"))
# ens_data %>% filter(L == 0.5, model == "CMC1-CanCM3")
