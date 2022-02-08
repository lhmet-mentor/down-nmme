

pcks <- c("raster", "terra", "tidyverse", "here", 
          "checkmate", "metR", "fs", "lubridate",
          "tictoc")
easypackages::libraries(pcks)


source(here("R", "data-proc-rds.R"))

##------------------------------------------------------------------------------


path_rds_files <- here("output", "rds")
model_counts <- readr::read_rds(here(path_rds_files, "model_counts.RDS"))
models <- model_counts$modelo
#files_rds <- dir_ls(path_rds, pattern = "CanCM4i")


# looping nos modelos para calculo da media ou mediana do ensemble
tic()
map(models, 
    ensemble_model_refrcst, 
    path_rds = path_rds_files,
    var_name = "prec",
    stat = "mean")
toc()




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
