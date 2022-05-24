
# Agregacao das series dos membros media e desvio padrao
# para agregar as previsoes pela media do ensemble de cada modelo


pcks <- c(
  "tidyverse", "here", "HEobs",
  "checkmate", "lubridate",
  "tictoc", "data.table"
  #"openair", "ggpubr", "ggExtra", "viridis", "see", "ggh4x"
)

easypackages::libraries(pcks)


# devtools::install_github("lhmet-ped/HEobs")

#------------------------------------------------------------------------------
# funcoes auxiliares
source(here("R/utils.R"))
source(here("R/data-proc-rds.R"))
source(here("R/aggregate-nmme.R"))




# medias dos membros por modelo ----------------------------

#NAO TESTADA AINDA POR CAUSA DA DEMORA ~18 min para rodar
tictoc::tic()
nmme_data_file_name <- aggregate_members_nmme(
  avg_type = "weighted", # melhores resultados
  extension = "qs",
  var_name = "prec", 
  suffix = "ens-smry",
  funs_list = list(
    avg = mean,
    med = median,
    sd = sd,
    mad = mad
  )
)
nmme_data_file_name
tictoc::toc()
# 
# Usa o arquivo gerado na chamada acima
# para calcular a media do conjunto de modelos

tictoc::tic()
nmme_ens_file_name <- aggregate_models(
  var_target = "members_avg",
  avg_type = "weighted", # melhores resultados
  extension = "qs",
  var_name = "prec",
  funs_list = list(
    avg = mean,
    med = median,
    sd = sd,
    mad = mad
  ),
  prefix = "nmme-mly-ens-mean-1982-2010"
)
tictoc::toc()
#38.782 sec elapsed
nmme_ens_file_name



# juncao das medias do modelo com a media ensemble
(data_pp_file <- join_nmme_model_ensemble(nmme_ens_file_name, nmme_data_file_name))


