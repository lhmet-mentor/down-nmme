
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




# aplica funcao para medias dos membros por modelo ----------------------------

#NAO TESTADA AINDA POR CAUSA DA DEMORA ~18 min para rodar
aggregate_members_nmme(
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
  
# importar arquivo resultante para analise posteriores




