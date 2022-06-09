
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
source(here("R/clim-functions.R"))
source(here("R/aggregate-nmme.R"))





# medias dos membros -----------------------------------------------------------

#NAO TESTADA AINDA POR CAUSA DA DEMORA ~18 min para rodar
tictoc::tic()
nmme_data_file_name <- aggregate_members_nmme(
  avg_type = "weighted", # melhores resultados
  extension = "qs",
  var_name = "prec", 
  suffix = "ens-members",
  funs_list = list(
    avg = mean,
    med = median,
    sd = sd,
    mad = mad
  )
)
# nmme_data_file_name
# nmme_data_file_name = '/home/hidrometeorologista/Dropbox/github/my_reps/lhmet/download-hindcast-NMME/output/qs/basin-avgs/weighted/nmme-cru-mly-weighted-avg-basins-ons-ens-members-prec.qs'
tictoc::toc()
 


# media do conjunto -----------------------------------------------------------

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
  suffix = "ens-models"
)
tictoc::toc()
# 45 sec elapsed
nmme_ens_file_name
# nmme_ens_file_name = '/home/hidrometeorologista/Dropbox/github/my_reps/lhmet/download-hindcast-NMME/output/qs/basin-avgs/weighted/nmme-cru-mly-weighted-avg-basins-ons-ens-models-prec-1982-2010.qs'


# previsoes dos membros --------------------------------------------------------
tictoc::tic()
nmme_members_wide_file <- spread_members_nmme(
  avg_type = "weighted", # melhores resultados
  extension = "qs",
  var_name = "prec",  
  out_file_suffix = "wide-flat"
)
tictoc::toc()
# 977.99 sec elapsed
#! 16.3 min
# nmme_members_wide_file = here('output/qs/basin-avgs/weighted/nmme-cru-mly-weighted-avg-basins-ons-wide-flat.qs')

# juncao das medias dos membros com a media ensemble----------------------------
ens_models_join_file <- join_nmme_model_ensemble(
  nmme_ens_file = nmme_ens_file_name,
  nmme_data_file = nmme_data_file_name
)
ens_models_join_file
# ens_models_join_file = here('output/qs/basin-avgs/weighted/nmme-cru-mly-weighted-avg-basins-ons-ens-members-ens-mean-prec-1982-2010.qs')


# juncao dos membros, das medias dos membros e da média ensemble ---------------
ens_members_models_join_file <- join_nmme_models_members_ensemble(
  nmme_models_file = ens_models_join_file,
  nmme_members_file = nmme_members_wide_file,
  var_name = "prec", 
  out_file_suffix = "members-models-ens-mean",
  climatology = TRUE
)
ens_members_models_join_file
# ens_members_models_join_file = "/home/hidrometeorologista/Dropbox/github/my_reps/lhmet/download-hindcast-NMME/output/qs/basin-avgs/weighted/nmme-cru-mly-weighted-avg-basins-ons-ens-members-models-ens-mean-prec-1982-2010.qs"





