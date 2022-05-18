pcks <- c(
  "tidyverse", "here", "HEobs",
  "checkmate", "lubridate",
  "tictoc", "openair", "ggpubr", "ggExtra", "viridis", "see", "ggh4x"
)

easypackages::libraries(pcks)

## Refs
# https://github.com/SantanderMetGroup/loadeR/wiki



# devtools::install_github("lhmet-ped/HEobs")

#------------------------------------------------------------------------------
# funcoes auxiliares
source("R/utils.R")
source("R/data-proc-rds.R")



# Metadata - codigos bacias e nomes -----------------------------------------
# data_link <- "https://www.dropbox.com/s/d40adhw66uwueet/VazoesNaturaisONS_D_87UHEsDirceuAssis_2018.dat?dl=1"
# qnat_meta <- extract_metadata(file = data_link, informative = TRUE)
# fs::dir_create(here("input/obs/qnat"))
# readr::write_rds(qnat_meta, file = here("input/obs/qnat", "qnat_meta_ons.RDS"))
##
qnat_meta <- readr::read_rds(here("input/obs/qnat", "qnat_meta_ons.RDS"))
glimpse(qnat_meta)
arrange(qnat_meta, estacao_codigo)


(models_summary <- import_bin_file("output/qs/model_counts.qs"))

# para escolher um posto ONS a partir do codONS
top6()



# dados das previsoes climaticas nmme e CRU------------------------------
avg_type <- "weighted" # melhores resultados
extension <- "qs"
var_name <- "prec"

nmme_cru_basin_data_models <- import_bin_file(
  .filename_basin_data(avg_type, extension, "-ens-smry")
) %>%
  dplyr::rename("n_L" = L)


# Dados para teste de aplicacao BC---------------------------------------------
nmme_cru_basin_data_models_flat <- nmme_cru_basin_data_models %>%
  dplyr::select(model, data) %>%
  unnest("data")

imodel <- "CanSIPS-IC3"
ibasin <- 6
month <- 1

dados_pp <- nmme_cru_basin_data_models_flat %>%
  dplyr::filter(codONS == ibasin, model == imodel, L == 1, month(date) == month) %>%
  dplyr::select(date, contains("avg"))


tail(dados_pp)


