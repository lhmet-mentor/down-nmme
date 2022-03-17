pcks <- c(
  "tidyverse", "here", "HEobs",
  "checkmate", "lubridate",
  "tictoc", "openair", #"ggpubr", "ggExtra", "viridis", "see", "ggh4x"
  "verification","SpecsVerification"
)

easypackages::libraries(pcks)


# devtools::install_github("lhmet-ped/HEobs")

#------------------------------------------------------------------------------
# funcoes auxiliares
source("R/utils.R")
source("R/data-proc-rds.R")

# dados das previsoes climaticas nmme e CRU------------------------------
avg_type <- "weighted" 
extension <- "qs"
imodel <- "CanSIPS-IC3"
ibasin <- 6


nmme_cru_basin_data <- import_bin_file(
  .filename_basin_data(avg_type, extension)
) %>%
  dplyr::rename("n_L" = L) %>%
  dplyr::filter(model %in% imodel) %>%
  tidyr::unnest("data") %>%
  dplyr::ungroup() %>%
  dplyr::filter(codONS == 6)


# previsoes para janeiro, iniciando em dezembro 
v_data <- nmme_cru_basin_data %>%
  dplyr::select(model, Sr:prec_obs) %>%
  dplyr::mutate(L = factor(trunc(L)), S = NULL) #%>%  dplyr::filter(L == 1, month(Sr) == 12) 


ens_data <- v_data %>%
  pivot_wider(names_from = "member", values_from = "prec_model", names_prefix = "prec_") %>%
  #dplyr::select(-prec_obs) %>%
  dplyr::group_by(model, codONS, L, Sr, date) %>%
  dplyr::summarise(
    dplyr::across(contains("prec"), mean),
    .groups = "drop"
  ) 

ens_data %>%
  dplyr::mutate()
  dplyr::filter(L ==1, month(Sr) == 12)




