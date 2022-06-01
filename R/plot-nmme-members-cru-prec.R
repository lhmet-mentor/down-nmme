pcks <- c(
  "tidyverse", "here", "HEobs",
  "checkmate", "lubridate",
  "tictoc", "openair", "ggpubr", "ggExtra", "viridis", "see", "ggh4x",
  "Polychrome"
)

easypackages::libraries(pcks)

source(here("R/plot-nmme-members-cru-prec-funs.R"))

#------------------------------------------------------------------------------
# previsoes membros, modelos e media ensemble
data_nmme_cru_file <- here("output/qs/basin-avgs/weighted/nmme-cru-mly-weighted-avg-basins-ons-ens-members-models-ens-mean-prec-1982-2010.qs")
data_nmme_cru <- import_bin_file(data_nmme_cru_file)

ggp <- plot_members_nmme(data_nmme_cru,
                         imodel = "CanSIPS-IC3",
                         cod_ons = 270,
                         months = c(1, 4, 8),
                         leads = 0:2,
                         highlights = c("climatology", "obs_avg", "ens_avg", "model_avg"),
                         alpha_memb = 0.4
) 

ggp


