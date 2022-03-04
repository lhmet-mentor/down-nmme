
## Comparação da climatologia dos modelos NMME com obs do CRU
# 
# periodo dos dados processados

#remotes::install_github("dreamRs/esquisse")
pcks <- c("tidyverse", "here", "HEobs",
          "checkmate", "lubridate",
          "tictoc", "openair", "ggpubr", "ggExtra", "viridis", "see", "ggh4x")

easypackages::libraries(pcks)


#devtools::install_github("lhmet-ped/HEobs")

#------------------------------------------------------------------------------
# funcoes auxiliares
source("R/utils.R")
source("R/data-proc-rds.R")
source("R/clim-functions.R")
source("R/plot-clim-models-funs.R")

#------------------------------------------------------------------------------
# Metadata - codigos bacias e nomes

#data_link <- "https://www.dropbox.com/s/d40adhw66uwueet/VazoesNaturaisONS_D_87UHEsDirceuAssis_2018.dat?dl=1"
#qnat_meta <- extract_metadata(file = data_link, informative = TRUE)
#fs::dir_create(here("input/obs/qnat"))
#readr::write_rds(qnat_meta, file = here("input/obs/qnat", "qnat_meta_ons.RDS"))

qnat_meta <- readr::read_rds(here("input/obs/qnat", "qnat_meta_ons.RDS"))
#glimpse(qnat_meta)
arrange(qnat_meta, estacao_codigo)
# para escolher um posto ONS a partir do codONS
top6()

(models_summary <- import_bin_file("output/qs/model_counts.qs"))



# dados das previsoes climaticas nmme e CRU------------------------------
nmme_cru_basin_clim <- climatology_nmme_cru("prec", 
                                            "weighted",
                                            "qs",
                                            list(
                                              avg = mean,
                                              med = median,
                                              sd = sd,
                                              mad = mad
                                            ))




#-------------------------------------------------------------------------------
# Viz

plot_clim <- ggp_climatologia(monthly_data = nmme_cru_basin_clim,
                              var_name = "prec",
                              ibasin = 270,
                              var_stat = c("avg", "sd")
                              #var_stat = c("med", "mad")
                              )

plot_clim[[1]]
plot_clim[[2]]
plot_clim[[3]]




# ndias <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
# names(ndias) <- 1:12
# 
# 
# clima_by_lead_month_rate <- clima_by_lead_month_tot %>%
#   mutate(prec_model_dly_variav =  prec_model/ndias[as.character(month)],
#          prec_model_dly_fix = prec_model/30
#          ) %>%
#   ungroup()

# descobrindo os periodos das climatologias de cada modelo
# prec_nmme_cru_flat %>%
#   group_by(model) %>%
#   summarise(sdate = min(date), edate = max(date))
#periodos <- readr::read_rds(here("output/rds/model_counts.RDS"))




