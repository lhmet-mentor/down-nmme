
pcks <- c("tidyverse", "here", "HEobs",
          "checkmate", "lubridate",
          "tictoc", "openair", "ggpubr", "ggExtra", "viridis", "see")
easypackages::libraries(pcks)


#devtools::install_github("lhmet-ped/HEobs")

#------------------------------------------------------------------------------
# funcoes auxiliares
source("R/utils.R")

#------------------------------------------------------------------------------
# Metadata - codigos bacias e nomes

#data_link <- "https://www.dropbox.com/s/d40adhw66uwueet/VazoesNaturaisONS_D_87UHEsDirceuAssis_2018.dat?dl=1"
#qnat_meta <- extract_metadata(file = data_link, informative = TRUE)
#fs::dir_create(here("input/obs/qnat"))
#readr::write_rds(qnat_meta, file = here("input/obs/qnat", "qnat_meta_ons.RDS"))

qnat_meta <- readr::read_rds(here("input/obs/qnat", "qnat_meta_ons.RDS"))
glimpse(qnat_meta)
arrange(qnat_meta, estacao_codigo)


#------------------------------------------------------------------------------
# previsoes climaticas nmme e observacoes do CRU
#avg_type <- "arithmetic"
avg_type <- "weighted" # melhores resultados

out_dir <- here(
  str_replace("output/rds/basin-avgs/avg_type", "avg_type", avg_type)
  )

prec_nmme_cru <- readr::read_rds(
  file = here(out_dir,
              str_replace("nmme-cru-mly-avg_type-avg-basins-ons.RDS",
                          "avg_type",
                          avg_type
                          )
              )
  ) 

#prec_nmme_cru[["data"]][[1]]

prec_nmme_cru_lfix <- prec_nmme_cru %>%
  mutate(data = map(data, 
                    ~ .x %>% mutate(S = NULL,
                                    # para facilitar interpretacao do lead time
                                    L = as.integer(trunc(L))
                                    )
                    )
  )

# para deixar os dados em tibble comum (sem dados aninhados)
prec_nmme_cru_flat <-  prec_nmme_cru_lfix %>%
  unnest() %>%
  ungroup()

saveRDS(prec_nmme_cru_flat, file = here(out_dir, "prec_nmme_cru_tidy.RDS"))
rm(prec_nmme_cru_lfix, prec_nmme_cru)

#-------------------------------------------------------------------------------
# Climatologia do NMME e do CRU para lead 0
models <- prec_nmme_cru_flat$model %>% unique()

clima_by_lead_month_tot <- 
 filter(prec_nmme_cru_flat, 
       #model == models[1]
       #L == 0
       ) %>%
  group_by(codONS, month = lubridate::month(date), L, model) %>%
  summarise_at(.vars = vars(contains("prec")), .funs = list(mean)) %>%
  arrange(model, codONS, L, month)

clima_by_lead_month_tot



ndias <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
names(ndias) <- 1:12


clima_by_lead_month_rate <- clima_by_lead_month_tot %>%
  mutate(prec_model_dly_variav =  prec_model/ndias[as.character(month)],
         prec_model_dly_fix = prec_model/30
         ) %>%
  ungroup()

# descobrindo os periodos das climatologias de cada modelo
# prec_nmme_cru_flat %>%
#   group_by(model) %>%
#   summarise(sdate = min(date), edate = max(date))
periodos <- readr::read_rds(here("output/rds/model_counts.RDS"))

# TO DO
# Ler netcdfs das clomatologias e formata-los em um tibble
#metR::ReadNetCDF
# juntar com os dados clima_by_lead_month_rate
# e avaliar qual prec_model eh mais proxima
# tomando em conta o periodo


#--------------------------------------------------------------------------
# 





