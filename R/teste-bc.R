pcks <- c(
  "tidyverse", "here", "HEobs",
  "checkmate", "lubridate",
  "tictoc", "openair", "ggpubr", "ggExtra", "viridis", "see", "ggh4x"
)

easypackages::libraries(pcks)


# devtools::install_github("lhmet-ped/HEobs")

#------------------------------------------------------------------------------
# funcoes auziliares
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

nmme_cru_basin_data <- import_bin_file(
  .filename_basin_data(avg_type, extension)
) %>%
  dplyr::rename("n_L" = L)

# media dos membros para o primeiro modelo
# m1 <- nmme_cru_basin_data[["data"]][[1]] %>%
#   select(-S) %>%
#   dplyr::mutate(L = factor(trunc(L))) %>%
#   #group_by(codONS, L, month = month(date)) %>%
#   group_by(codONS, L, date) %>%
#   summarise(across(contains("prec"), 
#                    list(avg = mean, 
#                         med = median,
#                         sd = sd,
#                         mad = mad
#                    )),
#             .groups = "drop"
#   ) #%>% View()
# 
# gc()


funs_l <- list(
  avg = mean,
  med = median,
  sd = sd,
  mad = mad
)

# media dos membros dos modelos
# 1 previsao por media do ensemble dos membros
nmme_cru_basin_data_ens <- 
  nmme_cru_basin_data %>%
  ungroup() %>%
  dplyr::mutate(.,
              data = map(
                data,
                ~ .x %>%
                  dplyr::select(-S) %>%
                  dplyr::mutate(L = factor(trunc(L), levels = 0:11, ordered = TRUE)) %>%
                  dplyr::group_by(codONS, L, date) %>%
                  dplyr::summarise(
                    dplyr::across(
                      dplyr::contains(var_name),
                      funs_l
                    ),
                    .groups = "drop"
                  )
              )
            )


# Dados para teste de aplicacao BC---------------------------------------------
imodel <- "CanSIPS-IC3"
ibasin <- 6
month <- 1

nmme_cru_basin_data_ens <- nmme_cru_basin_data_ens %>%
  dplyr::select(model, data) %>%
  unnest()


dados_pp <- nmme_cru_basin_data_ens %>%
  filter(codONS == ibasin, model == imodel, L == 1, month(date) == month) %>%
  dplyr::select(date, contains("avg"))


# dados_pp <- nmme_cru_basin_data_ens %>%
#   filter(codONS == 6, model == imodel, L == 1, month == 1) %>%
#   select(contains("avg"))

dados_pp