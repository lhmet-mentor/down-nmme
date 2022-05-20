pcks <- c(
  "tidyverse", "here", "HEobs",
  "checkmate", "lubridate",
  "tictoc", "openair", "ggpubr", "ggExtra", "viridis", "see", "ggh4x",
  "ensemblepp",
  "downscaleR"
)

easypackages::libraries(pcks)

## Refs
# https://github.com/SantanderMetGroup/loadeR/wiki

# devtools::install_github("lhmet-ped/HEobs")

#------------------------------------------------------------------------------
# funcoes auxiliares
source("R/utils.R")
source("R/data-proc-rds.R")
source("R/bc-funs.R")

# para escolher um posto ONS a partir do codONS
top6()



# parametros para selcao dos dados das previsoes climaticas nmme e CRU----------
avg_type <- "weighted" # melhores resultados
extension <- "qs"
var_name <- "prec"

# previsoes medias do ensemble (1 prev por modelo)
nmme_cru_basin_data_models <- import_bin_file(
  .filename_basin_data(avg_type, extension, "-ens-smry")
) %>%
  dplyr::select(model, data) %>%
  unnest("data")


# Dados para teste de aplicacao BC---------------------------------------------
imodel <- "GFDL-SPEAR"
ibasin <- 156
month <- 1

dados_pp <- nmme_cru_basin_data_models %>%
  #dplyr::filter(codONS %in% top6()[[1]], L == 1, month(date) == month) %>%
  dplyr::filter(month(date) == month) %>%
  dplyr::filter(codONS == ibasin, model == imodel, L == 1, month(date) == month) %>%
  dplyr::select(model, codONS, date,
                # media do ensemble
                contains("avg"), 
                # desvio padrao do ensemble (requerido por alguns metodos de PP)
                contains("model_sd")
                ) %>%
  arrange(date) %>%
  # previsao por ensemble sem correcao (bruta)
  rename("ens_avg" = prec_model_avg,  
         "obs" = prec_obs_avg,
         "ens_sd" = prec_model_sd
         )

rng <- range(select(dados_pp, obs, ens_avg))
rng <- c(trunc(rng[1]), ceiling(rng[2]))

dados_pp_long <- dados_pp %>%
  select(-ens_sd) %>%
  pivot_longer(cols = -c(model:date), names_to = "prec", values_to = "value")
  
dados_pp_long %>%
  ggplot(aes(x = date, y = value, color = prec)) +
  geom_point() + 
  geom_line() +
  theme_bw()

openair::scatterPlot(dados_pp, 
                     x = "obs", 
                     y = "ens_avg", 
                     linear = TRUE, 
                     mod.line = TRUE,
                     #type = c("model", "codONS")
                     pch = 20, 
                     xlim = c(rng), ylim = rng
)


dados_pp_long %>%
  ggplot(aes(x = value, color = prec)) +
  geom_density()




#-------------------------------------------------------------------------------
# separacao dos dados em periodos de treino (calibracao) e teste (validacao)
# IDEAL: "treino" seria o periodo das hindcasts (1982-2010)
#        "teste"  seria o periodo das forecasts (previsões) de 2011-2020 

library(tidymodels)

## separacao dos dados aleatoria
# dados_pp_split <- initial_split(dados_pp, prop = 0.6) 
## prop: proporcao das obs que serao usadas para treino
## 1 - prop: proporcao das obs usada para teste 


## como nao temos os dados baixados do periodo de previsao usaremos outra forma
## alternativa: validacao cruzada

dados_pp_split <- loo_cv(dados_pp)


dados_pp_cv <- map_df(
  1:nrow(dados_pp_split),
  function(ires) {
    # ires = 1
    # primeira reamostra
    i_resample <- dados_pp_split[["splits"]][[ires]]
    # dados de trinamento da reamostra
    treino <- training(i_resample)
    # dados de teste da reamostra
    teste <- testing(i_resample)

    teste <- teste %>%
      mutate(
      sbc = sbc(
        var_obs = treino$obs,
        var_exp = treino$ens_avg,
        var_cor = ens_avg,
        na.rm = TRUE
      ),
      bc_scaling = downscaleR:::scaling(
        o = treino$obs,
        p = treino$ens_avg,
        s = ens_avg,
        # method = "scaling",
        scaling.type = "multiplicative"
      )
      # inserir outros metodos aqui
      # ...
    )
   teste
  }
)

dados_pp_cv <- arrange(dados_pp_cv, model, codONS, date)



dados_pp_cv_long <- dados_pp_cv %>%
  select(-ens_sd) %>%
  pivot_longer(-(model:date), names_to = "prec", values_to = "value")

dados_pp_cv_long %>%
  ggplot(aes(x = date, y = value, color = prec)) +
  #geom_col(position = "dodge") + 
  geom_point() +
  geom_line() +
  theme_bw()


dados_pp_cv4plot <-  dados_pp_cv %>%
  select(-ens_sd) %>%
  pivot_longer(cols = -c(model:date, obs),
               names_to = "models",
               values_to = "value")

rng <- range(select(dados_pp_cv4plot, obs, value))
rng <- c(trunc(rng[1]), ceiling(rng[2]))

dados_pp_cv4plot %>%
openair::scatterPlot(., 
                     x = "obs", 
                     y = "value", 
                     type = "models",
                     linear = TRUE, 
                     mod.line = TRUE,
                     #type = c("model", "codONS")
                     pch = 20, 
                     xlim = c(rng), ylim = rng
)

# metricas de verificacao







# CSTools
# BiasCorrection()








