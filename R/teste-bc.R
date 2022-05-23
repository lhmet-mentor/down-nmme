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

# importa dados combinados das medias dos modelos e da media ensemble
data_join_file <- here("output/qs/basin-avgs/weighted/nmme-mly-models-avgs-ens-mean-1982-2010-prec.qs")

# importa dados combinados (ens e medias modelos)
data_pp <- import_bin_file(data_pp_file)


# Dados para teste de aplicacao BC---------------------------------------------
 imodel <- "NCEP-CFSv2"
 ibasin <- 156
 month <- 1

data_pp_1model <- data_pp %>%
  #dplyr::filter(codONS %in% top6()[[1]], L == 1, month(date) == month) %>%
  dplyr::filter(codONS == ibasin, month(date) == month, model == imodel, L == 1) 
  

rng <- range(select(data_pp_1model, model_avg, obs_avg, ens_avg))
rng <- c(trunc(rng[1]), ceiling(rng[2]))

data_pp_1model_long <- data_pp_1model %>%
  select(-ens_sd) %>%
  pivot_longer(cols = -c(model:date), names_to = "prec", values_to = "value")
  
data_pp_1model_long %>%
  ggplot(aes(x = date, y = value, color = prec)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  geom_hline(yintercept = mean(data_pp_1model$obs_avg))

openair::scatterPlot(data_pp_1model, 
                     x = "obs_avg", 
                     y = "ens_avg", 
                     linear = TRUE, 
                     mod.line = TRUE,
                     #type = c("model", "codONS")
                     pch = 20, 
                     xlim = c(rng), 
                     ylim = rng
                     
)
openair::scatterPlot(data_pp_1model, 
                     x = "obs_avg", 
                     y = "model_avg", 
                     linear = TRUE, 
                     mod.line = TRUE,
                     #type = c("model", "codONS")
                     pch = 20, 
                     xlim = c(rng), 
                     ylim = rng
                     
)

data_pp_1model_long %>%
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

dados_pp_split <- loo_cv(data_pp_1model)


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
      # Mean and Variance Adjustment. Leung et al. 1999. BiasCorrection {CSTools}
      sbc = sbc(
        var_obs = treino[["obs_avg"]],
        var_exp = treino[["model_avg"]],
        var_cor = model_avg,
        na.rm = TRUE
      ),
      # Scaling. biasCorrection1D {downscaleR}
      bc_scaling = downscaleR:::scaling(
        o = treino$obs_avg,
        p = treino$model_avg,
        s = model_avg,
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








