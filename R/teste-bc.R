pcks <- c(
  "tidyverse", "here", "HEobs",
  "checkmate", "lubridate",
  "tictoc", "openair", "ggpubr", "ggExtra", "viridis", "see", "ggh4x",
  "ensemblepp",
  "ensembleBMA",
  "ensembleMOS",
  "downscaleR",
  "tidymodels",
  "crch",
  "MBC" # 
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
data_join_file <- here("output/qs/basin-avgs/weighted",
                       "nmme-cru-mly-weighted-avg-basins-ons-ens-members-ens-mean-prec-1982-2010.qs")

# importa dados combinados (ens e medias modelos)
data_pp <- import_bin_file(data_join_file) 
unique(data_pp$model)

# data_pp %>%
#   group_by(model) %>%
#   tally()

# verificacao das distribuicoes de prec 
# (aproximacao gaussiana para chuva mensal)
#bc0p2 <- function(x) (x + (0.01 * mean(x, na.rm = TRUE))^0.2)/0.2

data_pp %>%
  filter(codONS %in% top6()$codONS, month(date) == 8) %>%
  #mutate()
  #ggplot(aes(x = bc0p2(model_avg))) +
  ggplot(aes(x = model_avg)) +
  #ggplot(aes(x = obs_avg)) +
  geom_histogram(bins = 20) +
  facet_grid(vars(codONS), vars(model))


  

# dados com os modelos nas colunas para usar com metodos multimodelos
# data_pp_wide <-  data_pp %>%
#   select(-())
#   mutate(model = str_replace_all(model, "-", "_")) %>%
#   select(-Sr) %>%
#   pivot_wider(
#     names_from = "model", 
#     values_from = "model_avg"
#     #names_prefix = ""
#   ) %>%
#   tail()


  





# Dados para teste de aplicacao BC---------------------------------------------
 imodel <- "CanSIPS-IC3"
 ibasin <- 6
 month <- 1

data_pp_1model <- data_pp %>%
  #select(-model_sd) %>%
  #dplyr::filter(codONS %in% top6()[[1]], L == 1, month(date) == month) %>%
  dplyr::filter(codONS == ibasin, month(date) == month, model == imodel, L == 1) 
  

d <- data_pp %>%
  #select(-model_sd) %>%
  #dplyr::filter(codONS %in% top6()[[1]], L == 1, month(date) == month) %>%
  dplyr::filter(codONS == ibasin, month(date) == month, L == 1)
d %>%
  ggplot(aes(x = date, y = model_avg, color = model)) + 
  geom_line() +
  geom_line(data = distinct(d, date, obs_avg),
            aes(x = date, y = obs_avg), color = 1)


rng <- range(select(data_pp_1model, model_avg, obs_avg, ens_avg))
rng <- c(trunc(rng[1]), ceiling(rng[2]))

data_pp_1model_long <- data_pp_1model %>%
  select(-c(ens_sd, model_sd)) %>%
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
    # ires = 9
    # primeira reamostra
    i_resample <- dados_pp_split[["splits"]][[ires]]
    # dados de trinamento da reamostra
    treino <- training(i_resample)
    # dados de teste da reamostra
    teste <- testing(i_resample)

    teste <- 
      mutate(teste,
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
      ),
      mos = MOS(obs = treino$obs_avg,
                prev = treino$model_avg,
                prev_new = model_avg
                ),
      ngr2 = NGR2(
        obs = treino$obs_avg,
        ensmean = treino$ens_avg,
        enssd = treino$ens_sd,
        ensmean_new = ens_avg,
        enssd_new = ens_sd
      )
      # inserir outros metodos aqui
      # ...
      # MBC
    )
   teste
  }
)

dados_pp_cv <- arrange(dados_pp_cv, model, codONS, date)



dados_pp_cv_long <- dados_pp_cv %>%
  select(-ens_sd) %>%
  pivot_longer(-(model:date), names_to = "prec", values_to = "value")

dados_pp_cv_long %>%
  filter(prec %in% c("obs_avg", "mos", "ens_avg", "ngr2")) %>%
  ggplot(aes(x = date, y = value, color = prec)) +
  #geom_col(position = "dodge") + 
  geom_point() +
  geom_line() +
  theme_bw()


dados_pp_cv4plot <-  dados_pp_cv %>%
  select(-ens_sd) %>%
  pivot_longer(cols = -c(model:date, obs_avg),
               names_to = "models",
               values_to = "value")

rng <- range(select(dados_pp_cv4plot, obs_avg, value))
rng <- c(trunc(rng[1]), ceiling(rng[2]))

dados_pp_cv4plot %>%
openair::scatterPlot(., 
                     x = "obs_avg", 
                     y = "value", 
                     type = "models",
                     linear = TRUE, 
                     mod.line = TRUE,
                     #type = c("model", "codONS")
                     pch = 20, 
                     xlim = c(rng), ylim = rng
)

# metricas de verificacao















