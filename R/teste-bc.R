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
  "MBC",
  "DataExplorer" 
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
data_nmme_cru_file <- here("output/qs/basin-avgs/weighted/",
                           "nmme-cru-mly-weighted-avg-basins-ons-ens-members-models-ens-mean-prec-1982-2010.qs")
data_nmme_cru <- import_bin_file(data_nmme_cru_file)


# data_pp %>%
#   group_by(model) %>%
#   tally()


# Dados para teste de aplicacao BC---------------------------------------------
 imodel = "CanSIPS-IC3"
 ibasin = 6
 month = 1
 lead_time = 1

data_pp_1model <- data_nmme_cru %>%
  dplyr::filter(codONS == ibasin, 
                month(date) == month, 
                model == imodel, 
                L == lead_time
                ) 

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
unique(month(data_pp_1model$date))
unique(data_pp_1model$L)

data_pp <- data_pp_1model %>%
  filter(month(date) == 1)

dados_pp_split <- loo_cv(data_pp)


dados_pp_cv <- map_df(
  1:nrow(data_pp),
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
      ),
      vi = variance_inflation(
        obs = treino$obs_avg,
        prev = treino$model_avg,
        ens = treino$ens_avg,
        ens_sd = treino$ens_sd,
        prev_new = model_avg,
        ens_new = ens_avg
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
  select(-ens_sd, -model_sd, -contains("member"), -month) %>%
  pivot_longer(-(model:date), names_to = "prec", values_to = "value")

dados_pp_cv_long %>%
  filter(prec %in% c("obs_avg", "mos", "ens_avg", "vi")) %>%
  ggplot(aes(x = date, y = value, color = prec)) +
  #geom_col(position = "dodge") + 
  geom_point() +
  geom_line() +
  theme_bw()


dados_pp_cv4plot <-  dados_pp_cv %>%
  janitor::remove_empty(which = "cols") %>%
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















