## aperfeicoamentos futuros
library(data.table)
pcks <- c("data.table", "dplyr", "tidyverse", "stringr")
easypackages::libraries(pcks)

# tabela com nome das variáveis em cada modelo
# informações obtidas manualmente no site ...

names_vars_models <- function() {

  # separando grupo de models por nome das teperaturas
  variables_1 <- c("tmax", "tmin", "prec")
  variables_2 <- c("t2mmax", "t2mmin", "prec")
  variables_3 <- c("t_ref_max", "t_ref_min", "prec")
  variables_4 <- c("tsmx", "tsmn", "prec")
  variables_5 <- c("prec")
  models_1 <- c("CanCM4i", "CanSIPSv2", "CMC1-CanCM3", "CMC2-CanCM4",
                "GEM-NEMO", "CanSIPS-IC3")
  models_2 <- c("NASA-GEOSS2S", "NASA-GMAO-062012")
  # os modelos 2 possuem os dados de temperatura em Celcius
  models_3 <- c("GFDL-SPEAR", "GFDL-CM2p1-aer04", "GFDL-CM2p5-FLOR-A06", 
                "GFDL-CM2p5-FLOR-B01")
  models_4 <- c("NCAR-CESM1")
  models_5 <- c("NCEP-CFSv2")

  names_vars_models <- expand.grid(models_1, variables_1) %>%
    rbind(expand.grid(models_2, variables_2)) %>%
    rbind(expand.grid(models_3, variables_3)) %>%
    rbind(expand.grid(models_4, variables_4)) %>%
    rbind(expand.grid(models_5, variables_5)) %>%
    setNames(c("model", "variable")) %>%
    tibble::as_tibble() %>%
    dplyr::mutate_all(.funs = as.character) %>%
    dplyr::arrange(model, variable)

  names_vars_models <- names_vars_models %>%
    dplyr::mutate(id = ifelse(stringr::str_detect(variable, "x"),
      "tmax",
      ifelse(stringr::str_detect(variable, "n"), "tmin", "prec")
    )) %>%
    tidyr::pivot_wider(names_from = "id", values_from = variable)
  names_vars_models
}

# criando uma tabela com os modelos e seus respectivos períodos e tipos
# informaçẽos obtidas manualmente no site
type_period_models <- function() {
  type_period_models <- tibble::tribble(
    ~model, ~type, ~start, ~end,
    "CanCM4i", "FORECAST", 2016, 2021,
    "CanCM4i", "HINDCAST", 1981, 2018,
    #"Cansips", "FORECAST", 2015, 2019, # foi substituido pelo CanSIPSv2
    #                                     previsoes so ate 2019
    "CanSIPS-IC3", "FORECAST", 2021, 2022,
    "CanSIPS-IC3", "HINDCAST", 1980, 2020,
    "CanSIPSv2", "FORECAST", 2016, 2021,
    "CanSIPSv2", "HINDCAST", 1981, 2018,
    "CMC1-CanCM3", "FORECAST", 2011, 2019,
    "CMC1-CanCM3", "HINDCAST", 1981, 2010,
    "CMC2-CanCM4", "FORECAST", 2011, 2019,
    "CMC2-CanCM4", "HINDCAST", 1981, 2010,
    "GEM-NEMO", "FORECAST", 2016, 2021,
    "GEM-NEMO", "HINDCAST", 1981, 2018,
    "GFDL-SPEAR", "FORECAST", 2020, 2022,
    "GFDL-SPEAR", "HINDCAST", 1991, 2020,
    "NASA-GEOSS2S", "FORECAST", 2017, 2022,
    "NASA-GEOSS2S", "HINDCAST", 1981, 2017,
    "NCAR-CESM1", "FORECAST", 2016, 2017,
    "NCAR-CESM1", "HINDCAST", 1980, 2010,
    "NCEP-CFSv2", "FORECAST", 2011, 2022,
    "NCEP-CFSv2", "HINDCAST", 1982, 2010,
    "GFDL-CM2p1-aer04", "MONTHLY", 1982, 2021,
    "GFDL-CM2p5-FLOR-A06", "MONTHLY", 1980, 2021,
    "GFDL-CM2p5-FLOR-B01", "MONTHLY", 1980, 2021,
    "NASA-GMAO-062012", "MONTHLY", 1981, 2018
  )

  type_period_models <- tibble::as_tibble(type_period_models) %>%
    dplyr::mutate(across(.funs = as.character))
  type_period_models
}
# criando uma tabela com o modelo e seus respectivos tipos de previsao e anos
tab_mod_year_type <- function(models_period = type_period_models()) {

  tab_mod_year_type <- expand_grid(
    model = models_period$model[1],
    type = models_period$type[1],
    year = seq(from = models_period$start[1], 
               to = models_period$end[1])
  )
  for (i in 2:length(models_period$model)) {
    tab_mod_year_type <- tab_mod_year_type %>%
      rbind(
        expand_grid(
          model = models_period$model[i],
          type = models_period$type[i],
          year = seq(from = models_period$start[i], 
                     to = models_period$end[i]
                     )
        )
      )
  }
  tab_mod_year_type
}

# criando uma tabela com as informacoes necessarias para realizacao do download
tab_mod_year_vname_type <- dplyr::full_join(
  tab_mod_year_type(),
  names_vars_models(),
  by = "model"
) %>%
  tidyr::pivot_longer(
    cols = -(model:year),
    names_to = "vname_ref", # nomes padronizados de tmax e tmin p/ chamada de download
    values_to = "vname_real" # nomes de tmax e tmin usados em cada modelo
  ) %>%
  dplyr::select(model, year, type, vname_ref) %>%
  dplyr::arrange(model, year) %>%
  dplyr::relocate(year, model, vname_ref, type)
