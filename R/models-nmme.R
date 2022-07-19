## aperfeicoamentos futuros
library(data.table)
pcks <- c("data.table", "dplyr", "tidyverse", "stringr")
easypackages::libraries(pcks)

# tabela com nome das variáveis em cada modelo
# informações obtidas manualmente no site ...

names_vars_models <- function() {
  tibble::tribble(
  ~model,                   ~prec,  ~tmax,         ~tmin,
  "CanCM4i",               "prec",  "tmax",        "tmin",
  "CanSIPS-IC3",           "prec",  "tmax",        "tmin",
  "CanSIPS-IC3-GEM5-NEMO", "prec",   "tmax",        "tmin",
  "CanSIPSv2",             "prec",  "tmax",        "tmin",
  "CMC1-CanCM3",           "prec",  "tmax",        "tmin",
  "CMC2-CanCM4",           "prec",  "tmax",        "tmin",
  "COLA-RSMAS-CCSM4",      "prec",  NA_character_, NA_character_,
  "GEM-NEMO",              "prec",  "tmax",        "tmin",
  "GFDL-CM2p1-aer04",      "prec",  "t_ref_max",   "t_ref_min",
  "GFDL-CM2p5-FLOR-A06",   "prec",  "t_ref_max",   "t_ref_min",
  "GFDL-CM2p5-FLOR-B01",   "prec",  "t_ref_max",   "t_ref_min",
  "GFDL-SPEAR",            "prec",  "t_ref_max",   "t_ref_min",
  "NASA-GEOSS2S",          "prec",  "t2mmax",      "t2mmin",
  "NASA-GMAO-062012",      "prec",  "t2mmax",      "t2mmin",
  "NCAR-CESM1",            "prec",  "tsmax",       "tsmin",
  # por nao haver tmx e tmin no cfsv2 deixamos NA por enquanto
  "NCEP-CFSv2",            "prec",  NA_character_, NA_character_ 
#>>>>>>> 0c30e633e399178d284bb3ed8e56d2d90ee5c74b
)
}

# criando uma tabela com os modelos e seus respectivos períodos e tipos
# informaçẽos obtidas manualmente no site
type_period_models <- function() {
  type_period_models <- tibble::tribble(
    ~model,               ~type,       ~start, ~end,
    "CanCM4i",            "FORECAST",  2016,   2021,
    "CanCM4i",            "HINDCAST",  1981,   2018,
    "CanSIPS-IC3",        "FORECAST",  2021,   2022,
    "CanSIPS-IC3",        "HINDCAST",  1980,   2020,
    "CanSIPS-IC3-GEM5-NEMO","FORECAST",   2021,   2022, 
    "CanSIPSv2",          "FORECAST",  2016,   2021,
    "CanSIPSv2",          "HINDCAST",  1981,   2018,
    "CMC1-CanCM3",        "FORECAST",  2011,   2019,
    "CMC1-CanCM3",        "HINDCAST",  1981,   2010,
    "CMC2-CanCM4",        "FORECAST",  2011,   2019,
    "CMC2-CanCM4",        "HINDCAST",  1981,   2010,
    "COLA-RSMAS-CCSM4",   "MONTHLY",   1982,   2022,
    "GEM-NEMO",           "FORECAST",  2016,   2021,
    "GEM-NEMO",           "HINDCAST",  1981,   2018,
    "GFDL-CM2p1-aer04",   "MONTHLY",   1982,   2021,
    "GFDL-CM2p5-FLOR-A06","MONTHLY",   1980,   2021,
    "GFDL-CM2p5-FLOR-B01","MONTHLY",   1980,   2021,
    "GFDL-SPEAR",         "FORECAST",  2020,   2022,
    "GFDL-SPEAR",         "HINDCAST",  1991,   2020,
    "NASA-GMAO-062012",   "MONTHLY",   1981,   2018,
    "NASA-GEOSS2S",       "FORECAST",  2017,   2022,
    "NASA-GEOSS2S",       "HINDCAST",  1981,   2017,
    "NCAR-CESM1",         "FORECAST",  2016,   2017,
    "NCAR-CESM1",         "HINDCAST",  1980,   2010,
    "NCEP-CFSv2",         "FORECAST",  2011,   2022,
    "NCEP-CFSv2",         "HINDCAST",  1982,   2010
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
  dplyr::filter(vname_real != is.na(vname_real)) %>% 
  dplyr::select(model, year, type, vname_ref) %>%
  dplyr::arrange(model, year) %>%
  dplyr::relocate(year, model, vname_ref, type)









