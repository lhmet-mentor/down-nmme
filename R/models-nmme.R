## aperfeicoamentos futuros
library(data.table)
pcks <- c("data.table", "dplyr", "tidyverse", "stringr")
easypackages::libraries(pcks)

# tabela com nome das variáveis em cada modelo
# informações obtidas manualmente no site ...

names_vars_models <- function(){
  
  variables_1 <- c( "tmax", "tmin", "prec")
  variables_2 <-c("t2mmax","t2mmin", "prec")
  variables_3 <- c("t_ref_max", "t_ref_min", "prec")
  variables_4 <- c("tsmx", "tsmn", "prec")
  models_1 <- c("CanCM4i","CanSIPSv2", "CMC1-CanCM3","CMC2-CanCM4", "GEM-NEMO", "CanSIPS-IC3")
  models_2 <- c("NASA-GEOSS2S","NASA-GMAO-062012")
  # os modelos 2 possuem os dados de temperatura em Celcius
  models_3 <- c("GFDL-SPEAR","GFDL-CM2p1-aer04", "GFDL-CM2p5-FLOR-A06", "GFDL-CM2p5-FLOR-B01")
  models_4 <- c("NCAR-CESM1")
  
  names_vars_models<- expand.grid(models_1, variables_1) %>% 
    rbind(expand.grid(models_2,variables_2))%>% 
    rbind(expand.grid(models_3, variables_3)) %>% 
    rbind(expand.grid(models_4, variables_4)) %>% 
    setNames(c("model", "variable")) %>%
    as_tibble() %>%   
    mutate_all(.funs = as.character) %>%  
    arrange(model, variable)
  
    names_vars_models <-   names_vars_models %>%
    mutate(id = ifelse(str_detect(variable, "x"), 
                       "tmax",
                       ifelse(str_detect(variable, "n"), "tmin", "prec"))
    ) %>% 
    pivot_wider(names_from = 'id', values_from = variable)
    names_vars_models
}

#criando uma tabela com os modelos e seus respectivos períodos e tipos
#informaçẽos obtidas manualmente no site
type_period_models <- function(){
  type_period_models <- tribble(
    ~model,           ~type,              ~start,    ~end,
    "CanCM4i",        "FORECAST",            2016,     2021,
    "CanCM4i",        "HINDCAST",            1981,     2018,
    "Cansips",        "FORECAST",            2015,     2019,
    "CanSIPS-IC3",    "FORECAST",            2021,     2022,
    "CanSIPS-IC3",    "HINDCAST",            1980,     2020,
    "CanSIPSv2",      "FORECAST",            2016,     2021,
    "CanSIPSv2",      "HINDCAST",            1981,     2018,
    "CMC1-CanCM3",    "FORECAST",            2011,     2019,
    "CMC1-CanCM3",    "HINDCAST",            1981,     2010,
    "CMC2-CanCM4",    "FORECAST",            2011,     2019,
    "CMC2-CanCM4",    "HINDCAST",            1981,     2010,
    "GEM-NEMO",       "FORECAST",            2016,     2021,   
    "GEM-NEMO",       "HINDCAST",            1981,     2018,
    "GFDL-SPEAR",     "FORECAST",            2020,     2022,
    "GFDL-SPEAR",     "HINDCAST",            1991,     2020,
    "NASA-GEOSS2S",   "FORECAST",            2017,     2022, 
    "NASA-GEOSS2S",   "HINDCAST",            1981,     2017,
    "NCAR-CESM1",     "FORECAST",            2016,     2017,
    "NCAR-CESM1",     "HINDCAST",            1980,     2010,
    "GFDL-CM2p1-aer04", "MONTHLY",           1982,     2021,
    "GFDL-CM2p5-FLOR-A06", "MONTHLY",        1980,     2021,
    "GFDL-CM2p5-FLOR-B01", "MONTHLY",        1980,     2021,
    "NASA-GMAO-062012", "MONTHLY",           1981,     2018
    
  )
  
  type_period_models <- as_tibble(type_period_models) %>% 
    mutate_all(.funs = as.character)
  type_period_models
}
# criando uma tabela com o modelo e seus respectivos tipos e anos
tab_mod_year_type <- function(){
  tab_mod_year_type <- expand_grid(
    model = type_period_models()$model[1], 
    type = type_period_models()$type[1],
    year = seq(from = type_period_models()$start[1], to = type_period_models()$end[1])
  ) 
  for(i in 2:length(type_period_models()$model)){
    tab_mod_year_type <- tab_mod_year_type%>% 
      rbind(
        expand_grid(
          model = type_period_models()$model[i], 
          type = type_period_models()$type[i],
          year = seq(from = type_period_models()$start[i], to = type_period_models()$end[i])
        ) 
      )
  }
  tab_mod_year_type
}

# criando uma tabela com as informacoes necessarias para realizacao do download
tab_mod_year_vname_type <- full_join(tab_mod_year_type(), select(names_vars_models(),-(prec))) %>% 
  pivot_longer(cols = c(tmax, tmin), names_to = "variable", values_to = "vname") %>%
  select(model, year, type, variable) %>%
  arrange(model, year) %>%
  relocate(year, model, variable, type)

