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
  
  table_control <- expand.grid(models_1, variables_1) %>% 
    rbind(expand.grid(models_2,variables_2))%>% 
    rbind(expand.grid(models_3, variables_3)) %>% 
    rbind(expand.grid(models_4, variables_4)) %>% 
    setNames(c("model", "variable")) %>%
    as_tibble() %>%   
    mutate_all(.funs = as.character) %>%  
    arrange(model, variable)
  
  table_control <- table_control %>%
    mutate(id = ifelse(str_detect(variable, "x"), 
                       "tmax",
                       ifelse(str_detect(variable, "n"), "tmin", "prec"))
    ) %>% 
    pivot_wider(names_from = 'id', values_from = variable)
  table_control
}


tab_mod_anos_vname_type <- function(){
  
  vnames <- c("tmax", "tmin")
  models <- names_vars_models()$model
  
  # tabela com a combinacao de anos e modelos para tipo hindcast
  #periodo hindcast
  start_y_hindcast <- 1980
  end_y_hindcast <- 2020
  tab_mod_anos_vname_hindcast <-
    expand.grid(models, start_y_hindcast:end_y_hindcast , vnames, "HINDCAST") %>%
    as_tibble() %>%
    mutate(across(.fns = as.character)) %>%
    rename("model" = "Var1", "year" = "Var2", "variable" = "Var3", "type" = "Var4") %>%
    arrange(model) %>%
    relocate(year, model, variable, type)
  
  # tabela com a combinacao de anos e modelos para tipo forecast
  #periodo forecast
  start_y_forecast <- 2016
  end_y_forecast <- 2022
  tab_mod_anos_vname_forecast <-
    expand.grid(models, start_y_forecast:end_y_forecast , vnames, "FORECAST") %>%
    as_tibble() %>%
    mutate(across(.fns = as.character)) %>%
    rename("model" = "Var1", "year" = "Var2", "variable" = "Var3", "type" = "Var4") %>%
    arrange(model) %>%
    relocate(year, model, variable, type)
  
  tab_mod_anos_vname_type <-
    full_join(tab_mod_anos_vname_hindcast, tab_mod_anos_vname_forecast) %>% 
    arrange(model, variable)
  
  tab_mod_anos_vname_type
}
