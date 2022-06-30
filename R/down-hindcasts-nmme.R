pcks <- c("terra", 
          "tidync", 
          "tidyverse", 
          "fs", 
          "tictoc", 
          "data.table",
          "here")

easypackages::libraries(pcks)
#options(timeout = 150)

#------------------------------------------------------------------------------
# scripts
source(here("R/models-nmme.R"))
#names_vars_models()
source(here("R/down-nmme.R"))


#------------------------------------------------------------------------------
# periodo
start_y <- 1980
end_y <- 2020

# modelos
#modelos <- tabela1$modelo
modelos <- tail(tabela1$modelo, 2)

# tabela com a combinacao de anos e modelos
tab_mod_anos <- expand.grid(modelos, start_y:end_y) %>%
  as_tibble() %>%
  mutate(across(.fns = as.character)) %>%
  rename("modelo" = "Var1", "ano" = "Var2") %>%
  arrange(modelo)

# lista para looping multivariado
modelos_l <- as.list(tab_mod_anos$modelo)
anos_l <- as.list(tab_mod_anos$ano)


# looping para baixar NetCDF para o intervalo de anos e modelos
tic()
baixados_prec <- map2(modelos_l, anos_l, ~down_nmme(modelo = .x, ano = .y))
toc()

