
# Agregacao das series dos membros media e desvio padrao
# para agregar as previsoes pela media do ensemble de cada modelo


pcks <- c(
  "tidyverse", "here", "HEobs",
  "checkmate", "lubridate",
  "tictoc" 
  #"openair", "ggpubr", "ggExtra", "viridis", "see", "ggh4x"
)

easypackages::libraries(pcks)


# devtools::install_github("lhmet-ped/HEobs")

#------------------------------------------------------------------------------
# funcoes auxiliares
source(here("R/utils.R"))
source(here("R/data-proc-rds.R"))


# Funcao para agregar membros de cada modelo pela media

aggregate_nmme_members <- function(
  avg_type = "weighted", # melhores resultados
  extension = "qs",
  var_name = "prec",  
  funs_list = list(
    avg = mean,
    med = median,
    sd = sd,
    mad = mad
  )
){
  nmme_cru_basin_data <- import_bin_file(
    .filename_basin_data(avg_type, extension)
  ) %>%
    dplyr::rename("n_L" = L)
  
  # media dos membros dos modelos-------------------------------------------------
  # 1 previsao por media do ensemble dos membros
  tic()
  # ensemble summary
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
  toc()
  
  #! 18 min
  # check NASA-GEOSS2S, esperado 8 (9 lead times)
  # tail(nmme_cru_basin_data_ens[["data"]][[8]])
  # tail(nmme_cru_basin_data_ens[["data"]][[1]]) # 11
  
  out_fname <- fs::path_file(.filename_basin_data(avg_type, extension)) %>%
    stringr::str_split("\\.") %>% 
    unlist() %>%
    nth(1) %>%
    glue::glue("-ens-smry.{extension}") %>%
    fs::path(fs::path_dir(.filename_basin_data(avg_type, extension)), .)
  
  export_bin_file(nmme_cru_basin_data_ens, out_fname)
  checkmate::assert_file_exists(out_fname)
  return(out_fname)
  
}


# aplica funcao para medias dos membros por modelo ----------------------------

#NAO TESTADA AINDA POR CAUSA DA DEMORA DE 18 min para rodar
aggregate_nmme_members(
  avg_type = "weighted", # melhores resultados
  extension = "qs",
  var_name = "prec",  
  funs_list = list(
    avg = mean,
    med = median,
    sd = sd,
    mad = mad
  )
)
  
# importar arquivo resultante para analise posteriores
