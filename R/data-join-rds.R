


# junta medias nas bacias dos membros nmme
join_files_model_nmme <- function(model_dir, format = c("qs", "RDS")){
  
  # model_dir <- nmme_models_d[7]; format = c("qs")
  checkmate::assert_choice(format, c("qs", "RDS"))
  
  # lista arquivos
  files_bin <- fs::dir_ls(model_dir, 
                          type = "file",
                          glob = glue::glue("*.{format}")
                          )
  
  # importa
  basin_avg_by_sl <- purrr::map_df(files_bin, import_bin_file) %>%
    dplyr::arrange(codONS, S, L)
  
  # ajusta data de inicializacao (S) para ter uma para cada mês do ano
  # gerando Sr (S arredondada)
  basin_avg_by_sl <- basin_avg_by_sl  %>%
    dplyr::mutate(
          Sr = floor_date(S + ddays(15), "month"),
          #date_lead = S + dmonths(trunc(L)) + ddays(15),
          #date_lead = lubridate::floor_date(as_date(date_lead), "month"),
          date_lead = Sr + dmonths(trunc(L)) + ddays(15),
          date_lead = lubridate::as_date(lubridate::floor_date(date_lead, "month"))
          )  %>% 
    dplyr::relocate(model, S, Sr, L, date_lead)
  basin_avg_by_sl
  
}


# join bin files of basin averages of a model by S and L ------------------


join_nmme_basin_avg_files <- function(sp_average = "weighted", format = "qs"){
  
  # diretorio das medias nas areas das bacias da variavel de interesse
  # diretorios separados por modelo
  basin_avg_d <- here(glue::glue("output/{format}/basin-avgs/{sp_average}"))
  # arquivos separados por S e L
  nmme_models_d <- dir_ls(basin_avg_d, type = "directory")
  
  # nome do arquivo de saida com juncao dos dados dos arquivos
  joined_basin_avgs <- path(
    basin_avg_d,
    glue::glue("nmme-models-{sp_average}-avg-basins-ons.{format}")
  )  
  
  
  if(!checkmate::test_file_exists(joined_basin_avgs)){
    
    tictoc::tic()
    basin_avg_nmme <- map_df(nmme_models_d, join_files_model_nmme)
    tictoc::toc()
    # 5 min elapsed
    basin_avg_nmme <- basin_avg_nmme %>%
      group_by(model) %>%
      nest()
    
    export_bin_file(basin_avg_nmme, joined_basin_avgs)
    
  } else {
    basin_avg_nmme <- import_bin_file(joined_basin_avgs)
  }
  
  basin_avg_nmme
  
}







# # teste para conversao de S,L para date ----------------------------------
# data_test <- expand.grid(S = unique(data_trans$S),L = unique(data_trans$L)) %>%
#   as_tibble() %>%
#   arrange(S, L) 
# paste0(day(data_test$S), "-", month(data_test$S))  
# 
# data_test <- data_test %>%
#   dplyr::mutate(
#     date_lead = S + dmonths(trunc(L)) + ddays(15),
#     date_lead = lubridate::floor_date(as_date(date_lead), "month")
#   )
# head(as.data.frame(select(data_test, S:date_lead)), 30)
# tail(as.data.frame(select(data_test, S:date_lead)), 30)