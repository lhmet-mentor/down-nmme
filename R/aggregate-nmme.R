# Funcao para agregar membros de cada modelo pela media

aggregate_members_nmme <- function(
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
  # flat data (54,333,414 rows)
  nmme_cru_basin_data <- import_bin_file(
    .filename_basin_data(avg_type, extension)
  ) %>%
    dplyr::select(model, data) %>%
    ungroup() %>%
    unnest("data")
  
  # media dos membros dos modelos-------------------------------------------------
  # 1 previsao por media do ensemble dos membros
  
  nmme_cru_basin_data <- data.table::as.data.table(nmme_cru_basin_data)
  
  tictoc::tic()
  ens <- nmme_cru_basin_data[,  
                      c(lapply(.SD, mean), lapply(.SD, median),
                        lapply(.SD, sd), lapply(.SD, mad)),
                       by = .(model, codONS, Sr, L, date),
                      .SDcols = patterns(var_name)]
  tictoc::toc()
  #! 865.498 sec elapsed
  #! 14 min
  names(ens)[grep("prec", names(ens))] <- 
    paste(names(ens)[grep("prec", names(ens))],
          rep(c("avg", "med", "sd", "mad"), each = 2),
          sep = "_"
          )
  ens[, L := factor(trunc(L), levels = 0:11, ordered = TRUE)]
  ens <- tibble::as_tibble(ens)
  
  # media dos membros dos modelos-------------------------------------------------
  # 1 previsao por media do ensemble dos membros
  # tic()
  # # ensemble summary
  # nmme_cru_basin_data_ens <- 
  #   nmme_cru_basin_data %>%
  #   ungroup() %>%
  #   dplyr::mutate(.,
  #                 data = map(
  #                   data,
  #                   ~ .x %>%
  #                     dplyr::select(-S) %>%
  #                     dplyr::mutate(L = factor(trunc(L), 
  #                                              levels = 0:11, 
  #                                              ordered = TRUE)
  #                     ) %>%
  #                     dplyr::group_by(codONS, L, date) %>%
  #                     dplyr::summarise(
  #                       dplyr::across(
  #                         dplyr::contains(var_name),
  #                         funs_l
  #                       ),
  #                       .groups = "drop"
  #                     )
  #                 )
  #   )
  # toc()
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
  
  export_bin_file(ens, out_fname)
  checkmate::assert_file_exists(out_fname)
  return(out_fname)
  
}