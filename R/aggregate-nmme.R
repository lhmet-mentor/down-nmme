# Funcao para agregar membros de cada modelo em média do conjunto

aggregate_members_nmme <- function(
  avg_type = "weighted", # melhores resultados
  extension = "qs",
  var_name = "prec",  
  suffix = "ens-members",
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
    dplyr::ungroup() %>%
    tidyr::unnest("data")
  
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
  
  #ens <- ens %>% dplyr::select(-c(prec_obs_med, prec_obs_sd, prec_obs_mad)) 
  
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
  #                         funs_list
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
    glue::glue("-{suffix}-{var_name}.{extension}") %>%
    fs::path(fs::path_dir(.filename_basin_data(avg_type, extension)), .)
  
  export_bin_file(ens, out_fname)
  checkmate::assert_file_exists(out_fname)
  message("File saved in: ", "\n", out_fname)
  out_fname
  
}

# periodo de anos com mais modelos
best_period <- function(model_info){
  tab_start <- table(model_info$start)
  ys <- as.numeric(names(which.max(tab_start)))
  tab_end <- table(model_info$end)
  ye <- as.numeric(names(which.max(tab_end)))
  period_optm <- c(ys, ye)
  period_optm
}


#------------------------------------------------------------------------------
#' Filtragem das previsões médias de cada modelo do NMME pelo período ótimo
#'
#' @param nmme_model_data dados com as previsões médias dos modelos
#' @param model_info_file arquivo com informações de período de anos e dimensões
#' dos arquivos NetCDF.
#'
#' @return dados com os modelos que possuam dados para o período ótimo (1982-2010).
#' @export
#'
#' @examples
filter_data_by_commom_period <- function(nmme_model_data, 
                                         model_info_file = "output/qs/model_counts.qs"){
  # filtragem para manter o periodo comum de dados entre os modelo ------------
  
  assert_file_exists(model_info_file)
  models_summary <- import_bin_file(model_info_file)
  # periodo comum (1982-2010) para filtragem de meses
  years_sel <- best_period(models_summary)
  # para incluir CFSv2!
  years_sel[1] <- years_sel[1] + 1 # 1982
  # para nome do arquivo de saida
  years_rng <- paste0(years_sel, collapse = "-")
  
  models_select <- models_summary %>%
    dplyr::filter(start <= years_sel[1] & end >= years_sel[2]) %>%
    dplyr::pull(modelo)
  
  nmme_model_data <- nmme_model_data %>%
    dplyr::filter(model %in% models_select) %>%
    dplyr::filter(year(date) >= years_sel[1] & year(date) <= years_sel[2]) # %>% filter(model == "GFDL-SPEAR")  
  nmme_model_data
}


#------------------------------------------------------------------------------
#' Média dos previsões do modelos
#' 
#' @param var_target the target variable to which the member summary statistics will apply.
#' @param avg_type "weighted" (default) or "arithmetic" (not available).
#' It defines the path to the file.
#' @param extension "qs", file extension from file to be imported.
#' It defines the path to the file.
#' @param var_name "prec".
#' @param funs_list named list object with the functions to summarize data.
#' @param prefix "nmme-mly-ens-mean-1982-2010", prefix for the output file.
#'
#' @return file path to the qs file. Data are storage in tibble with columns 
#' `codONS`, `date`, `L`, `ens_avg`, `ens_med`, `ens_sd`, `ens_mad`.
#' @details 
#' Os dados de entrada são as previsões médias de cada modelo obtida das séries
#' dos membros de cada modelo. 
#' Nesta primeira versão a função é aplicada somente a média dos membros. 
#' @export
#'
#' @examples
aggregate_models <- function(var_target = c("members_avg"),
                             avg_type = "weighted", # melhores resultados
                             extension = "qs",
                             var_name = "prec",
                             funs_list = list(
                               avg = mean,
                               med = median,
                               sd = sd,
                               mad = mad
                             ),
                             suffix = "ens-models"
                             ) {
  # previsoes medias do ensemble (1 prev por modelo)
  nmme_data_file <- .filename_basin_data(avg_type,
                                         extension, 
                                         glue::glue("-ens-members-{var_name}")
                                         )
  checkmate::assert_file_exists(nmme_data_file)
  
  # read data
  nmme_data <- import_bin_file(nmme_data_file)
    
  
  names(nmme_data)[-1] <- 
    # tira "prec" do nome das variaveis
    stringr::str_replace(names(nmme_data)[-1], "prec_","") %>%
    # substitui 'model' por 'members' para deixar claro que sao
    # estatisticas dos membros
    stringr::str_replace(., "model", "members")
  
  # como medias e medianas das obs sao iguais, removemos elas
  nmme_data <- nmme_data %>%
    dplyr::select(-c(obs_med, obs_sd, obs_mad)) %>%
    #! mudar members_avg por outra variavel se for de interesse
    dplyr::select(model:date, all_of(var_target))
  
  #unique(nmme_data$model)
  
  nmme_data <- filter_data_by_commom_period(nmme_data)
  
  # unique(nmme_data$model)
  # models_select
  
  ## check
  # data_select %>%
  #    group_by(model) %>%
  #    summarise(start = min(date), end = max(date))

  # 8 modelos por bacia
  # nmme_data %>% filter(
  #                     codONS == 6,
  #                     L == 1,
  #                     Sr == as.Date("1981-12-01"),
  #                     date == as.Date("1982-01-01")
  #                     )
  
  
  
  models_avg <- nmme_data %>%
    dplyr::group_by(codONS, Sr, date, L) %>%
    dplyr::summarise(dplyr::across(!!sym(var_target), funs_list), 
                     .groups = "drop"
    ) %>%
    # renomeia para ens_x
     arrange(L, codONS) %>%
    dplyr::rename_with(~ str_replace(.x, var_target, "ens"),
                       contains("members")
                       )
  # unique(models_avg$model)
  
  ens_mean_file <- fs::path_file(.filename_basin_data(avg_type, extension)) %>%
    stringr::str_split("\\.") %>% 
    unlist() %>%
    nth(1) %>%
    glue::glue("-{suffix}-{var_name}-{years_rng}.{extension}") %>%
    fs::path(fs::path_dir(.filename_basin_data(avg_type, extension)), .)
  
  
  export_bin_file(models_avg, file = ens_mean_file)
  message("File saved in: ", "\n", ens_mean_file)
  
  ens_mean_file
  
}


# Juncao dos medias dos modelos com a media ensemble --------------------------
join_nmme_model_ensemble <- function(nmme_ens_file, 
                                     nmme_data_file,
                                     suffix = "ens-mean",
                                     var_name = "prec"){
  
  checkmate::assert_file_exists(nmme_ens_file)
  checkmate::assert_file_exists(nmme_data_file)
  
  nmme_ens_data <- import_bin_file(nmme_ens_file)  
  
  nmme_models_data <- import_bin_file(nmme_data_file) %>%
    dplyr::rename_with(~ str_replace(.x, "prec_", ""),
                       contains("prec")
    ) %>%
    dplyr::select(-c(obs_med, obs_sd, obs_mad)) 
 
  
  nmme_join <- nmme_models_data %>%
    dplyr::inner_join(nmme_ens_data, by = c("codONS", "Sr","date", "L")) %>%
    dplyr::select(model:date, contains("avg"), contains("sd"))
  
  unique(nmme_join$model)
  # nmme_join %>% group_by(model) %>% summarise(Lmax = max(L))
  # # A tibble: 10 × 2
  # model        Lmax 
  # <chr>        <ord>
  #   1 CanCM4i      11   
  # 2 CanSIPS-IC3  11   
  # 3 CanSIPSv2    11   
  # 4 CMC1-CanCM3  11   
  # 5 CMC2-CanCM4  11   
  # 6 GEM-NEMO     11   
  # 7 GFDL-SPEAR   11   
  # 8 NASA-GEOSS2S 8    *
  # 9 NCAR-CESM1   11   
  # 10 NCEP-CFSv2   9 
  
  
  nmme_ens_file_split <- stringr::str_split(nmme_ens_file, var_name)[[1]]
  
  nmme_join_file <- nmme_ens_file_split[1] %>%
    stringr::str_replace(., "models", "members") %>%
    glue::glue("{suffix}-{var_name}") %>%
    paste0(., nmme_ens_file_split[2])
  
  export_bin_file(nmme_join, file = nmme_join_file)
  nmme_join_file
  
}

















  