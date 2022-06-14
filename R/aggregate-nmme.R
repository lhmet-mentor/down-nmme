# Funcao para agregar membros de cada modelo em média do conjunto

aggregate_members_nmme <- function(
  avg_type = "weighted", # melhores resultados
  extension = "qs",
  var_name = "prec",  
  out_file_suffix = "ens-members",
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
  nmme_cru_basin_data <- nmme_cru_basin_data[, !"S"]
  
  # nmme_cru_basin_data_w <- data.table::dcast(nmme_cru_basin_data, 
  #                   model + codONS + Sr + L + date + prec_obs ~ member,  
  #                   value.var = "prec_model"
  # )
  
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
    glue::glue("-{out_file_suffix}-{var_name}.{extension}") %>%
    fs::path(fs::path_dir(.filename_basin_data(avg_type, extension)), .)
  
  export_bin_file(ens, out_fname)
  checkmate::assert_file_exists(out_fname)
  message("File saved in: ", "\n", out_fname)
  out_fname
  
}




#-------------------------------------------------------------------------------
#' 
#' Espalha as previsões dos membros de cada modelo através das colunas
#'
#' @param avg_type "weighted" (default) or "arithmetic" (not available).
#' It defines the path to the file.
#' @param extension "qs", file extension from file to be imported.
#' It defines the path to the file.
#' @param var_name variable of interest (default: 'prec').
#' @param out_file_suffix character or NULL. Suffix for the output file, default
#'  is 'wide-flat'. IF it is NULL do not write data in a file.
#'
#' @return 
#' @export
#'
#' @examples
spread_members_nmme <- function(
    avg_type = "weighted", # melhores resultados
    extension = "qs",
    var_name = "prec",  
    out_file_suffix = "wide-flat"
){
  # flat data (54,333,414 rows)
  nmme_cru_basin_data <- import_bin_file(
    .filename_basin_data(avg_type, extension)
  ) %>%
    dplyr::select(model, data) %>%
    dplyr::ungroup() %>%
    tidyr::unnest("data")
  
  
  nmme_cru_basin_data <- data.table::as.data.table(nmme_cru_basin_data)
  nmme_cru_basin_data <- nmme_cru_basin_data[, !"S"]
  
  # espalha membros nas colunas
  nmme_cru_basin_data_w <- data.table::dcast(
    nmme_cru_basin_data, 
    model + codONS + Sr + L + date ~ member,  
    value.var = paste0(glue::glue("{var_name}_model"))
  ) %>% 
    tibble::as_tibble() %>%
    # adiciona prefixo 'member'
    dplyr::rename_with(~ paste0("member_", .x), dplyr::matches("[0-9]{1,2}"))
  
  
  if(is.null(out_file_suffix)){
    return(nmme_cru_basin_data_w)
  }
  
  # out file name
  nmme_spread_file <- stringr::str_split(
    .filename_basin_data(avg_type, extension), 
    "\\.")[[1]][1] %>%
    paste0(glue::glue("-{out_file_suffix}.{extension}"))
  
  # export spread data  
  export_bin_file(nmme_cru_basin_data_w, file = nmme_spread_file)
  checkmate::assert_file_exists(nmme_spread_file)
  message("File saved in: ", "\n", nmme_spread_file)
  nmme_spread_file
}



#------------------------------------------------------------------------------
# periodo de anos com mais modelos
best_period <- function(model_info){
  #model_info = models_summary
  tab_start <- table(model_info$start)
  ys <- as.numeric(names(which.max(tab_start)))
  tab_end <- table(model_info$end)
  ye <- as.numeric(names(which.max(tab_end)))
  period_optm <- c(ys, ye)
  period_optm
}


set_years_period <- function(model_info_file = "output/qs/model_counts.qs"){
  assert_file_exists(model_info_file)
  models_summary <- import_bin_file(model_info_file)
  # periodo comum (1982-2010) para filtragem de meses
  years_sel <- best_period(models_summary)
  
  #*******************************************************************
  # para incluir CFSv2!
  years_sel[1] <- years_sel[1] + 1 # 1982
  #*******************************************************************
  
  years_sel
  
}



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
                                         years_sel){
  # nmme_model_data = nmme_data
  # filtragem para manter o periodo comum de dados entre os modelo ------------

  years_sel <- set_years_period(model_info_file = "output/qs/model_counts.qs")
  
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



#' Média dos previsões do modelos
#' 
#' @param var_target the target variable to which the member summary statistics will apply.
#' @param avg_type "weighted" (default) or "arithmetic" (not available).
#' It defines the path to the file.
#' @param extension "qs", file extension from file to be imported.
#' It defines the path to the file.
#' @param var_name "prec".
#' @param funs_list named list object with the functions to summarize data.
#' @param out_file_suffix "ens-models", suffix for the output file.
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
                             out_file_suffix = "ens-models"
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
  
  years_rng <- set_years_period(
    model_info_file = "output/qs/model_counts.qs"
    ) %>%
    paste0(collapse = "-")
  
  
  
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
    glue::glue("-{out_file_suffix}-{var_name}-{years_rng}.{extension}") %>%
    fs::path(fs::path_dir(.filename_basin_data(avg_type, extension)), .)
  
  
  export_bin_file(models_avg, file = ens_mean_file)
  checkmate
  message("File saved in: ", "\n", ens_mean_file)
  
  ens_mean_file
  
}



#' Juncao das previsões médias de cada modelo com a média e desvio padrão 
#' do conjunto.
#'
#' @param nmme_ens_file nome do arquivo com média ensemble.
#' @param nmme_data_file nome do arquivo com as previsões médias de cada modelo. 
#' @param out_file_suffix sufixo do arquivo de saída. Será apendado ao 
#' @param var_name variável de interesse (defaul: 'prec').
#'
#' @return
#' @export
#'
#' @examples
join_nmme_model_ensemble <- function(nmme_ens_file, 
                                     nmme_data_file,
                                     out_file_suffix = "ens-mean",
                                     var_name = "prec"
                                     ){
  
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
  
  #unique(nmme_join$model)
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
  
  # if(add_members){
  #   spread_members_nmme()
  # }
  
  nmme_ens_file_split <- stringr::str_split(nmme_ens_file, var_name)[[1]]
  
  nmme_join_file <- nmme_ens_file_split[1] %>%
    stringr::str_replace(., "models", "members") %>%
    glue::glue("{out_file_suffix}-{var_name}") %>%
    paste0(., nmme_ens_file_split[2])
  
  export_bin_file(nmme_join, file = nmme_join_file)
  checkmate::assert_file_exists(nmme_join_file)
  message("File saved in: ", "\n", nmme_join_file)
  nmme_join_file
  
}




#' Junção das previsões dos membros, médias por modelo e média ensemble com a
#' a climatologia da variável
#' 
join_ensemble_climatology <- function(
    data_members, 
    new_var_name = "climatology"
){
  # members_file = here('output/qs/basin-avgs/weighted/nmme-cru-mly-weighted-avg-basins-ons-ens-members-models-ens-mean-prec-1982-2010.qs')
  cli <- climatology_nmme_cru(overwrite = FALSE) %>%
    dplyr::select(model:month, contains("obs_avg")) %>%
    dplyr::rename({{var_name}} := prec_obs_avg)
  
  data_members <- #import_bin_file(members_file) %>%
    data_members %>%
    dplyr::mutate(month = lubridate::month(date))
  
  data_j <- dplyr::inner_join(data_members, cli, 
                              by = c('model', 'codONS', 'L', 'month')
                              )
  data_j %>%
    dplyr::relocate({{new_var_name}}, .after = 'ens_avg') 
}


#' Juncao das previsões dos modelos e média ensemble com as previsões
#' de cada membro 
join_nmme_models_members_ensemble <- function(
    nmme_models_file = ens_models_join_file,
    nmme_members_file = nmme_members_wide_file,
    var_name = "prec",
    out_file_suffix = "members-models-ens-mean",
    climatology = TRUE
){
  
  checkmate::assert_file_exists(nmme_models_file)
  checkmate::assert_file_exists(nmme_members_file)
  
  models_data <- import_bin_file(nmme_models_file)
  members_data <- import_bin_file(nmme_members_file) %>%
    dplyr::mutate(L = factor(trunc(L), levels = 0:11, ordered = TRUE))
  
  nmme_join <- dplyr::inner_join(
    models_data, 
    members_data, 
    by = c("model", "codONS", "Sr","date", "L")
  ) 
  #range(nmme_join$date)
  
  if(climatology){
    nmme_join <- join_ensemble_climatology(
      nmme_join, new_var_name = "climatology"
      ) 
  }
  
  nmme_join_file_split <- stringr::str_split(nmme_models_file, var_name)[[1]]
  
  nmme_join_file <- nmme_join_file_split[1] %>%
    stringr::str_replace(., "members-ens-mean", out_file_suffix) %>%
    glue::glue("{var_name}") %>%
    paste0(., nmme_join_file_split[2])
  
  export_bin_file(nmme_join, file = nmme_join_file)
  checkmate::assert_file_exists(nmme_join_file)
  message("File saved in: ", "\n", nmme_join_file)
  nmme_join_file
  
}


#-------------------------------------------------------------------------------
# Funcoes para arrumar os dados no formato para modelagem estatistica e 
# posprocessamento.

# nomes dos modelos limpos para usar nas colunas-------------------------------
.clean_names_nmme <- function(clim_prevs){
  model_nms <- unique(clim_prevs$model)
  models_nms_clean <- data.frame(matrix(NA, ncol = length(model_nms))) %>%
    set_names(tolower(model_nms)) %>%
    janitor::clean_names() %>%
    names()
  
  level_key <- setNames(models_nms_clean, model_nms)
  
  # nomes com ponto para juntar com nomes dos modelos para deixar dados em formato
  # amplo.
  clim_prevs <- clim_prevs %>%
    dplyr::mutate(model = recode(model, !!!level_key)) %>%
    dplyr::rename_with(~ str_replace_all(.x, "_", "\\.")) %>%
    dplyr::rename_with(~ str_replace_all(.x, "member|model", "m"), -model) %>%
    dplyr::rename_with(~ str_replace_all(.x, "avg", "mean"))  
  
  clim_prevs
  
  # names(clim_prevs)
  # [1] "model"       "codONS"      "Sr"          "L"          
  # [5] "date"        "m.mean"      "obs.mean"    "ens.mean"   
  # [9] "climatology" "m.sd"        "ens.sd"      "m.1"        
  # [13] "m.2"         "m.3"         "m.4"         "m.5"        
  # [17] "m.6"         "m.7"         "m.8"         "m.9"        
  # [21] "m.10"        "m.11"        "m.12"        "m.13"       
  # [25] "m.14"        "m.15"        "m.16"        "m.17"       
  # [29] "m.18"        "m.19"        "m.20"        "m.21"       
  # [33] "m.22"        "m.23"        "m.24"        "month"
}

# verificar nomes das variaveis nos dados
.check_names_nmme_data <- function(data_names){
  checkmate::assert_character(data_names)
  checkmate::assert_names(data_names,
                          identical.to = c(
                            "model", "codONS", "Sr", "L", "date", "model_avg", "obs_avg",
                            "ens_avg", "climatology", "model_sd", "ens_sd", "member_1", "member_2",
                            "member_3", "member_4", "member_5", "member_6", "member_7", "member_8",
                            "member_9", "member_10", "member_11", "member_12", "member_13",
                            "member_14", "member_15", "member_16", "member_17", "member_18",
                            "member_19", "member_20", "member_21", "member_22", "member_23",
                            "member_24", "month"
                          )
  )  
}

# espalhar previsoes dos membros, medias modelos e medias ensemble nas colunas
.spread_fcsts_nmme <- function(.nmme_data, 
                               .ibasin,
                               .imonth,
                               .lead_time,
                               .model_exclude = 'gfdl_spear'){
  data_pp_wide <- .nmme_data %>%
    select(-obs.mean, -climatology) %>%
    dplyr::filter(!model %in% .model_exclude) %>%
    dplyr::filter(
      codONS == .ibasin,
      month(date) %in% .imonth,
      # model %in% imodel,
      L %in% .lead_time
    ) %>% # pull(month) %>% unique()
    tidyr::pivot_longer(-c(model:date, month),
                        names_to = "previsao",
                        values_to = "valor"
    ) %>%
    tidyr::unite("forecast", c("model", "previsao")) %>%
    tidyr::pivot_wider(names_from = "forecast", values_from = "valor") %>%
    janitor::remove_empty(which = "cols")
  
  obs_per_basins <- .nmme_data %>%
    dplyr::filter(!model %in% .model_exclude) %>%
    dplyr::filter(
      codONS == .ibasin,
      month(date) %in% .imonth,
      # model %in% imodel,
      L %in% .lead_time
    ) %>%
    dplyr::select(codONS, date, obs.mean) %>%
    dplyr::distinct(codONS, date, obs.mean)
  
  
  data_pp_all <- inner_join(data_pp_wide, 
                            obs_per_basins, 
                            by = c("codONS", "date")
                            ) %>%
    dplyr::relocate(obs.mean, .after = month)
  # names(data_pp_all)
  
  # remove variaveis redundantes
  data_pp_all <- data_pp_all %>%
    dplyr::rename(
      "ens.mean" = cancm4i_ens.mean,
      "ens.sd" = cancm4i_ens.sd
    ) %>%
    dplyr::select(-contains("_ens.mean"), -contains("_ens.sd")) 
  
}


#' Spread all NMME's forecasts along columns 
#'
#' Tidy data forecasts for a given basin, month and lead time. This is applied 
#' before build a statistical model to pos-process the forecasts. 
#'
#' @param data_file file of processed data
#' @param ibasin ONS basin code
#' @param imonth month of interest
#' @param lead_time lead time(s)
#' @param model_exclude model to exclude from because the diferent span of models.
#' Default is 'gfdl_spear'.
#'
#' @return 
#' @export
#'
#' @examples
spread_all_nmme <- function(nmme_data,
                           ibasin,
                           imonth,
                           lead_time,
                           model_exclude = "gfdl_spear") {
  # data_file = "/home/hidrometeorologista/Dropbox/github/my_reps/lhmet/download-hindcast-NMME/output/qs/basin-avgs/weighted/nmme-cru-mly-weighted-avg-basins-ons-ens-members-models-ens-mean-prec-1982-2010.qs"
  # ibasin = 6;   imonth = 1;  lead_time = 1:2; model_exclude = "gfdl_spear"
  
  #nmme_data <- import_bin_file(data_file)
  # range(nmme_data$date)
  .check_names_nmme_data(names(nmme_data))
  
  # limpeza de nomes das vars
  nmme_data <- .clean_names_nmme(nmme_data)
  
  .spread_fcsts_nmme(nmme_data, 
                     .ibasin = ibasin,
                     .imonth = imonth,
                     .lead_time = lead_time,
                     .model_exclude = model_exclude
                     )
  
}
