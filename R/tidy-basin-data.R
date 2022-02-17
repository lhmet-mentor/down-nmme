
#' Tidy basin average data from models in NMME system
#'
#' @param var_name e.g. 'prec', 'tmax'
#' @param out_dir e.g `output/{qs,RDS}/basin-avgs/{arithmetic, weighted}`
#' @param sp_average 'arithmetic' or 'weighted'
#' @param ext file extension for read/write processed data, 'qs' or 'RDS' are accepted
#'
#' @return tidy data in a tibble
#' @export
#'
#' @examples
tidy_nmme_basin_data <- function(.var_name = "prec", 
                                 .sp_average = "weighted",
                                 .ext = "qs",
                                 .stat = "identity",
                                 .out_dir = here(glue::glue("output/{.ext}/basin-avgs/{.sp_average}"))
                                 ) {
  
  basin_avg_nmme_file <- here(
    .out_dir,
    glue::glue("nmme-models-{.sp_average}-avg-basins-ons.{.ext}")
  )
  
  checkmate::assert_file_exists(basin_avg_nmme_file)
  
  # resumo de informacoes dos modelos
  path_model_summary <- fs::path_split(.out_dir) %>% 
    `[[`(., 1) 
  path_model_summary <- fs::path_join(
    path_model_summary[1:(grep("basin", path_model_summary)-1)]
  )
  models_summary <- import_bin_file(glue::glue("{path_model_summary}/model_counts.{.ext}"))
  
  # importa dados nmme medias nas bacias
  prec_nmme_avg_basin <- import_bin_file(basin_avg_nmme_file) %>%
    mutate(data = map(
      data, ~ .x %>%
        rename("date" = date_lead)
    ))
  
  # prec_nmme_avg_basin[["data"]][[1]] # wide
  # adiciona info dos membros, leads e freq (#anos de previsoes retorospectivas)
  prec_nmme_avg_basin <- prec_nmme_avg_basin %>%
    inner_join(dplyr::select(models_summary, modelo, M, L, freq),
               by = c("model" = "modelo")
    ) %>%
    dplyr::rename("n_members" = "M")
  
  # tidy nmme data considering 'stat'
  if (.stat == "identity") {
    # pivota membros nas linhas
    tictoc::tic()
    prec_nmme_avg_basin_long <- prec_nmme_avg_basin %>%
      dplyr::mutate(data = purrr::map(
        data,
        ~ .x %>%
          tidyr::pivot_longer(
            cols = dplyr::contains(.var_name),
            names_to = "member",
            names_prefix = paste0(glue::glue("{.var_name}_")),
            names_transform = list(member = as.integer),
            values_to = paste0(glue::glue("{.var_name}_model"))
          ) %>%
          dplyr::mutate(prec_model = prec_model * 30) %>% # 30 eh o num de dias no calendario dos modelos
          # because pivot_longer expand members to the max 
          # members of the models (24)
          dplyr::filter(!is.na(prec_model)) #%>%
        #dplyr::arrange(S, L, codONS, member)
      ))
    
    tictoc::toc()
    # 124.885 s
    
    gc()
  } else {
    prec_nmme_avg_basin_long <- prec_nmme_avg_basin %>%
      dplyr::mutate(data = purrr::map(
        data,
        ~ .x %>%
          dplyr::rename("prec_model" = prec) %>%
          dplyr::mutate(
            prec_model = prec_model * 30,
            member = 0L
          ) #%>% # 30 eh o num de dias no calendario dos modelos
        #dplyr::arrange(S, L, codONS, member)
      ))
  }
  
  
  prec_nmme_avg_basin_long
  
}


# CRU --------------------------------------------------------------------------
tidy_cru_basin_data <- function(
  .var_name = "prec",
  .sp_average = "weighted",
  .ext = "qs",
  .out_dir = here(glue::glue("output/{.ext}/basin-avgs/{.sp_average}"))
){
  
  basin_avg_cru_file <- here(
    .out_dir,
    glue::glue("cru-{.var_name}-basins-{.sp_average}-avg.{.ext}")
  )  
  
  checkmate::assert_file_exists(basin_avg_cru_file)
  
  cru_basin_data <- import_bin_file(basin_avg_cru_file) %>%
    dplyr::mutate(
      ID = NULL,
      # ajuste das datas, pq no CRU as datas sao do meio do mes
      date = lubridate::floor_date(date, unit = "months")
    )  
  
  cru_basin_data
  
}



# # check how to join-----
# pred <- nmme_basin_data[["data"]][[1]]
# #summary(pred)
# #dplyr::arrange(pred, codONS, member)
# 
# comb_nmme_obs <- dplyr::inner_join(pred, cru_basin_data,
#                                    by = c("date", "codONS")
# )
# filter(comb_nmme_obs, codONS == 1, date == as_date("2000-12-01"), L == 1.5) %>%
#   as.data.frame()
# tail(as.data.frame(comb_nmme_obs), 30)


# join ------------------------------------------------------------------------
join_cru_nmme_basin_data <- function(sp_average = "weighted",
                                     ext = "qs",
                                     var_name = "prec",
                                     stat = "identity",
                                     out_dir = here(glue::glue("output/{ext}/basin-avgs/{sp_average}"))
) {
  # nmme -------------------------------------------------------------------------
  nmme_basin_data <- tidy_nmme_basin_data(
    .var_name = var_name,
    .sp_average = sp_average,
    .ext = ext,
    .stat = stat,
    .out_dir = out_dir
  )
  gc()
  # cru obs ----------------------------------------------------------------
  cru_basin_data <- tidy_cru_basin_data(
    .var_name = var_name,
    .sp_average = sp_average,
    .ext = ext,
    .out_dir = out_dir
  )
  
  # join ----------------------------------------------------------------
  nmme_basin_data <- nmme_basin_data %>%
    mutate(
      data = map(
        data,
        ~ .x %>%
          dplyr::inner_join(
            cru_basin_data,
            by = c("date", "codONS")
          )
      )
    )
  gc()
  #nmme_basin_data[["data"]][[8]]
  
  out_file <- here(
    out_dir,
    glue::glue("nmme-cru-mly-{sp_average}-avg-basins-ons.{ext}")
    )
  export_bin_file(nmme_basin_data, out_file)
  
  checkmate::assert_file_exists(out_file)
  message("join data saved at: ", out_file)
  nmme_basin_data
  #summary(nmme_basin_data[["data"]][[1]])
}