
#download NMME

download_file_safe <- purrr::safely(download.file)

.pick_varname <- function(.model, .variable){
  var_name_down <- names_vars_models() %>%
    dplyr::filter(model == .model) %>%
    dplyr::select(all_of(.variable)) %>%
    dplyr::pull()  
  var_name_down
}


#' Baixa o arquivo anual da previsao retrospectiva do NMME para a AS
#' Download da previsao retrospectiva do Sistema NMME recortado para 
#' a America do Sul, incluindo os 12 tempos de antecedencia e os 10
#' tempos de inicio da previsao.  
#'
#' @param ano 
#' @param modelo 
#' @param variavel 
#'
#' @return
#' @export
#'
#' @examples
down_nmme_by_ymv <- function(year = "1980", 
                             model = "CanCM4i", 
                             variable = c("prec", "tmax", "tmin"),
                             type = c("HINDCAST", "FORECAST", "MONTHLY"),
                             overwrite = FALSE
){
  
  # al <- as.list(args_l)
  # year = pluck(al, "year")[2];  model = pluck(al, "model")[2]; variable = pluck(al, "vname_ref")[2]; type = pluck(al, "type")[2]; overwrite = FALSE
  type <- toupper(type)
  # year = 2000;  model = "NASA-GEOSS2S"; variable = "tmax"; type = "HINDCAST"
  
  
  year <- as.character(year)
  
  variable_model <- .pick_varname(model, variable)
  
  type_link <- ifelse(type == "MONTHLY", "MONTHLY", paste0("{type}","/.MONTHLY")) %>%
    glue::glue(.)
  
  data_link <- paste0(
    "http://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/",
    # modelo
    ".{model}/",
    #tipo
    ".{type_link}/",
    # variavel
    ".{variable_model}/",
    # Forecast Start Time (forecast_reference_time)
    "S/%280000%201%20Jan%20{year}%29%280000%2030%20Dec%20{year}",
    # sub dominio
    "%29RANGEEDGES/X/%2830W%29%2885W%29RANGEEDGES/Y/%2860S%29%2815N%29RANGEEDGES/",
    # nome dop arq default
    "data.nc"
  )
  
  #out_dir <- here("output", variable)
  out_dir <- here("output", "ncdf")
  
  
  if(as.logical(!fs::dir_exists(out_dir))){
    fs::dir_create(out_dir)
  }
  
  prefix <- paste0("nmme_",  variable, "_", model, "_", year) 
  file <- stringr::str_replace(fs::path_file(data_link), "data", prefix)
  dest_file <- here::here(out_dir, file)
  
  # se nao sobrescreve arquivo previos
  if(!overwrite){
    # caso exista arquivo baixado previamente
    if(fs::file_exists(dest_file)){
      # retorna o arquivo existente e nao fará download
      return(dest_file)
    }
  }
  
  
  Sys.sleep(1)
  
  data_link_year <- glue::glue(data_link)
  
  #path_file(data_link_year)
  message("Baixando arquivo: ", "\n", fs::path_rel(dest_file))
  
  res <- download_file_safe(data_link_year, destfile = dest_file, mode = "wb")
  

  if(!is.null(res$error)){ # arquivo para ano nao encontrado
    warning("Nao foi possivel baixar o arquivo: \n", data_link_year)
    return(NULL)
  }   
  
  # se deu tudo certo ...
  checkmate::assert_file_exists(dest_file)
  dest_file
  
}



