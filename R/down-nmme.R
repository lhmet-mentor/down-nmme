
#download NMME

download_file_safe <- purrr::safely(download.file)


.commom_longest_string <- function(a, b){
  #  a = "tmax"; b = "tmin"
  ta <- drop(attr(adist(a, b, counts=TRUE), "trafos"))
  stringi::stri_sub(b, stringi::stri_locate_all_regex(ta, "M+")[[1]])  
}





.pick_varname <- function(.model, .variable, closest_match = FALSE){
  
  # .model = "NCEP-CFSv2"; .variable =  "tmax"; closest_match = TRUE
  
  var_name_down <- names_vars_models() %>%
    dplyr::filter(model == .model) %>%
    dplyr::select(all_of(.variable)) %>%
    dplyr::pull()  
  
  if(any(is.na(var_name_down))) {
    
    if(!closest_match) stop(
      glue::glue(
        'Variable `{.variable}` is not available for the model `{.model}`.'
        
      ), "\n",
      '   Avaiable choices are: ', paste(.pick_vars_from_model(.model), collapse = ", ")
    )
    
    
    commom_strings <- purrr::map_chr(.pick_vars_from_model(.model),
                              ~ .commom_longest_string(.variable,.x)
    )
    commom_strings <- commom_strings[!is.na(commom_strings)]
    commom_strings <- commom_strings[which.max(nchar(commom_strings))]
    
    var_name_down <- stringr::str_subset(.pick_vars_from_model(.model), commom_strings)
    
    warning(
      glue::glue(
      "Using {var_name_down} because {.variable} is not available for {.model}."
      )
    )
    
    return(var_name_down)
  }
  
  var_name_down
  
}

.pick_vars_from_model <- function(.model){
  # .model = names_vars_models()$model[1]
  vars_mod <- dplyr::filter(names_vars_models(), model == .model) %>%
    dplyr::select(-model)
  vars <- c(t(vars_mod))
  vars[!is.na(vars)]
}

.set_type <- function(type, model){
  type_link <- ifelse(type == "MONTHLY", "MONTHLY", 
                      paste0("{type}","/.MONTHLY")) %>%
    glue::glue(.)
  
  # Como o link do modelo NCEP-CFSv2 tipo FORECAST é diferente dos demais sobrescreve o type_link se necessário 
  type_link <- ifelse(model == "NCEP-CFSv2" & type == "FORECAST",
                      paste0("{type}", "/.EARLY_MONTH_SAMPLES/.MONTHLY"), type_link)%>%
    glue::glue(.)
  #Como o link do modelo   CanSIPS-IC3-GEM5-NEM0 é diferente dos demaais sobrescreve o type_link se necessário
  type_link <- ifelse(model == "GEM5-NEMO",
                      paste0("GEM5-NEMO/.", "{type}", "/.MONTHLY"), type_link)%>%
    glue::glue(.)
  
  type_link
}


.set_model <- function(model){
  
  # No link o nome do modelo "CanSIPS-IC3-GEM5-NEMO" é apenas CanSIPS-IC3
  model_link <- ifelse(model == "GEM5-NEMO", 
                       "CanSIPS-IC3", model)
  
  model_link <- ifelse(model == "NCAR-CCSM4", "COLA-RSMAS-CCSM4", model)
  model_link
  
}



nmme_link <- function(model = "CanCM4i", 
                      variable = c("prec", "tmax", "tmin", "tref"),
                      type = c("HINDCAST", "FORECAST", "MONTHLY"),
                      .closest_match = TRUE
                      
){
  
  # model = "NCEP-CFSv2"; variable = "prec"; type = "HINDCAST"
  
  type <- toupper(type)
  variable_model <- .pick_varname(model, variable, closest_match = .closest_match)
  type_link <- .set_type(type, model)
  model_link <- .set_model(model)
  
  nmme_url <- paste0(
    "http://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/",
    # modelo
    ".{model_link}/",
    #tipo
    ".{type_link}/",
    # variavel
    ".{variable_model}/"
  )
  nmme_url <- glue::glue(nmme_url)
  nmme_url
}

## TEST
# .pick_varname("NCEP-CFSv2", "tmin", closest_match = TRUE)
# .pick_varname("NCEP-CFSv2", "tmax", closest_match = TRUE)
# link <- nmme_link("NCEP-CFSv2", "tmax", "HINDCAST", .closest_match = TRUE)
# browseURL(link)


#' Baixa o arquivo anual da previsao retrospectiva do NMME para a AS
#' Download da previsao retrospectiva do Sistema NMME recortado para 
#' a America do Sul, incluindo os 12 tempos de antecedencia e os 10
#' tempos de inicio da previsao.  
#'
#' @param year ano 
#' @param modelo codigo do modelo NMME
#' @param variavel 'prec', 'tmax', 'tmin', ou 'tref'
#'
#' @return
#' @export
#'
#' @examples
#' down_nmme_by_ymv("2017", "NASA-GEOSS2S", "prec", "")
down_nmme_by_ymv <- function(year = "1980", 
                             model = "CanCM4i", 
                             variable = c("prec", "tmax", "tmin"),
                             type = c("HINDCAST", "FORECAST", "MONTHLY"),
                             overwrite = FALSE
){
  
  # al <- as.list(args_l)
  # year = pluck(al, "year")[2];  model = pluck(al, "model")[2]; variable = pluck(al, "vname_ref")[2]; type = pluck(al, "type")[2]; overwrite = FALSE
  type <- toupper(type)
  # year = 2000;  model = "NCEP-CFSv2"; variable = "prec"; type = "HINDCAST"
  year <- as.character(year)

  data_link <- paste0(
    nmme_link(model, variable, type),
    # Forecast Start Time (forecast_reference_time)
    "S/%280000%201%20Jan%20{year}%29%280000%2030%20Dec%20{year}",
    # sub dominio
    "%29RANGEEDGES/X/%2830W%29%2885W%29RANGEEDGES/Y/%2860S%29%2815N%29RANGEEDGES/",
    # nome dop arq default
    "data.nc"
  )
  
  #out_dir <- here("output", variable)
  out_dir <- here("output", "ncdf", model)
  
  if(as.logical(!fs::dir_exists(out_dir))){
    fs::dir_create(out_dir)
  }

  # nome do arquivo de destino `{modelo}/nmme_{variavel}_{modelo}_{type}_{ano}.nc`
  prefix <- paste0("nmme_", variable, "_", model, "_", type, "_", year) 
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



