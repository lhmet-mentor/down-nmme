
#download NMME

download_file_safe <- purrr::safely(download.file)



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
down_nmme <- function(ano = 1981, 
                      modelo = "CanCM4i", 
                      variavel = "prec", 
                      tipo = "hindcast"){
  tipo <- toupper(tipo)
  # ano = 1980;  modelo = "NASA-GEOSS2S"; variavel = "prec"
  # ano = 1992;  modelo = "GFDL-SPEAR"; variavel = "prec"
  
  ano <- as.character(ano)
  
  data_link <- paste0(
    "http://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/",
    # modelo
    ".{modelo}/.{tipo}/.MONTHLY/",
    # variavel
    ".{variavel}/",
    # Forecast Start Time (forecast_reference_time)
    "S/%280000%201%20Jan%20{ano}%29%280000%2030%20Dec%20{ano}",
    # sub dominio
    "%29RANGEEDGES/X/%2830W%29%2885W%29RANGEEDGES/Y/%2860S%29%2815N%29RANGEEDGES/",
    # nome dop arq default
    "data.nc"
  )
  
  prefix <- paste0("nmme_", variavel, "_", modelo, "_", ano) 
  file <- stringr::str_replace(fs::path_file(data_link), "data", prefix)
  #dest_file <- here::here("output", variavel, file)
  dest_file <- here::here("output", "ncdf", file) 
  
  Sys.sleep(1)
  

  # data_link_ano <- stringr::str_replace_all(data_link, "variavel", variavel)
  # data_link_ano <- stringr::str_replace_all(data_link_ano, "modelo", modelo)
  # data_link_ano <- stringr::str_replace_all(data_link_ano, "YYYY", ano)
  
  data_link_ano <- glue::glue(data_link)
  
  #path_file(data_link_ano)
  message("Baixando arquivo: ", dest_file)
  
  res <- download_file_safe(data_link_ano, destfile = dest_file, mode = "wb")
  
  
  
  if(!is.null(res$error)){ # arquivo para ano nao encontrado
    warning("Nao foi possivel baixar o arquivo: \n", data_link_ano)
    return(NULL)
  }   
  
  # se deu tudo certo ...
  if(file.exists(dest_file)){
    return(dest_file)
  }
  
}



