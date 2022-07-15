## Funções para o processamento dos arquivos NetCDF
## 
.pick_var_name <- function(name){
  ifelse(
    stringr::str_detect(name, "tmax"),
    "tmax",
    ifelse(stringr::str_detect(name,"tmin"),
           "tmin",
           "prec"
    )    
  )
}

.pick_type <- function(type){
  ifelse(
    stringr::str_detect(type, "HINDCAST"),
    "HINDCAST",
    ifelse(stringr::str_detect(type, "FORECAST"),
           "FORECAST",
           "MONTHLY"
    )    
  )
}
#----------------------------------------------------------------------------
# Pré-processamento adicional. Os dados que baixei do gdrive são zipados.
# foram salvos no output.

extract_zip <- function(ifile, ex_dir) {
  # ifile = zips[1]
  cat(ifile, "\n", dest_dir, "\n")
  unzip(ifile, exdir = dest_dir, jun) #%>% select(Name) %>% pull()
}

unzip_ncs <- function(vname = "prec", ex_dir = "output"){
  zips <- fs::dir_ls(ex_dir, 
                     regexp = stringr::str_replace("vname.*zip$", "vname", vname)
  )  
  
  dest_dir <- fs::dir_create(here(ex_dir, vname))
  
  # a função map é para fazer looping, descompactando cada zip
  #purrr::map(zips, extract_zip)
  purrr::walk(zips, extract_zip) 
  
}



#' Extrai o nome do modelo NMME a partir do nome do arquivo nc
#' @note Não testada para outros nomes de variáveis
#' @examples 
#' sample_files <- nc_files[sample(1:length(nc_files), 10)]
#' model_name(sample_files, "prec")
model_name <- function(nc_files, vname = .pick_var_name(nc_files),
                       type = .pick_type(nc_files)){
  fs::path_file(nc_files) %>%
    stringr::str_replace_all(pattern = glue::glue("nmme_{vname}_"), "") %>%
    stringr::str_replace_all(pattern = glue::glue("_{type}"), "") %>%
    stringr::str_replace_all(pattern = "_[0-9]{4}\\.nc", "")
}

# Obtem ano do arquivo nc a partir do nome do arquivo
year_from_ncfile <- function(nc_files, model = model_name(nc_files)){
 fs::path_file(nc_files) %>%
 stringr::str_replace_all(pattern = glue::glue("{model}"), "") %>%
    stringr::str_extract_all("[0-9]{4}") %>%
 unlist() %>%
 as.integer()
}


# numero de membros e tempos de antecedencia dos modelo ------------------------
.n_dim_nc <- function(nc_files, dim_name = "M", vname = .pick_var_name(nc_files)){
  #nc_file = nc_files[1]; dim_name = c("L", "M")
  dim_name <- toupper(dim_name)
  checkmate::assert_subset(dim_name, c("M", "L", "S", "X", "Y"))
  dir <- paste0(here::here(), glue::glue("/output/{vname}"))
  setwd(dir) 
  nc_info <- metR::GlanceNetCDF(nc_files)
  dim_info <- purrr::map_df(nc_info$dims, function(x) x$len)
  setwd(here::here())
  dim_info[dim_name]
}

#.n_dim_nc(nc_file, c("M", "L", "S", "X", "Y"))
#.n_dim_nc(nc_file, c("L"))


# sorteia arquivo NetCDF para os modelos ---------------------------------------
.sample_model_nc_file <- function(.nc_files, 
                                  .model = model_name(.nc_files), 
                                  .type = .pick_type(.nc_files),
                                  .vname = unique(.pick_var_name(.nc_files)), 
                                  .n = 1
                                         ){
  # .nc_files = nc_files; .model = model_counts$modelo; .n = 1; .vname = "tmax"; .type = "HINDCAST"
  .nc_files_type_var <- stringr::str_subset(.nc_files, pattern = .model) %>%
    stringr::str_subset(pattern = .type) %>%
    stringr::str_subset(pattern = .vname)
  
  model_names_nmme <- model_name(.nc_files_type_var, vname = .vname) %>% unique()

  checkmate::assert_subset(.model, model_names_nmme)
  
  model_regex <- ifelse(length(.model) > 1, paste(.model, collapse = "|"), .model)
    
  files_samp <- grep(model_regex, .nc_files_type_var, value = TRUE) %>%
    unique() %>%

    split(., model_name(., vname = .vname)) %>%
    map(., ~.x %>% sample(., size = .n)) %>%

    unlist()
    
  files_samp
}




# Contagem de arquivos por modelo, ano e tipo ----------------------------------------
nc_files_by_model_year <- function(nc_files, 
                                   out_ext = c("RDS", "qs"), 
                                   vname =.pick_var_name(nc_files)){
  
  # nc_files <- fs::dir_ls(here::here("output/ncdf"), type = "file", glob = "*.nc"); out_ext = "qs"; vname = "prec"
  
  # periodos
  model_counts <- tibble::tibble(file = nc_files, 
                         modelo = model_name(nc_files, vname = vname),
                         ano = year_from_ncfile(nc_files),
                         tipo = .pick_type(nc_files)
  ) %>%
    dplyr::group_by(modelo, tipo) %>%
    dplyr::summarise(start = min(ano), end = max(ano), freq = n()) %>%
    dplyr::mutate(
      check_span = end-start+1
    ) 

  # dimensoes
  files_samp <- .sample_model_nc_file(
    nc_files,
    model_counts$modelo, 
    .vname = vname,
    .type = .pick_type(nc_files),
    .n = 1
  )
  
  model_dimensions <- map_dfr(files_samp, 
          .n_dim_nc, 
          dim_name = c("M", "L", "S", "X", "Y"), 
          .id = "modelo"
          )
  
  models_info <- dplyr::full_join(model_counts, model_dimensions)
  
  out_file <- glue::glue("output/{out_ext}/model_counts.{out_ext}")
  export_bin_file(
    models_info, 
    here(out_file)
  )
  message("data exported to: ", out_file)
  models_info
}

#' Importa os dados de um arquivo netCDF e filtra para o lead time
#'
#' @param nc_file character escalar com caminho do arquivo netCDF
#' @param lead.time escalar numérico 


filter_lead_time <- function(nc_file, lead_time = 0.5, 
                             var_name = .pick_var_name(nc_file),
                             type = .pick_type(nc_file)){
  # nc_file = nc_files[1]; lead_time = 0.5; var_name = "tmax"; type = "FORECAST"
  
  # dados em formato tidy
  dir <- paste0(here::here(), glue::glue("/output/{var_name}"))
  setwd(dir) 
  prec_year_mod <- metR::ReadNetCDF(nc_file)
  prec_year_mod <- prec_year_mod[L == lead_time]
  #prec_year_mod <- PCICt::as.PCICt(prec_year_mod[L == lead_time], cal = "360_day")
  prec_year_mod <- prec_year_mod[, model := unique(model_name(nc_file, var_name, type))]
  prec_year_mod <- prec_year_mod[, year := year_from_ncfile(nc_file)]
  setwd(here::here())
  prec_year_mod
  #função não está retornando algo
}
#data_nc  <- filter_lead_time(model_files[1], lead_time = 0.5, var_name = "prec")




# Estou usando funções do pacote data.table
# não foi ensinado no curso de adar, é um pacote para manipulação
# mais eficiente e rápida de big data no R.
# Para entender as funções consulte o cartão 
# https://trello.com/c/DWnuJWnT


.save_qs <- function(.DT, .dt_file_rds, .overwrite = FALSE){
  #.DT = DT; .dt_file = dt_file
  .dt_file <- stringr::str_replace(.dt_file_rds, "RDS", "qs")
  
  if(!checkmate::test_file_exists(.dt_file)){
    #tictoc::tic()
    qs::qsave(.DT, .dt_file) # 133 MB, 5 sec elapsed
    #tictoc::toc()
    message("data were save at ", "\n", .dt_file)
    return(.dt_file)
  }
  
  # arquivo existe
  
  if(!.overwrite){
    return(.dt_file)
  } 
  
    qs::qsave(.DT, .dt_file) # 133 MB, 5 sec elapsed
    #tictoc::toc()
    message("data were save at ", "\n", .dt_file)
    .dt_file

}


.save_rds <- function(.DT, .dt_file, .overwrite = FALSE){
  #.DT = DT; .dt_file = dt_file
  
  if(!checkmate::test_file_exists(.dt_file)){
    #tictoc::tic()
    readr::write_rds(.DT, .dt_file)
    #tictoc::toc()
    message("data were save at ", "\n", .dt_file)
    return(.dt_file)
  }
  
  # arquivo existe
  
  if(!.overwrite){
    return(.dt_file)
  } 
  
  readr::write_rds(.DT, .dt_file)
  #tictoc::toc()
  message("data were save at ", "\n", .dt_file)
  .dt_file
  
}



#' Extrai os dados de todos arquivos netcdf de um modelo, para 1 lead time.
#'
#' @param nc_model_files vetor do tipo character com nomes dos arquivos netCDF.
#' @param var_name nome da variável ("prec", "tmax", "tmin").
#' @param lead_time tempo de antecedência de interesse (de 0.5 a 11.5)
#' @param dest_dir diretório de destino para o arquivo RDS. Pré definido como
#' `output/rds`. Se for NULL os dados não são exportados para um arquivo RDS.
#' @param use_qs logical, TRUE for quickly saving and reading objects to and 
#' from disk with qs package.
#' @return data.table
#' @details 
#' @export
#'
#' @examples
#' # lê e salva os dados em um RDS
#' model_data_lt15 <- data_model_lt(
#'   model_files, lead_time = 11.5, var_name = "prec"
#' )
#' # apenas lê os dados
#' model_data_lt05 <- data_model_lt(
#'      model_files, lead_time = 0.5, var_name = "prec", dest_dir = NULL
#' )
#' 
data_model_lt <- function(
  nc_model_files,
  lead_time, 
  var_name, 
  dest_dir = here("output", "rds"),
  use_qs = TRUE,
  overwrite = FALSE
) {
  # nc_model_files <- nc_files; lead_time = 0.5; dest_dir = here("output", "rds"); use_qs = TRUE; var_name = "prec"
  
  tictoc::tic()
  DT <- data.table::rbindlist(
    lapply(
      nc_model_files,
      function(file_nc) {
        cat(file_nc, "\n")
        filter_lead_time(file_nc, lead_time, var_name)
      }
    )
  )
  tictoc::toc()
  
  if(!is.null(dest_dir)){
    if(!dir_exists(dest_dir)) fs::dir_create(dest_dir)
    model_id <- DT[["model"]][1]
    dt_file <- glue::glue("nmme_{var_name}_{model_id}_lt{lead_time}.RDS")
    dt_file <- fs::path(dest_dir, dt_file)
    
    if(use_qs){
      return(.save_qs(DT, dt_file, overwrite))
    }
    
    return(.save_rds(DT, dt_file, overwrite))
  }
  
  DT
}

# read text with qs-------------------------------------------------------------
# gc()
# microbenchmark::microbenchmark(
#   base = read_rds(dt_file),  
#   qs = qs::qread(stringr::str_replace(dt_file, "RDS", "qs")), 
#   times = 1
# )
# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# base 26.06404 26.06404 26.06404 26.06404 26.06404 26.06404     1
# qs  4.83701  4.83701  4.83701  4.83701  4.83701  4.83701     1



#--------------------------------
#' Processa ncs de um modelo para todos lead times
#'
#' @param model nome do modelo NMME
#' @param variavel nome da variável a ser processada 
#' @param input_d diretório com os arquivos NetCDF
#' @param output_d diretório de saída para os arquivos RDS com os dados 
#' @param qs lógico, TRUE para salvar e ler rapidamenteobjetos para e a 
#' partir do disco com o pacote `qs`.
#' @inheritParams data_model_lt
#' processados
#'
#' @return
#' @export
#'
#' @examples
proc_ncs_by_lt <- function(model = model_nms[1], 
                           lead_time = seq(0.5, 11.5, by = 1),
                           variavel = "prec",
                           input_d = here("output", "prec"),
                           output_d = here("output", "rds"),
                           qs = TRUE,
                           overwrite = FALSE){
  
  nc_files <- fs::dir_ls(path = input_d, 
                         regexp = paste0(model, ".*\\.nc"),
  )
  
  map_chr(lead_time, 
          ~.x %>% 
            data_model_lt(nc_model_files = nc_files, 
                          lead_time = ., 
                          var_name = variavel,
                          dest_dir = output_d,
                          use_qs = qs, 
                          overwrite 
            )
  )
}





