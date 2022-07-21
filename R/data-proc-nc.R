## Funções para o processamento dos arquivos NetCDF
##
.pick_var_name <- function(name) {
  ifelse(
    stringr::str_detect(name, "tmax"),
    "tmax",
    ifelse(stringr::str_detect(name, "tmin"),
      "tmin",
      "prec"
    )
  )
}

.pick_type <- function(type) {
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
  unzip(ifile, exdir = dest_dir, jun) # %>% select(Name) %>% pull()
}

unzip_ncs <- function(vname = "prec", ex_dir = "output") {
  zips <- fs::dir_ls(ex_dir,
    regexp = stringr::str_replace("vname.*zip$", "vname", vname)
  )

  dest_dir <- fs::dir_create(here(ex_dir, vname))

  # a função map é para fazer looping, descompactando cada zip
  # purrr::map(zips, extract_zip)
  purrr::walk(zips, extract_zip)
}



#' Extrai o nome do modelo NMME a partir do nome do arquivo nc
#' @note Não testada para outros nomes de variáveis
#' @examples
#' sample_files <- nc_files[sample(1:length(nc_files), 10)]
#' model_name(sample_files, "prec")
model_name <- function(nc_files, vname = .pick_var_name(nc_files),
                       type = .pick_type(nc_files)) {
  fs::path_file(nc_files) %>%
    stringr::str_replace_all(pattern = glue::glue("nmme_{vname}_"), "") %>%
    stringr::str_replace_all(pattern = glue::glue("_{type}"), "") %>%
    stringr::str_replace_all(pattern = "_[0-9]{4}\\.nc", "")
}

# Obtem ano do arquivo nc a partir do nome do arquivo
year_from_ncfile <- function(nc_files, model = model_name(nc_files)) {
  fs::path_file(nc_files) %>%
    stringr::str_replace_all(pattern = glue::glue("{model}"), "") %>%
    stringr::str_extract_all("[0-9]{4}") %>%
    unlist() %>%
    as.integer()
}


# numero de membros e tempos de antecedencia dos modelo ------------------------
.n_dim_nc <- function(nc_file, dim_name = "M", vname = .pick_var_name(nc_file)) {
  # nc_file = nc_files[1]; dim_name = c("M", "L", "S", "X", "Y")
  dim_name <- toupper(dim_name)
  checkmate::assert_subset(dim_name, c("M", "L", "S", "X", "Y"))
  # dir <- paste0(here::here(), glue::glue("/output/ncdf/{vname}"))
  # setwd(dir)
  nc_info <- metR::GlanceNetCDF(nc_file)
  dim_info <- purrr::map_df(nc_info$dims, function(x) x$len)
  # setwd(here::here())
  dim_info[dim_name]
}


# sorteia arquivo NetCDF para os modelos ---------------------------------------
# .sample_model_nc_file <- function(.nc_files,
#                                   .model = unique(model_name(.nc_files)),
#                                   .type = unique(.pick_type(.nc_files)),
#                                   .vname = unique(.pick_var_name(.nc_files)),
#                                   .n = 1
#                                          ){
#   # .nc_files = nc_files; .model = unique(model_counts$modelo); .n = 1; .vname = "prec"; .type = c("HINDCAST", "FORECAST", "MONTHLY")
#
#   .model_pattern <- ifelse(length(.model) > 1,
#                            paste(.model, collapse = "|"),
#                            .model
#                            )
#   .type <- ifelse(length(.type) > 1,
#                   paste(.type, collapse = "|"),
#                   .type
#   )
#
#   .vname <- ifelse(length(.vname) > 1,
#                     paste(.vname, collapse = "|"),
#                     .vname
#   )
#
#   .nc_files_type_var <- str_subset(nc_files, .model_pattern) %>%
#      stringr::str_subset(pattern = .type) %>%
#      stringr::str_subset(pattern = .vname)
#
#   #model_names_nmme <- model_name(.nc_files_type_var, vname = .vname) %>% unique()
#
#   #checkmate::assert_subset(.model, model_names_nmme)
#
#   .nc_files_type_var <- .nc_files_type_var %>%
#     unique()
#
#   files_samp <- tibble(
#                    model = model_name(.nc_files_type_var, vname = .vname),
#                   type = .pick_type(.nc_files_type_var),
#                   file = .nc_files_type_var,
#
#                  ) %>%
#     dplyr::group_by(model, type) %>%
#     dplyr::summarise(file = sample(file, size = 1), .groups = "drop")
#
#   files_samp
# }



#' Check integrity of NetCDF files in terms of number of members and 
#' initialization times
#' 
#' @param files path to files (optional)
#' @param metadata output [tibble][tibble::tibble-package] from `nc_metadata()`.
#'
#' @return a [tibble][tibble::tibble-package] with one more variable called 
#' `nc_integrity`. Its a logical variable with FALSE indicating that NetCDF
#' file does not have the expected number of members or forecast times for a
#' annual file.
#' @export
#'
#' @examples
check_nc_integrity <- function(files, metadata){
  # files = nc_files
  checkmate::assert_true(!(missing(files) && missing(metadata)))
  
  if(missing(metadata) &&  !missing(files)){
    metadata <- nc_metadata(files)  
  }
  
  # output to files that do not match expected M or S (12)
  # default is TRUE
  metadata <- dplyr::mutate(metadata, nc_integrity = TRUE)
  
  mtdt_ref <- metadata_ref(model_string = metadata[["model"]][1])
  files_incomplete <- anti_join(metadata, mtdt_ref, by = c("M", "S"))
  
  if(nrow(files_incomplete) > 0) {
    metadata <- dplyr::mutate(
      metadata, 
      nc_integrity = !(file %in% files_incomplete[["file"]])
    )
  }
  metadata
}



#' Metadata from NetCDF files
#'
#' @param nc_files character, path to annual NetCDF files of same variable.
#'
#' @return a [tibble][tibble::tibble-package] with variables: `model`, `year`,
#' `file`, `M` (number of members), `L` (lead time), 
#' `S` (number of forecasts start time), `X` (number of longitudes) and 
#' `Y`(number of latitudes).
#' @export
#'
#' @examples
#' if(FALSE){
#'  nc_dir = here::here("output", "ncdf")
#'  model = "CanCM4i" 
#'  nc_files <- fs::dir_ls(
#'    path = nc_dir, 
#'    glob = glue::glue("*{var_name}_{model}*.nc"),
#'     recurse = TRUE
#'  )
#'  nc_metadata(nc_files)
#' }
nc_metadata <- function(nc_files) {

  # vname = var_name = "prec"; nc_dir = here::here("output", "ncdf"); model = "CanCM4i"; nc_files <- fs::dir_ls(path = nc_dir, glob = glue::glue("*{var_name}_{model}*.nc"), recurse = TRUE)

  # check files for one model only
  model_nm <- model_name(nc_files) %>% unique() 
  nmodels <- length(model_nm)
  checkmate::assert_set_equal(nmodels, 1)
  
  
  # informacoes baseadas no nome do arquivo
  info_files <- tibble::tibble(
    file = fs::path_rel(nc_files),
    model = model_nm,
    year = year_from_ncfile(nc_files),
    type = .pick_type(nc_files)
  ) %>%
    dplyr::relocate(model:type)


  # metadados do netcdf
  metadata <- map_dfr(info_files[["file"]],
    .n_dim_nc,
    dim_name = c("M", "L", "S", "X", "Y")
  )

  metadata_ncs <- dplyr::bind_cols(info_files, metadata)
  
  metadata_ncs

}


#' Expected metadata from NetCDF file of a NMME model for HINDCAST and FORECAST
#'
#' Give the expected number of ensemble members and the number of forecasts 
#' start time for HINDCAST and FORECAST files.
#' @param model_string model name
#'
#' @return a [tibble][tibble::tibble-package] with variables: `model`, `type`,
#' `M` (number of ensemble members), and `S` (number of forecasts start time).
#' @export
#'
#' @examples
metadata_ref <- function(model_string = "CanCM4i"){
  
  mtdt_ref <-  type_period_models() %>%
    dplyr::filter(model == model_string) %>%
    dplyr::select(-(start:end)) %>%
    dplyr::rename("M" = "nmembers") %>%
    # numero de datas de inicializacao esperadas em cada arquivo anual
    dplyr::mutate(S = 12)
  mtdt_ref
  
}



#' Metadata from NetCDF files
#'
#' @param model_files chatacter with paths to NetcDF files from a model. The 
#' function can deal with hidcast and forecast files. 
#'
#' @return a [tibble][tibble::tibble-package] with variables: `model`, `year`,
#'  `type` (HINDCAST or FORECAST), `file`, `M` (number of members), 
#'  `L` (lead time), `S` (number of forecasts start time), 
#'  `X` (number of longitudes), `Y`(number of latitudes), and 
#'  `nc_integrity` (logical).
#'  
#' @export
#'
#' @examples
#' if(FALSE){
#'  nc_dir <- here::here("output", "ncdf")
#'  model <- "CanCM4i" 
#'  nc_files <- fs::dir_ls(
#'    path = nc_dir, 
#'    glob = glue::glue("*{var_name}_{model}*.nc"),
#'     recurse = TRUE
#'  )
#'  nmme_md <- nmme_metadata(model_files = nc_files)
#'  nmme_md
#'  nmme_md %>%
#'    dplyr::filter(year %in% year[nc_integrity])
#'  nmme_md_smry <- nmme_metadata(model_files = nc_files, summary = TRUE)
#'  nmme_md_smry
#' }
#' 
nmme_metadata <- function(model_files, summary = FALSE){
  # model_files = nc_files; summary = FALSE
  mtdt <- nc_metadata(nc_files = model_files)
  mtdt <- check_nc_integrity(metadata = mtdt)
  
  if(!summary) return(mtdt)
  
  # most likely values
  mtdt_summary <- mtdt %>%
    group_by(model, type) %>%
    summarise(across(M:Y, modes), 
              start = min(year),
              end = max(year),
              freq = n(),
              .groups = "drop"
    )%>%
    dplyr::mutate(
      check_span = end-start+1,
      nc_integrity = TRUE
    )
  
  mtdt_summary
  
}

 

#' Importa os dados de um arquivo netCDF e filtra para o lead time
#'
#' @param nc_file character escalar com caminho do arquivo netCDF
#' @param lead.time escalar numérico


filter_lead_time <- function(nc_file, lead_time = 0.5,
                             var_name = .pick_var_name(nc_file),
                             type = .pick_type(nc_file)) {
  # nc_file = nc_files[1]; lead_time = 0.5; var_name = "tmax"; type = "FORECAST"

  # dados em formato tidy
  dir <- paste0(here::here(), glue::glue("/output/{var_name}"))
  setwd(dir)
  prec_year_mod <- metR::ReadNetCDF(nc_file)
  prec_year_mod <- prec_year_mod[L == lead_time]
  # prec_year_mod <- PCICt::as.PCICt(prec_year_mod[L == lead_time], cal = "360_day")
  prec_year_mod <- prec_year_mod[, model := unique(model_name(nc_file, var_name, type))]
  prec_year_mod <- prec_year_mod[, year := year_from_ncfile(nc_file)]
  setwd(here::here())
  prec_year_mod
  # função não está retornando algo
}
# data_nc  <- filter_lead_time(model_files[1], lead_time = 0.5, var_name = "prec")




# Estou usando funções do pacote data.table
# não foi ensinado no curso de adar, é um pacote para manipulação
# mais eficiente e rápida de big data no R.
# Para entender as funções consulte o cartão
# https://trello.com/c/DWnuJWnT


.save_qs <- function(.DT, .dt_file_rds, .overwrite = FALSE) {
  # .DT = DT; .dt_file = dt_file
  .dt_file <- stringr::str_replace(.dt_file_rds, "RDS", "qs")

  if (!checkmate::test_file_exists(.dt_file)) {
    # tictoc::tic()
    qs::qsave(.DT, .dt_file) # 133 MB, 5 sec elapsed
    # tictoc::toc()
    message("data were save at ", "\n", .dt_file)
    return(.dt_file)
  }

  # arquivo existe

  if (!.overwrite) {
    return(.dt_file)
  }

  qs::qsave(.DT, .dt_file) # 133 MB, 5 sec elapsed
  # tictoc::toc()
  message("data were save at ", "\n", .dt_file)
  .dt_file
}


.save_rds <- function(.DT, .dt_file, .overwrite = FALSE) {
  # .DT = DT; .dt_file = dt_file

  if (!checkmate::test_file_exists(.dt_file)) {
    # tictoc::tic()
    readr::write_rds(.DT, .dt_file)
    # tictoc::toc()
    message("data were save at ", "\n", .dt_file)
    return(.dt_file)
  }

  # arquivo existe

  if (!.overwrite) {
    return(.dt_file)
  }

  readr::write_rds(.DT, .dt_file)
  # tictoc::toc()
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
#'   model_files,
#'   lead_time = 11.5, var_name = "prec"
#' )
#' # apenas lê os dados
#' model_data_lt05 <- data_model_lt(
#'   model_files,
#'   lead_time = 0.5, var_name = "prec", dest_dir = NULL
#' )
data_model_lt <- function(
                          nc_model_files,
                          lead_time,
                          var_name,
                          dest_dir = here("output", "rds"),
                          use_qs = TRUE,
                          overwrite = FALSE) {
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

  if (!is.null(dest_dir)) {
    if (!dir_exists(dest_dir)) fs::dir_create(dest_dir)
    model_id <- DT[["model"]][1]
    dt_file <- glue::glue("nmme_{var_name}_{model_id}_lt{lead_time}.RDS")
    dt_file <- fs::path(dest_dir, dt_file)

    if (use_qs) {
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
                           input_d = here("output", "ncdf"),
                           output_d = here("output", "qs"),
                           qs = TRUE,
                           overwrite = FALSE) {
  nc_files <- fs::dir_ls(
    path = input_d,
    regexp = paste0(model, ".*\\.nc"),
    recurse = TRUE
  )

  map_chr(
    lead_time,
    ~ .x %>%
      data_model_lt(
        nc_model_files = nc_files,
        lead_time = .,
        var_name = variavel,
        dest_dir = output_d,
        use_qs = qs,
        overwrite
      )
  )
}
