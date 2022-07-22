pcks <- c(
  "rvest", "httr", "xml2", "tidyverse",
  "janitor", "lubridate"
)
easypackages::libraries(pcks)


#-------------------------------------------------------------------------------
.clean_ftp_dirs <- function(.url) {
  
  #.url = urls[1]
  
  xml2::read_html(.url) %>%
    rvest::html_table(fill = TRUE, na.strings = c("", "-")) %>%
    purrr::flatten_df() %>%
    janitor::remove_empty(c("rows", "cols")) %>%
    janitor::clean_names() %>%
    dplyr::mutate(last_modified = lubridate::dmy_hm(last_modified)) %>%
    dplyr::filter(!is.na(last_modified)) %>%
    dplyr::rename(directory = "name") %>%
    dplyr::mutate(directory = gsub("\\/", "", directory),
                  model = basename(.url)
                  )
    
}

clean_ftp_dirs <- function(urls){
  
  # urls = url
  purrr::map_df(urls, function(x) .clean_ftp_dirs(.url = x))
}


#-------------------------------------------------------------------------------
nmme_oper_from_cpc <- function(updated_models_only = TRUE, ensmean = FALSE) {
  # verificação dos dados operacionais do NMME
  url <- "https://ftp.cpc.ncep.noaa.gov/NMME/realtime_anom/"

  models_op <- clean_ftp_dirs(url)


  # os modelos com previsões atualizadas com IC = mes corrente
  cur_month <- lubridate::today() %>%
    lubridate::month()

  if (updated_models_only) {
    models_op <- models_op %>%
      dplyr::filter(
        lubridate::month(last_modified) %in% c(cur_month, cur_month - 1)
      )
  }

  if (!ensmean) {
    models_op <- dplyr::filter(models_op, directory != "ENSMEAN")
  }
  models_op
}


#-------------------------------------------------------------------------------
#' Get links to download NMME real time forecasts
#' 
#'
#' @param model one of the following names or a combination of them: "CFSv2", 
#' "CanCM4i", "GEM5_NEMO", "GFDL_SPEAR", "NASA_GEOS5v2", "NCAR_CCSM4".
#' @param yyyymm character with year (4 digits) and month (2 digits), 
#' e.g. "202007".
#'
#' @return character with the links to download the files for the requested
#'  months.
#' @export
#'
#' @examples
#' models <- nmme_oper_from_cpc()$directory[2]
#' cpc_links_from_model_year_month(models, c("202111", "202206"))
#' 
cpc_links_from_model_year_month <- function(
                                            model = tab_models$directory[3:4],
                                            yyyymm = "202207",
                                            quiet = FALSE) {
  
  #checkmate::assert_subset(model, nmme_oper_from_cpc()$directory)
  checkmate::assert_subset(model, c("CFSv2", "CanCM4i", "GEM5_NEMO",
                                    "GFDL_SPEAR", "NASA_GEOS5v2", 
                                    "NCAR_CCSM4"))
  
  
  # month year to date
  i_ymd <- lubridate::ym(as.character(yyyymm))

  url <- glue::glue("https://ftp.cpc.ncep.noaa.gov/NMME/realtime_anom/{model}/")

  # diretorios de datas disponiveis com prevs operacionais
  model_dates_dirs <- clean_ftp_dirs(url) %>%
    dplyr::mutate(
      date_directory = lubridate::ymd_h(directory),
      date_directory = lubridate::as_date(date_directory)
    ) %>%
    dplyr::arrange(desc(date_directory))

  # dir for the target year-month
  link_files <- dplyr::filter(
    model_dates_dirs,
    lubridate::year(date_directory) %in% lubridate::year(i_ymd),
    lubridate::month(date_directory) %in% lubridate::month(i_ymd)
  ) %>%
    dplyr::pull(directory) %>%
    paste0(url, ., sep = "/")

  # files in the the target year-month for the variables of interest
  tab_files <- clean_ftp_dirs(link_files) %>%
    dplyr::rename("file" = "directory") %>%
    dplyr::filter(stringr::str_detect(file, "prate|tmp2m"))

  if (!quiet) print(tab_files)

  paste0(url, tab_files$file)
}






# obter inicio e fim das FORECASTS e HINDCASTS

#http://leg.ufpr.br/~walmes/ensino/web-scraping/tutoriais/R-xml.html




  
  
  




#forecast_start_times(link)
  
  

# l <- doc %>%
#   xml_find_all(xpath = "/html/body/form/input[10]") %>% 
#   xml_attrs() %>%
#   unlist()
# l[c("name", "data-default")]


# # titulo
# title <- xml_find_all(doc, xpath = '//*[@id="info"]/h2')
# class(title)
# methods(class = "xml_nodeset")
# xml_text(title)
# 
# subtitle
# 
# ns <- xml_find_all(doc, xpath = '//*[@id="info"]')
# class(ns)
# as_list(ns)
# 
# class(ns)
# methods(class = "xml_node")
# 
# 
# iri_info_model <-  map_dfr(xml_attrs(ns), 
#       function(x) {
#         # x <- xml_attrs(ns)[[6]]
#         sel <- names(x)  %in% c("name", "data-default")
#         if(sum(sel) > 1) return(x[sel])
#         NULL
#       }
#   ) 
  




