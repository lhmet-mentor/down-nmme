## aperfeicoamentos futuros
library(data.table)
pcks <- c("data.table", "dplyr", "tidyverse", "stringr")
easypackages::libraries(pcks)

# tabela com nome das variáveis em cada modelo
# informações obtidas manualmente no site ...


# Para obter forecast refrence times de  cada modelo e posteriormente adicionar 
# datas em names_vars_models().
# NAO USADO AINDA
forecast_start_times <- function(nmme_url){
  # nmme_url = link
  doc <- httr::GET(nmme_url) %>%
    xml2::read_html()
  
  info_model <- doc %>%
    xml2::xml_find_all(xpath = '//*[@id="info"]/div[3]/dl[1]') %>%
    xml2::xml_text() %>%
    textConnection() %>%
    readLines()
  
  # forecast start time (S)
  fst <- grep("forecast_reference_time", info_model, value = TRUE) %>%
    stringr::str_extract_all("(?<=\\().+?(?=\\))") %>%
    unlist() %>%
    lubridate::fast_strptime(format = "0000 %d %b %Y")
  fst <- lubridate::as_date(fst[!is.na(fst)])
  fst 
}



#' Table of variables of the NMME models
#'
#' @return a [tibble][tibble::tibble-package] with variables: `model`, `prec`,
#' `tmax`, `tmin` e `tref`.
#' @export
#'
#' @examples
#' if(FALSE) {
#'  names_vars_models()
#' }
names_vars_models <- function() {
  tibble::tribble(
    ~model, ~prec, ~tmax, ~tmin, ~tref,
    "NCEP-CFSv2", "prec", NA_character_, NA_character_, "tref",
    "CanCM4i", "prec", "tmax", "tmin", NA_character_,
    "GEM5-NEMO", "prec", "tmax", "tmin", NA_character_,
    "GFDL-SPEAR", "prec", "t_ref_max", "t_ref_min", NA_character_,
    "NASA-GEOSS2S", "prec", "t2mmax", "t2mmin", NA_character_,
    # "NASA-GMAO-062012",      "prec",  "t2mmax",      "t2mmin",
    "NCAR-CCSM4", "prec", NA_character_, NA_character_, "tref"
    # "CanSIPS-IC3",           "prec",  "tmax",        "tmin",
    # para evitar problemas em outras funcoes os nomes dos modelos
    # devem ser únicos
    # "CanSIPS-IC3-GEM5-NEMO", "prec",   "tmax",        "tmin",
    # "CanSIPSv2",             "prec",  "tmax",        "tmin",
    # "CMC1-CanCM3",           "prec",  "tmax",        "tmin",
    # "CMC2-CanCM4",           "prec",  "tmax",        "tmin",
    # "GEM-NEMO",              "prec",  "tmax",        "tmin",
    # "GFDL-CM2p1-aer04",      "prec",  "t_ref_max",   "t_ref_min",
    # "GFDL-CM2p5-FLOR-A06",   "prec",  "t_ref_max",   "t_ref_min",
    # "GFDL-CM2p5-FLOR-B01",   "prec",  "t_ref_max",   "t_ref_min",
    # "NCAR-CESM1",            "prec",  "tsmax",       "tsmin",
    # por nao haver tmx e tmin no cfsv2 deixamos NA por enquanto
  )
}

# criando uma tabela com os modelos e seus respectivos períodos e tipos
# informaçẽos obtidas manualmente no site
type_period_models <- function() {
  type_period_models <- tibble::tribble(
    ~model, ~type, ~start, ~end, ~nmembers,
    "NCEP-CFSv2", "HINDCAST", 1982, 2010, 24,
    "NCEP-CFSv2", "FORECAST", 2011, 2022, 32,

    "CanCM4i", "HINDCAST", 1981, 2018, 10,
    "CanCM4i", "FORECAST", 2016, 2021, 10,

    "GEM5-NEMO", "HINDCAST", 1980, 2020, 10,
    "GEM5-NEMO", "FORECAST", 2021, 2022, 10,

    "GFDL-SPEAR", "HINDCAST", 1991, 2020, 15,
    "GFDL-SPEAR", "FORECAST", 2020, 2022, 30,

    "NASA-GEOSS2S", "HINDCAST", 1981, 2017, 4,
    "NASA-GEOSS2S", "FORECAST", 2017, 2022, 10,

    # "NASA-GMAO-062012",   "MONTHLY",   1981,   2018,

    "NCAR-CCSM4", "MONTHLY", 1982, 2022, 10
    # "CanSIPS-IC3",        "FORECAST",  2021,   2022,
    # "CanSIPS-IC3",        "HINDCAST",  1980,   2020,
    # "GEM5-NEMO",          "FORECAST",   2021,   2022,
    # "CanSIPSv2",          "FORECAST",  2016,   2021,
    # "CanSIPSv2",          "HINDCAST",  1981,   2018,
    # "CMC1-CanCM3",        "FORECAST",  2011,   2019,
    # "CMC1-CanCM3",        "HINDCAST",  1981,   2010,
    # "CMC2-CanCM4",        "FORECAST",  2011,   2019,
    # "CMC2-CanCM4",        "HINDCAST",  1981,   2010,
    # "GEM-NEMO",           "FORECAST",  2016,   2021,
    # "GEM-NEMO",           "HINDCAST",  1981,   2018,
    # "GFDL-CM2p1-aer04",   "MONTHLY",   1982,   2021,
    # "GFDL-CM2p5-FLOR-A06","MONTHLY",   1980,   2021,
    # "GFDL-CM2p5-FLOR-B01","MONTHLY",   1980,   2021,
    # "NCAR-CESM1",         "FORECAST",  2016,   2017,
    # "NCAR-CESM1",         "HINDCAST",  1980,   2010
  )

  type_period_models <- tibble::as_tibble(type_period_models) %>%
    dplyr::mutate(across(.funs = as.character))
  type_period_models
}

# criando uma tabela com o modelo e seus respectivos tipos de previsao e anos
tab_mod_year_type <- function(models_period = type_period_models(),
                              priority_type = "HINDCAST",
                              quiet = TRUE) {
  tab_myt <- type_period_models() %>%
    tidyr::pivot_longer(start:end, names_to = "id", values_to = "year") %>%
    dplyr::group_by(model, type) %>%
    tidyr::expand(year = full_seq(year, 1)) %>%
    dplyr::arrange(model, year) %>%
    dplyr::ungroup()



  # anos duplicados
  dup_myt <- tab_myt %>%
    dplyr::group_by(model, year) %>%
    dplyr::tally() %>%
    dplyr::filter(n > 1) %>%
    dplyr::left_join(tab_myt, by = c("model", "year")) %>%
    dplyr::select(-n) %>%
    dplyr::ungroup()

  if (priority_type == "none") {
    if (!quiet) {
      warning("Anos com arquivos duplicados para hindcasts e forecasts.",
        print(knitr::kable(dup_myt)),
        call. = FALSE
      )
    }

    return(tab_myt)
  }

  # prioridade para HINDCAST
  tab_myt <- dup_myt %>%
    dplyr::filter(type != priority_type) %>%
    dplyr::anti_join(tab_myt, ., by = c("model", "type", "year"))

  ## check
  #  tab_myt %>% group_by(model, year) %>%  tally() %>% filter(n!=1) %>% nrow()
  # models_span_actual <- tab_myt %>%
  #   group_by(model, type) %>%
  #   summarise(start = min(year), end = max(year)) %>%
  #   ungroup()
  ## forecasts corrigidas
  # anti_join(models_period, models_span_actual)
  tab_myt
}

# criando uma tabela com as informacoes necessarias para realizacao do download
tab_mod_year_vname_type <- function(vname, priority = "none", .quiet = TRUE) {
  nmme_info <- dplyr::full_join(
    tab_mod_year_type(priority_type = priority, quiet = .quiet),
    names_vars_models(),
    by = "model"
  ) %>%
    tidyr::pivot_longer(
      cols = -(model:year),
      names_to = "vname_ref", # nomes padronizados de tmax e tmin p/ chamada de download
      values_to = "vname_real" # nomes de tmax e tmin usados em cada modelo
    ) %>%
    dplyr::filter(vname_real != is.na(vname_real)) %>%
    dplyr::select(model, year, type, vname_ref) %>%
    dplyr::arrange(model, year) %>%
    dplyr::relocate(year, model, vname_ref, type)

  if (!missing(vname)) {
    nmme_info <- dplyr::filter(nmme_info, vname_ref == vname)
  }
  nmme_info
}


#' Time span o NMME's models
#'
#' @param by_type logical, if TRUE return time span for HIND and FORECASTS.
#' @return [tibble][tibble::tibble-package] with `model`, `start` and `end` 
#' year of models dataset.
#' 
#' @export
#'
#' @examples
#' if (FALSE) {
#'   nmme_models_span()
#'   nmme_models_span(TRUE)
#' }
nmme_models_span <- function(by_type = FALSE,
                             priority_type = "none", 
                             quiet = TRUE) {
  models_span_actual <- tab_mod_year_vname_type(
    priority = priority_type,
    .quiet = quiet
    ) %>%
    group_by(model, type) %>%
    summarise(start = min(year), end = max(year), .groups = "drop")

  if (by_type) {
    return(models_span_actual)
  }

  models_span_actual <- models_span_actual %>%
    tidyr::pivot_longer(start:end, names_to = "id", values_to = "year") %>%
    dplyr::group_by(model) %>%
    dplyr::summarise(start = min(year), end = max(year), .groups = "drop")

  models_span_actual
}


plot_nmme_models_span <- function(data = tab_mod_year_vname_type("prec"),
                                  .plotly = FALSE) {
  p <- data %>%
    # mutate(value = 1) %>%
    ggplot2::ggplot(aes(x = year, y = model, fill = type)) +
    ggplot2::geom_tile(alpha = 0.7, width = 0.9, height = 0.9) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(minor_breaks = scales::pretty_breaks(n = 40))

  if (.plotly) p <- plotly::ggplotly(p)
  p
}
