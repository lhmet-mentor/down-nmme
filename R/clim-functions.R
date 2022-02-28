
# Funcao para calculo da climatologia dos modelos por lead time, bacia e mes ----
clim_stats <- function(data_models = nmme_cru_basin_data,
                       var_name = "prec",
                       funs_l = list(
                         avg = mean,
                         med = median,
                         sd = sd,
                         mad = mad
                       )) {
  clim_data <- dplyr::mutate(data_models,
    data = map(
      data,
      ~ .x %>%
        dplyr::select(-S) %>%
        dplyr::mutate(L = factor(trunc(L))) %>%
        dplyr::group_by(codONS, L, month = month(date)) %>%
        dplyr::summarise(
          dplyr::across(
            dplyr::contains(var_name),
            funs_l
          ),
          .groups = "drop"
        )
    )
  )
  clim_data
}



climatology_nmme_cru <- function(var_name = "prec",
                                 avg_type = "weighted",
                                 extension = "qs",
                                 funs_list = list(
                                   avg = mean,
                                   med = median,
                                   sd = sd,
                                   mad = mad
                                 )) {
  in_dir <- here(glue::glue("output/{extension}/basin-avgs/{avg_type}"))
  out_dir <- here(glue::glue("output/{extension}/basin-avgs/{avg_type}/climatology"))
  input_file <- here(in_dir, 
                     glue::glue("nmme-cru-mly-{avg_type}-avg-basins-ons.{extension}")
                     )
  out_clim_file <- here(
    glue::glue(out_dir, "/nmme_cru_basin_clim.{extension}")
  )
  # se arquivo existir retornar dados lidos
  if (checkmate::test_file_exists(out_clim_file)) {
    return(import_bin_file(out_clim_file))
  }
  # se nao existir arquivo criar diretorio para salvar climatologia
  if (!checkmate::test_directory_exists(out_dir)) {
    fs::dir_create(out_dir)
  }
  # read data
  nmme_cru_basin_data <- import_bin_file(input_file) %>%
    dplyr::rename("n_L" = L)

  # climatology statistics
  nmme_cru_basin_clim <- clim_stats(nmme_cru_basin_data, 
                                    var_name,
                                    funs_l = funs_list
                                    )
  # desaninhar dados
  nmme_cru_basin_clim <- nmme_cru_basin_clim %>%
    select(model, data) %>%
    unnest(cols = c(data)) %>%
    ungroup()

  # write data
  export_bin_file(nmme_cru_basin_clim, out_clim_file)

  message(
    fs::path_file(out_clim_file),
    " saved in \n",
    fs::path_dir(out_clim_file)
  )

  nmme_cru_basin_clim
}
