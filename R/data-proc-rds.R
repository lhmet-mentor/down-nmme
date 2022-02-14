# Funções para processamento dos dados
# de reforecast dos modelos nmme pre-processados (convertidos de nc para rds)


import_bin_file <- function(file){
  ext <- fs::path_ext(file)
  if(ext == "qs") return(qs::qread(file))
  readr::read_rds(file)
}

export_bin_file <- function(data, file){
  ext <- fs::path_ext(file)
  if(ext == "qs") return(qs::qsave(data, file))
  readr::write_rds(data, file)
}


##------------------------------------------------------------------------------
# Funcao par extrai nome do modelo a partir do arquivo rds
model_name_rds <- function(file_rds, vname = "prec"){
  
  #file_rds = files[1]; vname = "prec"
  #file_rds
  ext <- fs::path_ext(file_rds)[1]
  
  nm <- fs::path_file(file_rds) %>%
    stringr::str_replace_all(pattern = glue::glue("nmme_{vname}_"), "") %>%
    stringr::str_replace_all(
      pattern = paste0("_lt[0-9]{1,2}\\.[0-9]{1}\\.", ext), ""
      ) %>%
    unique()
  
  assert_true(length(nm) == 1)
  nm
  
}


# Agregacao dos membros dos modelos (ensemble) para cada ponto-----------------
ensemble_members <- function(refcst_file, var_name = "prec", stat = "median") {
  # refcst_file <- files[1]; var_name = "prec"; stat = "mean"
  
  checkmate::assert_choice(stat, c("mean", "median", "identity"))
  
  refcst <- import_bin_file(refcst_file)
  
  # case median----------------------------------------------------------------
  if(stat == "median"){
    refcst <- refcst[,
                     .(# mediana e mad (medidas stats + robustas)
                       prec_ensmed = median(prec),
                       prec_ensmad = mad(prec)
                     ),
                     #keyby = .(S, L)
                     by = c("S", "L", "X", "Y")
    ]  
    refcst[, model := model_name_rds(refcst_file, var_name)]
    return(refcst)
  }
  # case mean------------------------------------------------------------------
  if(stat == "mean"){
    refcst <- refcst[,
                     .(prec_ensmean = mean(prec),
                       prec_enssd = sd(prec)#,
                       # mediana e mad (medidas stats + robustas)
                       #prec_ensmed = median(prec),
                       #prec_ensmad = mad(prec)
                     ),
                     #keyby = .(S, L)
                     by = c("S", "L", "X", "Y")
    ]
    refcst[, model := model_name_rds(refcst_file, var_name)]  
    return(refcst)
  }
  
  # case identity---------------------------------------------------------------
  tic()
  refcst_wide <- data.table::dcast(refcst, 
                    S + L + X + Y + model ~ M,  
                    value.var = var_name
                    )
  toc()
  gc()
  #1 min
  
  refcst_wide <- dplyr::rename_with(refcst_wide, 
                     ~ paste0(var_name, "_", .x), 
                     dplyr::matches("[0-9]")
                     )
  refcst_wide
  
}


# medias dos membros de um modelo
# para formar prev por ensemble (por mes de inicialização, leadtime e modelo)
# o nome do modelo será inserido numa coluna
# pode ser aplicado a uma lista de arquivos rds separados por lead time
ensemble_refcst_files <- function(files = model_files_rds,
                                  variable = "prec", 
                                  statistic = "mean"
                                  ){
  
  ens <- data.table::rbindlist(
    lapply(
      files,
      function(ifile) {
        cat(fs::path_file(ifile), "\n")
        # ifile = files_rds[1]
        ensemble_members(refcst_file = ifile, 
                        var_name = variable, 
                        stat = statistic
        )
      }
    )
  )
  
  #187.09 sec elapsed
  ens <- ens[order(S, L)]
  ens
}



# Para os arquivos RDS de um modelo, separados por lead time
# media das previsoes dos 10 membros nos pontos de grade,
# para cada mes de inicializ., lead time

ensemble_model_refrcst <- function(imodel, 
                                   path_files = path_qs_files,
                                   var_name = "prec", 
                                   stat = "mean",
                                   output_format = "qs"
){
  # imodel = "GFDL-SPEAR"; path_files = path_qs_files
  # model_name_rds(model_files_rds, vname = "prec")  
  checkmate::assert_choice(output_format, c("RDS", "rds", "qs"))
  
  cat(imodel, "\n")
  
  model_files <- dir_ls(
    path_files, 
    regexp = glue::glue('{imodel}_lt[0-9]\\.[0-9]')
  )
  #bin_file_ext <- fs::path_ext(model_files) %>% unique()
  
  ens_model_refcst <- ensemble_refcst_files(files = model_files, 
                                            variable = var_name, 
                                            statistic = stat
  )
  
  out_ens_file <- here(path_files,
                  paste0("ensemble-", 
                         imodel, 
                         "-", 
                         stat,
                         ".", 
                         output_format
                  )
  )
  
  export_bin_file(ens_model_refcst, out_ens_file)
  
  out_ens_file
}