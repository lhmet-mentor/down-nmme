# Funções para processamento dos dados
# de reforecast dos modelos nmme pre-processados (convertidos de nc para rds)

##------------------------------------------------------------------------------
# Funcao par extrai nome do modelo a partir do arquivo rds
model_name_rds <- function(file_rds, vname = "prec"){
  
  #file_rds = files_rds; vname = "prec"
  #file_rds
  nm <- fs::path_file(file_rds) %>%
    stringr::str_replace_all(pattern = glue::glue("nmme_{vname}_"), "") %>%
    stringr::str_replace_all(pattern = "_lt[0-9]{1,2}\\.[0-9]{1}\\.RDS", "") %>%
    unique()
  
  assert_true(length(nm) == 1)
  nm
  
}


# Média e desvio padrao dos membros de prec (ensemble) para cada ponto----------
ensemble_members <- function(refcst_rds, var_name = "prec", stat = "median") {
  # refcst_rds <- files_rds[1]; var_name = "prec"; stat = "identity"
  
  checkmate::assert_choice(stat, c("mean", "median", "identity"))
  
  tic()
  refcst <- readr::read_rds(refcst_rds)
  toc()
  
  if(stat == "median"){
    refcst <- refcst[,
                     .(# mediana e mad (medidas stats + robustas)
                       prec_ensmed = median(prec),
                       prec_ensmad = mad(prec)
                     ),
                     #keyby = .(S, L)
                     by = c("S", "L", "X", "Y")
    ]  
    refcst[, model := model_name_rds(refcst_rds, var_name)]
    return(refcst)
  }
  
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
    refcst[, model := model_name_rds(refcst_rds, var_name)]  
    return(refcst)
  }
  
  # case identity
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
  
  
}


# medias dos membros de um modelo
# para formar prev por ensemble (por mes de inicialização, leadtime e modelo)
# o nome do modelo será inserido numa coluna
# pode ser aplicado a uma lista de arquivos rds separados por lead time
ensemble_refcst_files <- function(files_rds = model_files_rds,
                                  variable = "prec", 
                                  statistic = "mean"
                                  ){
  ens <- data.table::rbindlist(
    lapply(
      files_rds,
      function(ifile) {
        cat(fs::path_file(ifile), "\n")
        # ifile = files_rds[1]
        ensemble_members(refcst_rds = ifile, 
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
                                   path_rds = path_rds_files,
                                   var_name = "prec", 
                                   stat = "mean"
){
  # imodel = "GFDL-SPEAR"
  # path_rds = path_rds_files
  #model_name_rds(model_files_rds, vname = "prec")  
  cat(imodel, "\n")
  
  model_files_rds <- dir_ls(
    path_rds, 
    regexp = glue::glue('{imodel}_lt[0-9]\\.[0-9]\\.RDS')
  )
  
  ens_model_refcst <- ensemble_refcst_files(files_rds = model_files_rds, 
                                            variable = var_name, 
                                            statistic = stat
  )
  
  out_rds <- here(path_rds,
                  paste0("ensemble-", 
                         imodel, 
                         "-", 
                         stat,
                         ".RDS"
                  )
  )
  saveRDS(ens_model_refcst, file = out_rds)
  out_rds
}