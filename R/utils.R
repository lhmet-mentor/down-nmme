## Funcao para obter nome das bacias -------------------------------------------
stn_name <- function(code, meta){
  # code = prec_nmme_cru_flat$codONS
  # qnat_meta %>%
  #   filter(estacao_codigo == code) %>%
  #   pull(nome_estacao)
  # 
  if(missing(meta)){
    meta <- readr::read_rds(here("input/obs/qnat", "qnat_meta_ons.RDS"))  
  }
  
  codigos <- meta$estacao_codigo
  nomes <- meta$nome_estacao
  names(nomes) <- codigos
  
  nomes[as.character(code)]
}



# filtra os dados para as 28 principais reservetórios de regularizacao----------
# monitorados pelo ONS
major28 <- function(){
  
  # digitado manualmente a partir de 
  # Tabela 1 do artigo 
  tibble::tribble(
    ~codONS,  ~nome,      ~bacia,
    6,       "FURNAS",      "Grande",
    #"MASCARENHAS" (M. MORAES)?
    17,       "MARIMBONDO",  "Grande",
    18,       "A. VERMELHA", "Grande",
    
    24,       "EMBORCACAO",  "Paranaíba",
    25,       "NOVA PONTE",  "Paranaíba",
    31,       "ITUMBIARA",   "Paranaíba",
    33,       "SAO SIMAO",   "Paranaíba",
    
    47,       "JURUMIRIM",   "Paranapanema",
    61,       "CAPIVARA",   "Paranapanema",
    #CHAVANTES?
    74,       "G.B. MUNHOZ",   "Iguaçu",
    77,       "SLT.SANTIAGO",   "Iguaçu",
    
    92,       "ITA",           "Uruguai",
    93,       "PASSO FUNDO",   "Uruguai",
    # MACHADINHO?
    111,       "PASSO REAL",    "Jacui",
    
    237,       "BARRA BONITA",   "Tietê",
    240,       "PROMISSAO",      "Tietê",
    243,       "TRES IRMAOS",    "Tietê",
    
    34,       "I. SOLTEIRA",    "Paraná",
    245,            "JUPIA",    "Paraná",
    266,            "ITAIPU",    "Paraná",
    
    156,       "TRES MARIAS",    "São Francisco",
    169,       "SOBRADINHO",     "São Francisco",
    # ITAPARICA?
    270,       "SERRA MESA",     "Tocantins",
    275,       "TUCURUI",        "Tocantins"
    
  )
}
# stn_name(code = qnat_meta$estacao_codigo)
# stn_name(major28()$codONS)

top6 <- function(){
  mjrs <- major28()
  top6_aprovhidrel <- c(156, 6, 270, 31, 24, 25)
  dplyr::filter(mjrs, codONS %in% top6_aprovhidrel)
}
  


.filename_basin_data <- function(avgtype, extensao, sufixo = ""){
  in_dir <- here::here(glue::glue("output/{extensao}/basin-avgs/{avgtype}"))
  input_file <- here::here(in_dir, 
                     glue::glue("nmme-cru-mly-{avgtype}-avg-basins-ons{sufixo}.{extensao}")
  )
  input_file
}



#-----------------------------------------------------------------------------
# Renomear arquivos 'nmme_{vname}_{old_model_nm}_{type}_year.nc' para 
#                   'nmme_{vname}_{new_model_nm}_{type}_year.nc'
# './output/ncdf/{modelo}'
## Processamento para uma variavel e um modelo---------------------------------
source(here::here("R/nmme-realtime.R"))
# parametros

# modelos operacionais
#tab_models_op <- nmme_oper_from_cpc()
#nms_models_op <- tab_models_op$directory

rename_files_nmme <- function(old_model_nm = "COLA-RSMAS-CCSM4",
                              new_model_nm = "NCAR_CCSM4",
                              var_name = "prec"){
  
  nc_dir <- here::here("output", "ncdf"); 
  
  old_files_nc <- fs::dir_ls(here::here("output/ncdf"), 
                             type = "file", 
                             glob = "*.nc", 
                             recurse = TRUE
  ) %>%
    grep(pattern = old_model_nm, x = ., value = TRUE) %>%
    grep(pattern = var_name, x = ., value = TRUE)
  
  new_files_nc <- stringr::str_replace_all(old_files_nc, old_model_nm, new_model_nm)  
  file.rename(old_files_nc, new_files_nc)
}

# rename_files_nmme()



modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}



# count_ncs <- fs::dir_ls(nc_dir) %>%
#   fs::path_ext() %>%
#   table()
## total de arquivos nc
# count_ncs
#  271 
# Apos incluir CanSIPS-IC3 e GFDL-SPEAR
# 342

nc_files <- fs::dir_ls(path = nc_dir, glob = "*.nc")

#-----------------------------------------------------------------------------
# Corrigir nome dos arquivos ncdf baixados previamente para o padrao
# Funcao especifica e nao eh par ser usada fora deste contexto

.rename_old_ncs <- function(){
  files_nc <- fs::dir_ls(here::here("output/ncdf"), type = "file", glob = "*.nc")
  file_names <- fs::path_file(files_nc)
  is_hc_file <- !stringr::str_detect(file_names, "forecast")
  hindcasts_files <- file_names[is_hc_file]
  
  hindcasts_files_new <- str_split(hindcasts_files, "_") %>% 
    map_chr(., function(x){
      # x <- str_split(hindcasts_files, "_")[[1]]
      paste(c(x[1:3], "HINDCAST", x[-c(1:3)]), collapse = "_")
    })
  
  hindcasts_files_new <- path(fs::path_dir(files_nc)[is_hc_file],
                              hindcasts_files_new
                              )
  hindcasts_files <- path(fs::path_dir(files_nc)[is_hc_file],
                          hindcasts_files
  )
    
  file.rename(hindcasts_files, hindcasts_files_new)
  # depois copiei manualmente os dados baixados por Torma para o output/ncdf
}


