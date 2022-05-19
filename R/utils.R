## Funcao para obter nome das bacias -------------------------------------------
stn_name <- function(code, meta = qnat_meta){
  # code = prec_nmme_cru_flat$codONS
  # qnat_meta %>%
  #   filter(estacao_codigo == code) %>%
  #   pull(nome_estacao)
  # 
  codigos <- qnat_meta$estacao_codigo
  nomes <- qnat_meta$nome_estacao
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

