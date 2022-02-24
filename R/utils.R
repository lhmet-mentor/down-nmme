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
  tribble(
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
  


# Funcao para calculo da climatologia dos modelos por lead time, bacia e mes ----
clim_stats <- function(data_models = nmme_cru_basin_data,
                       var_name = "prec",
                       funs_l = list(
                         avg = mean,
                         med = median,
                         sd = sd,
                         mad = mad
                       )) {
  dplyr::mutate(data_models,
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
}

