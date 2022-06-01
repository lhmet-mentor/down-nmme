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


# 20 distinctive colours
# "https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/"
colors_distintc <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', 
                     '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', 
                     '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', 
                     '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080'
)

#library(Polychrome)
## build-in color palette
#pie(rep(1, n), col = Polychrome::palette36.colors(n = 36))
# BOA OPCAO
#pie(rep(1, n), col = Polychrome::glasbey.colors(n = 28))

# library(randomcoloR)
# n <- 28
# palette <- distinctColorPalette(n)
# pie(rep(1, n), col=palette)


# library(RColorBrewer)
# distinct_colors <- function(n = 28) {
#   qual_col_pals <- brewer.pal.info[brewer.pal.info$category == "qual", ]
#   col_vector <- unlist(
#     mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))
#   )
#   cores <- sample(col_vector, n)
#   pie(rep(1, n), col = cores)
# }





