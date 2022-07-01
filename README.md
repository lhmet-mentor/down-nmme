
<!-- README.md is generated from README.Rmd. Please edit that file -->

# down-nmme

<!-- badges: start -->
<!-- badges: end -->

O objetivo do down-nmme é baixar os dados das previsões climáticas
(retrospectivas) do NMME.

# Dependências

``` r
remotes::install_cran("qs", type = "source", configure.args = "--with-simd=AVX2")
remotes::install_github("rspatial/terra")
remotes::install_github("rspatial/raster")
```

``` r
pcks <- c("tidyverse", "data.table", "metR", "raster", "terra", "qs", "readr", 
          "here", "checkmate", "fs", "glue", "purrr", "stringr", "tictoc",
          "lubridate", "ggpubr", "ggExtra", "viridis", "see", "ggh4x")
easypackages::libraries(c("tidyverse"))
#> Loading required package: tidyverse
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
#> ✔ ggplot2 3.3.6          ✔ purrr   0.3.4     
#> ✔ tibble  3.1.7          ✔ dplyr   1.0.9     
#> ✔ tidyr   1.2.0.9000     ✔ stringr 1.4.0     
#> ✔ readr   2.1.2          ✔ forcats 0.5.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> All packages loaded successfully
#fs::dir_ls(here("R"), glob = "*.R") %>%
#  fs::path_file()
packageVersion("terra")
#> [1] '1.5.20'
packageVersion("raster")
#> [1] '3.5.15'
packageVersion("qs")
#> [1] '0.25.2'
```

# Descrição

1.  `down-hindcasts-nmme.R`: script com looping para baixar arquivos
    NetCDF para um intervalo de anos e uma lista de modelos.

-   depende do script `models-nmme.R` que gera objeto chamado
    `models_info` com informações dos modelos, como nomes e períodos.
    Requer alteração quando inserir novos modelos. Tabela gerada
    manualmente. Atualmente somente o nome dos modelos desta tabela são
    usados.

-   depende do script `down-nmme.R`: função para *download* dos dados
    por modelo e ano, no formato NetCDF. O domínio espacial dos dados é
    a América do Sul.

-   arquivos de saída em `output/ncdf/nmme_{variavel}_{modelo}_{ano}.nc`

2.  `dados-brutos.R`:

-   depende do script `data-proc-nc.R` que contém as funções para:

    -   extrair os dados de todos arquivos NetCDF de um dado modelo,
        para todos *lead times* (`L` o qual varia de 0.5 a 11.5).

    -   gerar a tabela `model_counts.{rds, qs}` com o periodo dos
        modelos e as dimensões dos arquivos.

-   arquivos de entrada em
    `output/ncdf/nmme_{variavel}_{modelo}_{ano}.nc`

-   arquivos de saída em
    `output/{rds,qs}/nmme_{var_name}_{model_id}_lt{lead_time}.{rds, qs}`
    (arquivo `.qs` com tamanho de \~300 MB contra \~3 GB do `.rds`)

------------------------------------------------------------------------
