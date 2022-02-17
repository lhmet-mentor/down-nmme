
<!-- README.md is generated from README.Rmd. Please edit that file -->

# download-hindcast-NMME

<!-- badges: start -->
<!-- badges: end -->

O objetivo do download-hindcast-NMME é processar os dados das previsões
climática do NMME.

# Dependências

``` r
remotes::install_cran("qs", type = "source", configure.args = "--with-simd=AVX2")
remotes::install_github("rspatial/terra")
remotes::install_github("rspatial/raster")
```

``` r
pcks <- c("tidyverse", "data.table", "metR", "raster", "terra", "qs", "readr", 
          "here", "checkmate", "fs", "glue", "purrr", "stringr", "tictoc",
          "lubridate")
easypackages::libraries(c("tidyverse"))
#> Loading required package: tidyverse
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
#> ✓ ggplot2 3.3.5          ✓ purrr   0.3.4     
#> ✓ tibble  3.1.6          ✓ dplyr   1.0.5     
#> ✓ tidyr   1.1.3.9000     ✓ stringr 1.4.0     
#> ✓ readr   1.4.0          ✓ forcats 0.5.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
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

-   depende do script `models-nmme.R` que gera objeto chamado `tabela1`
    com informações dos modelos, como nomes e períodos. Requer alteracao
    quando inserir novos modelos.

-   depende do script `down-nmme.R`: função para *download* dos dados
    por modelo e ano, no formato NetCDF. O domínio espacial dos dados é
    a América do Sul.

-   arquivos de saída em
    `output/{variavel}/nmme_{variavel}_{modelo}_{ano}.nc`

2.  `dados-brutos.R`:

-   depende do script `data-proc-nc.R` que contém as funções para
    extrair os dados de todos arquivos NetCDF de um dado modelo, para
    todos *lead times* (`L` o qual varia de 0.5 a 11.5).

    -   arquivos de entrada em
        `output/{variavel}/nmme_{variavel}_{modelo}_{ano}.nc`

-   arquivos de saída em
    `output/{rds,qs}/nmme_{var_name}_{model_id}_lt{lead_time}.{rds, qs}`
    (arquivo `.qs` com tamanho de \~300 MB contra \~3 GB do `.rds`)

3.  `ensemble-members-averages.R`:

-   depende do script `data-proc-rds.R` que contém as funções para obter
    a média ou a mediana (para cada ponto, por data de inicialização e
    lead time) de todos membros de um dado modelo.

-   arquivos de saída em
    `output/{rds,qs}/ensemble-{model_id}-{stat}.{RDS,qs}` (cada arquivo
    \~290 MB)

4.  `spatial-average-nmme-basins.R` (etapa mais demorada)

-   arquivos de entrada em
    `output/{rds,qs}/ensemble-{model_id}-{stat}.{RDS, qs}`

-   depende do script `data-proc-basin.R` que contém as funções para
    obter a média na área das bacias hirográficas (por data de
    inicialização e *lead time*) da variável de interesse (precipitação,
    temperatura) agregada (média, mediana ou identidade) de cada modelo.

    -   Há duas opções para agregação espacial: média aritmética
        (`arithmetic`) ou média ponderada pela fração de área das
        células do modelo dentro da bacia hidrográfica (`weighted`).

-   arquivos de saída em
    `output/rds/basin/avgs/{spatial_average_type}/{model_id}-{stat}`. Os
    arquivos são separados por tipo de média espacial, modelo, mês de
    inicialização e lead time. O nome dos aquivos RDS segue o padrão
    `{model_id}_S{YYYYMMDD}_L{0.5-11.5}_{spatial_agreggation_type}_avg.RDS`
    (cada arquivo em torno de 4.5 KB)

5.  `join-spavg-nmme-basins.R`:

-   depende do script `data-join-rds.R` que contém a função para juntar
    todos arquivos binários (`.qs` ou `.RDS`).

-   arquivos de saída em `output/rds/basin/avgs/{sp_average}` no fomato
    RDS no padrão `nmme-models-{sp_average}-avg-basins-ons.RDS` (cada
    arquivo em torno de 160 MB)

6.  `spatial-average-obs-basins.R`:

-   depende do script `data-proc-basin.R` que contém as funções para
    obter a média na área bacias hidrográficas para os dados do CRU.

-   arquivos de saída em `output/rds/basin/avgs/{sp_average}` no formato
    RDS no padrão `cru-prec-basins-{sp_average}-avg.RDS` (cada arquivo
    em torno de 3.6 MB)

7.  `join-prec-cru-nmme.R`: combina os dados observados e previsões dos
    membros dos modelos.

-   depende do script `tidy-basin_data.R` que arruma os dados das
    previsões dos membros dos modelos no formato *tidy* para combiná-los
    com as observações do CRU.

-   arquivo de saída em
    `output/{ext}/basin-avgs/{sp_average}/nmme-cru-mly-{sp_average}-avg-basins-ons.qs`,
    onde `ext = 'qs' ou 'RDS'` e
    `sp_average =  'weighted' ou 'arithmetic'`.

8.  `evaluation-cru-nmme.R`:

-   depende do script `utils.R` que contém as funções auxiliares para
    obter nomes dos postos e tabela com código e nome das ‘28’ maiores
    hidrelétricas e dos 6 maiores aproveitamentos hidrelétricos do SIN.

-   calcula a correlação entre prev e obs para os diferentes tempos de
    antecedências por meses e apresenta visualização para algumas Bacias
    Hidrográficas;

9.  `comparacao-climatol-cru-nmme.R` TO DO DESC.
