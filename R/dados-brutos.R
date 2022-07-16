pcks <- c("terra", "tidyverse", "here", "checkmate", "metR", "fs", "glue", "qs")
easypackages::libraries(pcks)

## Funções para processamento dos dados netcdf------------------------------
source(here("R/data-proc-nc.R"))
source(here("../proc-NMME/R/data-proc-rds.R"))

## util somente no caso de obter os dados do google drive ----------------------
# source(here("R", "unzip-nc.R"))
# unzip_ncs()

var_name = "prec"

## listar todos os arquivos das previsões retrospectivas da precipitação CanCM4i
nc_dir <- here::here("output", "ncdf")

# count_ncs <- fs::dir_ls(nc_dir) %>%
#   fs::path_ext() %>%
#   table()
## total de arquivos nc
# count_ncs
#  271 
# Apos incluir CanSIPS-IC3 e GFDL-SPEAR
# 342

nc_files <- fs::dir_ls(path = nc_dir, glob = "*.nc")

model_counts <- nc_files_by_model_year(nc_files, "qs", var_name)
# HINDCASTAS
# modelo          start   end  freq check_span     M     L     S     X     Y
# <chr>        <int> <int> <int>      <dbl> <int> <int> <int> <int> <int>
# 1  CanCM4i       1981  2018    38         38    10    12    12    56    76
# 2  CanSIPS-IC3   1980  2020    41         41    20    12    12    56    76
# 3  CanSIPSv2     1981  2018    38         38    20    12    12    56    76
# 4  CMC1-CanCM3   1981  2010    30         30    10    12    12    56    76
# 5  CMC2-CanCM4   1981  2010    30         30    10    12    12    56    76
# 6  GEM-NEMO      1981  2018    38         38    10    12    12    56    76
# 7  GFDL-SPEAR    1991  2020    30         30    15    12    12    56    76
# 8  NASA-GEOSS2S  1981  2017    37         37     4     9    12    56    76
# 9  NCAR-CESM1    1980  2010    31         31    10    12    12    56    76
# 10 NCEP-CFSv2    1982  2010    29         29    24    10    12    56    76

# FORECASTS
# modelo       start   end  freq check_span     M     L     S     X     Y
# <chr>        <int> <int> <int>      <dbl> <int> <int> <int> <int> <int>
#   1 CanCM4i     2016  2021     6          6    10    12    12    56    76
# 2 CanSIPS-IC3   2021  2022     2          2    20    12     3    56    76
# 3 CanSIPSv2     2016  2021     6          6    20    12    12    56    76
# 4 CMC1-CanCM3   2011  2019     9          9    10    12    12    56    76
# 5 CMC2-CanCM4   2011  2019     9          9    10    12    12    56    76
# 6 GEM-NEMO      2016  2021     6          6    10    12    12    56    76
# 7 GFDL-SPEAR    2020  2022     3          3    30    12    12    56    76
# 8 NASA-GEOSS2S  2017  2022     6          6    10     9    12    56    76
# 9 NCAR-CESM1    2016  2017     2          2    10    12     6    56    76

# como o nome dos arquivos especifica o nome dos modelos
#model_nm <- "CanCM4i"
(model_nms <- model_counts$modelo)
#files_model <- nc_files[grep(model_nms, nc_files)]


# # # para obter informacao do lead time
# model_nm <- "CanSIPS-IC3"
# model_files <- nc_files[grep(model_nm, nc_files)]
# # variaveis e dimensoes com funcao do pacote metR
# nc_info <- GlanceNetCDF(model_files[1])
# nc_info
# # nc_info é uma lista
# str(nc_info)
# # lead time pode ser obtido dela
# str(nc_info$dims$L)
# (lt <- nc_info$dims$L$vals)
# membros
# n_members <- length(nc_info$dims$M$vals)


# looping para processar dados 
# model_nms <- model_nms[c(2, 7)]
map(model_nms,
    function(imodel){
      # imodel = model_nms[1]
      cat(imodel, "-------\n", "\n")
      gc()
      proc_ncs_by_lt(model = imodel, 
                     variavel = "prec",
                     lead_time = seq(0.5, 11.5, by = 1), 
                     input_d = nc_dir,
                     output_d = here("output", "qs"),
                     overwrite = FALSE
                    )
    })
