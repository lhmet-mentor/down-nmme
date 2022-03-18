pcks <- c(
  "raster", "terra", "tidyverse", "here",
  "checkmate", "metR", "fs", "lubridate",
  "tictoc", "furrr"
)
easypackages::libraries(pcks)


source(here("R/data-join-rds.R"))
source(here("R/data-proc-rds.R"))


prec_nmme_basin_avg <- join_nmme_basin_avg_files(
  sp_average = "weighted",
  ext = "qs",
  overwrite = TRUE #! para juntar dados reprocessados de lead > 10.5
)

prec_nmme_basin_avg[["data"]][[1]]


# Verificação do período de dados dos modelos--------------------
# deve bater os intervalos de anos

models_span <- prec_nmme_basin_avg %>%
  mutate(
    data = map(data, ~ .x %>% filter(L == 0.5)),
    sdate = map_chr(
      data,
      ~ .x %>%
        pull(date_lead) %>%
        min() %>%
        as.character()
    ),
    edate = map_chr(
      data,
      ~ .x %>%
        pull(date_lead) %>%
        max() %>%
        as.character()
    ),
    data = NULL
  ) %>%
  arrange(model)

ungroup(models_span)
# A tibble: 10 × 3
# model        sdate      edate     
# <chr>        <chr>      <chr>     
# 1 CanCM4i      1981-01-01 2018-12-01
# 2 CanSIPS-IC3  1980-01-01 2020-12-01
# 3 CanSIPSv2    1981-01-01 2018-12-01
# 4 CMC1-CanCM3  1981-01-01 2010-12-01
# 5 CMC2-CanCM4  1981-01-01 2010-12-01
# 6 GEM-NEMO     1981-01-01 2018-12-01
# 7 GFDL-SPEAR   1991-01-01 2020-12-01
# 8 NASA-GEOSS2S 1981-02-01 2017-01-01
# 9 NCAR-CESM1   1980-01-01 2010-12-01
# 10 NCEP-CFSv2   1982-01-01 2010-12-01

import_bin_file(here("output/qs/model_counts.qs"))






# -----------------------------------------------
# library(openair)
# d <- prec_nmme_basin_avg[["data"]][[8]]
# d <- rename(d, "date" = date_lead) %>%
#   mutate(L = as.factor(L)) %>%
#   filter(codONS == 74)
# 
# d %>%
#   select(-date, -S) %>%
#   rename(date = Sr) %>%
#   mutate(prec = across(contains("prec"), rowMeans))
#   pivot_wider(names_from = L, values_from = prec) %>%
#   timePlot(., names(.)[-c(1,2)], group = TRUE)
