pcks <- c(
  "raster", "terra", "tidyverse", "here",
  "checkmate", "metR", "fs", "lubridate",
  "tictoc", "furrr"
)
easypackages::libraries(pcks)


source(here("R/data-join-rds.R"))
source(here("R/data-proc-rds.R"))


prec_nmme_basin_avg <- join_nmme_basin_avg_files()

stop()


# stop --------------------------------------------------------------------






# Verificação do período de dados dos modelos--------------------
temp <- basin_avg_nmme %>%
  mutate(sdate = map_chr(data, 
                     ~.x %>% 
                       pull(date_lead) %>% 
                       min() %>%
                       as.character()
                     ),
         edate= map_chr(data, 
                    ~.x %>% 
                      pull(date_lead) %>% 
                      max() %>%
                      as.character()
         ),
         data = NULL)

temp
read_rds("output/rds/model_counts.RDS")


# -----------------------------------------------
# library(openair)
# d <- basin_avg_nmme[["data"]][[8]]
# d <- rename(d, "date" = date_lead) %>%
#   mutate(L = as.factor(L)) %>%
#   filter(codONS == 74)
# 
# d %>%
#   select(-date, -S) %>%
#   rename(date = Sr) %>%
#   pivot_wider(names_from = L, values_from = prec) %>%
#   timePlot(., names(.)[-c(1,2)], group = TRUE)
