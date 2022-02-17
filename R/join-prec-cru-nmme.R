
pcks <- c(
  "tidyverse", "here",
  "checkmate", "lubridate",
  "tictoc", "openair"
)
easypackages::libraries(pcks)

source(here("R/data-proc-rds.R"))
source(here("R/tidy-basin-data.R"))
source(here("R/utils.R"))



nmme_cru_basin_data <- join_cru_nmme_basin_data(
  sp_average = "weighted",
  ext = "qs",
  var_name = "prec",
  stat = "identity"
)

(models_summary <- import_bin_file("output/qs/model_counts.qs"))
top6()



## ensemble averages
# d <- nmme_cru_basin_data[["data"]][[8]] %>%
#   #filter(year(Sr) %in% 2001) %>% 
#   filter(codONS == 24) %>%
#   mutate(L = factor(trunc(L))) %>%
#   pivot_wider(names_from = "member", 
#               values_from = "prec_model", 
#               names_glue = "{.value}_{member}"
#               ) %>%
#   pivot_longer(contains("prec"), names_to = "source", values_to = "value") %>%
#   mutate(source = as.factor(source))
# 
# d %>%
#   ggplot(aes(x = date, y = value, colour = source)) +
#   geom_line() +
#   facet_grid(vars(L), vars(month(date)))
  
  
