
## Comparação da climatologia dos modelos NMME com obs do CRU
# 
# periodo dos dados processados

#remotes::install_github("dreamRs/esquisse")
pcks <- c("tidyverse", "here", "HEobs",
          "checkmate", "lubridate",
          "tictoc", "openair", "ggpubr", "ggExtra", "viridis", "see", "ggh4x")

easypackages::libraries(pcks)


#devtools::install_github("lhmet-ped/HEobs")

#------------------------------------------------------------------------------
# funcoes auxiliares
source("R/utils.R")
source("R/data-proc-rds.R")


#------------------------------------------------------------------------------
# Metadata - codigos bacias e nomes

#data_link <- "https://www.dropbox.com/s/d40adhw66uwueet/VazoesNaturaisONS_D_87UHEsDirceuAssis_2018.dat?dl=1"
#qnat_meta <- extract_metadata(file = data_link, informative = TRUE)
#fs::dir_create(here("input/obs/qnat"))
#readr::write_rds(qnat_meta, file = here("input/obs/qnat", "qnat_meta_ons.RDS"))

qnat_meta <- readr::read_rds(here("input/obs/qnat", "qnat_meta_ons.RDS"))
#glimpse(qnat_meta)
arrange(qnat_meta, estacao_codigo)
# para escolher um posto ONS a partir do codONS
top6()

(models_summary <- import_bin_file("output/qs/model_counts.qs"))

#------------------------------------------------------------------------------
# dados das previsoes climaticas nmme e CRU------------------------------
avg_type <- "weighted" # melhores resultados
extension <- "qs"

out_dir <- here(glue::glue("output/{extension}/basin-avgs/{avg_type}"))
nmme_cru_basin_data <- import_bin_file(
  here(out_dir, glue::glue("nmme-cru-mly-{avg_type}-avg-basins-ons.{extension}"))
) %>%
  dplyr::rename("n_L" = L)


nmme_cru_basin_clim <- clim_stats(nmme_cru_basin_data, 
                                  "prec", 
                                  funs_l = list(
                                    avg = mean,
                                    med = median,
                                    sd = sd,
                                    mad = mad
                                  ))

nmme_cru_basin_clim <- nmme_cru_basin_clim %>%
  select(model, data) %>%
  unnest() %>%
  ungroup()

#-------------------------------------------------------------------------------
# Viz

imodel <- "CanSIPS-IC3"
ibasin <- 6
var_stat <- c("avg", "sd")

obs_stat <- glue::glue("obs_{var_stat}")
regex_str <- ifelse(length(obs_stat) > 1, paste0(obs_stat, collapse = "|"), obs_stat)



vars_obs <- grep(
  regex_str, 
  names(nmme_cru_basin_clim), 
  value = TRUE
)

vars_model <- str_replace(vars_obs, "obs", "model")

m <- month.abb %>% set_names(., 1:12)

plot_data <- nmme_cru_basin_clim %>%
  dplyr::filter(codONS %in% ibasin) %>%
  dplyr::select(model:month, one_of(vars_obs, vars_model)) %>%
  pivot_longer(cols = contains("prec"), names_to = "prec", values_to = "valor") %>%
  mutate(prec = str_replace_all(prec, "prec_", "")) %>%
  separate(prec, c("type", "stat")) %>%
  pivot_wider(names_from = "stat", values_from = "valor") %>%
  dplyr::mutate(
    L = as.character(L),
    month = ordered(month, levels = c(7:12, 1:6)),
    month = recode(month, !!!m)
  ) 

# Climatologia dos modelos por lead time ---------------------------------------
ggplot(plot_data, aes(x = month, y = avg, group = type)) +
  geom_ribbon(aes(ymin = avg + sd, 
                  ymax = avg - sd,
                  fill = type), 
              alpha = 0.3) +
  geom_line(aes(colour = type), size = 1) +
  facet_grid(vars(L), vars(model)) + #,scales = "free", independent = "y"
  scale_x_discrete(labels = ~.x %>% str_sub(1, 1)) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.position = "top",
    legend.direction = "horizontal"
  )
  
# Climatologia dos modelos com lead times no mesmo plot-------------------------
ggplot(filter(plot_data, type == "model") %>%
         select(-type), 
       aes(x = month, y = avg, group = L)) +
  # geom_ribbon(aes(ymin = avg + sd, 
  #                 ymax = avg - sd,
  #                 fill = type), 
  #             alpha = 0.3) +
  geom_line(aes(colour = L)) +
  facet_wrap(vars(model)) + #,scales = "free", independent = "y"
  scale_x_discrete(labels = ~.x %>% str_sub(1, 1)) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.position = "top",
    legend.direction = "horizontal"
  ) +
  geom_line(
    data = filter(plot_data, type == "obs"),
    aes(x = month, y = avg)
  )




# ndias <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
# names(ndias) <- 1:12
# 
# 
# clima_by_lead_month_rate <- clima_by_lead_month_tot %>%
#   mutate(prec_model_dly_variav =  prec_model/ndias[as.character(month)],
#          prec_model_dly_fix = prec_model/30
#          ) %>%
#   ungroup()

# descobrindo os periodos das climatologias de cada modelo
# prec_nmme_cru_flat %>%
#   group_by(model) %>%
#   summarise(sdate = min(date), edate = max(date))
#periodos <- readr::read_rds(here("output/rds/model_counts.RDS"))




