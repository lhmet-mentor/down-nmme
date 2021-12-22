
pcks <- c("tidyverse", "here", "HEobs",
          "checkmate", "lubridate",
          "tictoc", "openair", "ggpubr", "ggExtra", "viridis", "see")
easypackages::libraries(pcks)


#devtools::install_github("lhmet-ped/HEobs")

#------------------------------------------------------------------------------
# funcoes auziliares
source("R/utils.R")

#------------------------------------------------------------------------------
# Metadata - codigos bacias e nomes

#data_link <- "https://www.dropbox.com/s/d40adhw66uwueet/VazoesNaturaisONS_D_87UHEsDirceuAssis_2018.dat?dl=1"
#qnat_meta <- extract_metadata(file = data_link, informative = TRUE)
#fs::dir_create(here("input/obs/qnat"))
#readr::write_rds(qnat_meta, file = here("input/obs/qnat", "qnat_meta_ons.RDS"))

qnat_meta <- readr::read_rds(here("input/obs/qnat", "qnat_meta_ons.RDS"))
glimpse(qnat_meta)
arrange(qnat_meta, estacao_codigo)


#------------------------------------------------------------------------------
# previsoes climaticas nmme
#avg_type <- "arithmetic"
avg_type <- "weighted" # melhores resultados

out_dir <- here(
  str_replace("output/rds/basin-avgs/avg_type", "avg_type", avg_type)
  )

prec_nmme_cru <- readr::read_rds(
  file = here(out_dir,
              str_replace("nmme-cru-mly-avg_type-avg-basins-ons.RDS",
                          "avg_type",
                          avg_type
                          )
              )
  ) 

prec_nmme_cru_lfix <- prec_nmme_cru %>%
  mutate(data = map(data, 
                    ~ .x %>% mutate(S = NULL)
                    )
  )

# prec_nmme_cru[["data"]][[8]] %>%
#   mutate(
#     Sr = floor_date(S + ddays(15), "month"),
#     S = NULL
#   ) %>%
#   relocate(Sr, .before = L)

## se nao eh feita esta correcao nao ha meses de inicializacao em fevereiro!
## como descobri isso ...

# # modelos q
# prec_nmme_cru %>%
#   filter(model != "CanCM4i") %>%
#   unnest() %>%
#   ungroup() %>%
#   select(S) %>%
#   distinct(format(S, "%m-%d")) %>% as.data.frame()
# 

# prec_nmme_cru %>%
#   filter(model != "CanCM4i") %>%
#   unnest() %>%
#   ungroup() %>%
#   mutate(Sr = floor_date(S + ddays(15), "month")) %>%
#   filter(L == 0.5) %>%
#   select(S, Sr) %>% #as.data.frame()
# mutate(across(everything(), ~.x %>% format("%m-%d"))) %>%
#   distinct() %>% as.data.frame()


prec_nmme_cru_flat <-  prec_nmme_cru_lfix %>%
  unnest() %>%
  ungroup()


# -----------------------------------------------------------------------------
# calculo das correlacoes entre a prec obs e as previsoes para diferentes
# lead e meses

nested_mlcm <- prec_nmme_cru_flat %>%
  mutate(L = as.integer(trunc(L))) %>%
  group_by(model, mes = as.integer(month(Sr)), L, codONS) %>%
  dplyr::select(-Sr) %>%
  nest()

nested_mlcm[["data"]][[1]]


tab_cor <- nested_mlcm %>% 
  mutate(
    test = map(data, ~ cor.test(.x$prec_model, .x$prec_obs)), 
    tidied = map(test, broom::tidy),
    test = NULL
  )%>% 
  unnest(tidied) %>%
  mutate(p.value = round(p.value, 2)) %>%
  arrange(desc(estimate))

gc()

# sites selecionados
i_site <- top6()$codONS #c("266", "275", "6", "287", "92")
stn_name(i_site)


# filter(tab_corr, codONS %in% i_site, model == "CanSIPSv2") %>%
#   ungroup() %>%
#   arrange(mes, L) %>%
#   pivot_wider(names_from = "mes", values_from = r)
# 
# check_leads <- tab_corr %>%
#   ungroup(cols = c("model", "mes", "L","codONS")) %>%
#   filter(codONS == i_site) %>%
#   select(-r) %>% 
#   distinct() %>%
#   arrange(codONS, mes, L)
# View(check_leads)

data_cor_plot <- tab_cor %>%
  dplyr::select(-c(data, statistic, parameter, method, alternative, contains("conf"))) %>%
  dplyr::ungroup(cols = c("model", "mes", "L","codONS")) %>%
  dplyr::filter(codONS %in% i_site) %>%
  dplyr::mutate(codONS = stn_name(codONS))

limiar_alpha <- 0.05 # nivel de confiança

# pctg de casos significativos de correl
(nrow(filter(data_cor_plot, p.value < limiar_alpha))/nrow(data_cor_plot) * 100)
# wavg
# 13.18 %
# aavg
# 12.82 %


correl_plot_by_month_lead <- data_cor_plot %>%
  ggplot(aes(x = L, y = mes, fill = estimate)) +
  geom_tile(color = "white", size = 0.1) +
  scale_y_continuous(breaks = 1:12, expand = c(0, 0)) +
  scale_x_continuous(breaks = 0:11, expand = c(0, 0)) +
  scale_fill_gradient2(name = "correlação", low = "blue", high = "red") +
  # scale_fill_distiller(palette = "RdBu") +
  # scale_fill_viridis(name="Correlation", option ="C") +
  # scale_fill_manual(values = myPallette) +
  facet_grid(codONS ~ model) +
  theme_minimal(base_size = 8) +
  geom_tile(
    data = filter(data_cor_plot, p.value < limiar_alpha),
    aes(x = L, y = mes),
    color = "black", fill = NA,
    size = 0.15
  ) +
  labs(title= "Previsões brutas de precipitação mensal do NMME",
    y = "Mês de referência", x = "Horizonte de previsão (meses)"
  ) +
  theme(legend.position = "bottom",
      plot.title=element_text(size = 14, hjust = 0),
      axis.text.y=element_text(size=6),
      strip.background = element_rect(colour="white"),
      axis.ticks=element_blank(),
      axis.text=element_text(size=7),
      legend.title=element_text(size=8),
      legend.text=element_text(size=6)
    ) #+ removeGrid()  
  
correl_plot_by_month_lead


# theme(
#   panel.grid.major = element_blank(),
#   panel.border = element_blank(),
#   panel.background = element_blank(),
#   axis.ticks = element_blank()
# )

#------------------------------------------------------------------------------
# 
# tab_corr %>%
#   #filter(codONS == i_site) %>%
#   ggplot(aes(x = L, y = r, color = model)) +
#   geom_boxplot() +
#   scale_color_social() +
#   facet_wrap(~mes) +
#   scale_x_continuous(breaks = 0:11) +
#   theme_bw() +
#   theme(strip.background = element_rect(colour="snow"))
  







