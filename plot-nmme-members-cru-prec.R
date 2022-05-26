pcks <- c(
  "tidyverse", "here", "HEobs",
  "checkmate", "lubridate",
  "tictoc", "openair", "ggpubr", "ggExtra", "viridis", "see", "ggh4x"
)

easypackages::libraries(pcks)


# devtools::install_github("lhmet-ped/HEobs")

#------------------------------------------------------------------------------
# funcoes auxiliares
source("R/utils.R")
source("R/data-proc-rds.R")

# principais bacias do SIN
top6()

#------------------------------------------------------------------------------
# previsoes membros, modelos e media ensemble
data_nmme_cru_file <- here("output/qs/basin-avgs/weighted/nmme-cru-mly-weighted-avg-basins-ons-ens-members-models-ens-mean-prec-1982-2010.qs")
data_nmme_cru <- import_bin_file(data_nmme_cru_file)



imodel <- "CanSIPS-IC3"
ibasin <- 6
months_plot <- c(1, 7, 10)
leads_plot <- 0:2

titulo <- paste0("Modelo: ", imodel)
subtitulo <- paste0("Bacia Hidrográfica: ", stn_name(code = ibasin))

data_plot <- data_nmme_cru %>%
  dplyr::select(model:date, 
                model_avg, obs_avg, ens_avg, 
                contains("member"),
                -Sr
                ) %>%
  dplyr::mutate(month = as.integer(lubridate::month(date)),
                month = ordered(month, levels = sort(unique(month)))
  ) %>%
  dplyr::relocate(month, .after = "L") %>%
  dplyr::filter(month %in% months_plot, 
                L %in% leads_plot, 
                codONS == ibasin,
                model == imodel
                ) %>%
  tidyr::pivot_longer(cols = -c(model:date), 
                      names_to = "previsao", 
                      values_to = "prec"
                      ) %>%
  dplyr::filter(!is.na(prec))

#data_plot %>% filter(L == 1, month == 1, previsao == "member_1")

data_cli <- dplyr::filter(data_plot, previsao == "obs_avg") %>%
  dplyr::group_by(L, month) %>%
  dplyr::summarise(climatology = mean(prec), .groups = "drop") 

vars_exc <- data_plot %>%
  dplyr::filter(str_detect(previsao, "avg")) %>%
  pull(previsao) %>%
  unique()

p <- data_plot %>%
  dplyr::filter(!previsao %in% vars_exc) %>%
  ggplot(aes(x = date, y = prec, colour = previsao)) +
  geom_line() +
  facet_grid2(vars(L), vars(month),
              scales = "free", independent = "y"
  ) +
  # scales = "free_y", independent = "y", space = "free_x") +
  # scale_x_date(date_breaks = "6 years", date_labels = "%y") +
  scale_x_date(
    date_labels = "%y", date_breaks = "5 years",
    date_minor_breaks = "2 years", expand = c(0, 0)
  ) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.position = "top",
    legend.direction = "horizontal"
  ) +
  geom_line(
    data = filter(data_plot, previsao %in% vars_exc),
    aes(x = date, y = prec, colour = previsao), size = 1
  )

g <- ggplot_build(p)
cols <- unique(g$data[[1]]["colour"])
labs <- data_plot %>%
  dplyr::filter(!previsao %in% vars_exc) %>%
  dplyr::pull(previsao) %>%
  unique() %>%
  as.character()



p <- p +
  scale_colour_manual(
    values = c(cols[[1]], c("red", "black", "blue")),
    labels = c(labs, vars_exc)
  ) +
  geom_hline(
    data = data_cli,
    aes(yintercept = climatology), colour = "black", lty = 2
  ) +
  ggtitle(titulo, subtitulo)
p


# model_avg errada!

