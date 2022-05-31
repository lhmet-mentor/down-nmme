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

.members_names <- function(data_long, col = "previsao"){
  up <- unique(data_long[[col]])
  up <- up[grep("member", up)]
  up
}



#------------------------------------------------------------------------------
# previsoes membros, modelos e media ensemble
data_nmme_cru_file <- here("output/qs/basin-avgs/weighted/nmme-cru-mly-weighted-avg-basins-ons-ens-members-models-ens-mean-prec-1982-2010.qs")
data_nmme_cru <- import_bin_file(data_nmme_cru_file)


plot_members_nmme <- function(
         model = "CanSIPS-IC3", 
         cod_ons = 6, 
         months = c(1, 4, 8),
         leads = 0:2,
         highlights = c("climatology", "obs_avg", "ens_avg", "model_avg")
){

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
    tidyr::drop_na(prec)
  
  memb_nms <- .members_names(data_plot, "previsao")
  
  # juntar com a climatologia
  
  
  data_plot <- dplyr::mutate(data_plot,
                             previsao = ordered(previsao, levels = c(memb_nms, highlights)
                             )
  )
  
  
}



#library(Polychrome)
## build-in color palette
#Glasbey <- glasbey.colors(32)



#levels(data_plot$previsao)

# # ckeck model_avg values
# mod_avgs <- data_plot %>%
#   filter(previsao %in% paste0("member_", 1:20)) %>%
#   group_by(model, codONS, L, date) %>%
#   summarise(prec_avg = mean(prec), .groups = "drop") %>%
#   arrange(L, date)
# 
# data_nmme_cru %>% 
#   filter(codONS == ibasin, model == imodel,
#          month(date) %in% months_plot, L %in% leads_plot) %>%
#   group_by(model, codONS, L, date) %>%
#   arrange(L)
#! OK

#data_plot %>% filter(L == 1, month == 1, previsao == "member_1")



# subconjunto para linhas a destacar
vars_exc <- tail(levels(data_plot$previsao), 3)

p <- data_plot %>%
  #dplyr::filter(!previsao %in% vars_exc) %>%
  ggplot(aes(x = date, y = prec, 
             colour = previsao, linetype = previsao, size = previsao,
             alpha = previsao)) +
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
  scale_colour_manual(
    values = c(colors_distintc, c("black", "red", "blue")),
    labels = levels(data_plot$previsao)
  )
  #scale_colour_metro_d() 
  
  # geom_line(
  #   data = filter(data_plot, previsao %in% tail(levels(data_plot$previsao), 3)),
  #   aes(x = date, y = prec, colour = previsao), size = 1
  # )

# pega cores usadas no plot
g <- ggplot_build(p)
cols <- unique(g$data[[1]]["colour"])

p <- p +
  # scale_colour_manual(
  #   values = c(colors_distintc, c("black", "red", "blue")),
  #   labels = levels(data_plot$previsao)
  # ) +
  # scale_alpha_manual(
  #   values = c(rep(0.2, length(labs)), rep(1, length(vars_exc)))
  # ) +
  scale_size_manual(values = c(rep(0.5, length(head(cols[[1]], -3))),
                               c(1.2, 1.6, 1.4)
                               ),
                    labels = levels(data_plot$previsao)
                    ) + 
  scale_linetype_manual(values = c(rep(1, length(head(cols[[1]], -3))),
                                   c(1, 3, 4)),
                        labels = levels(data_plot$previsao)
  ) +
  scale_alpha_manual(values = c(rep(0.4, length(head(cols[[1]], -3))),
                                rep(1, 3)),
                     labels = levels(data_plot$previsao)
  ) +
  geom_hline(
    data = data_cli,
    aes(yintercept = climatology), colour = "black", lty = 2
  ) +
  ggtitle(titulo, subtitulo) 
  
p

# p +
#  geom_line(
#    data = filter(data_plot, previsao %in% tail(levels(data_plot$previsao), 3)),
#    aes(x = date, y = prec, linetype = previsao), size = 1
#  )

