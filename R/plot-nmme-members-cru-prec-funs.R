#------------------------------------------------------------------------------
# funcoes auxiliares
source(here("R/utils.R"))
source(here("R/data-proc-rds.R"))

# principais bacias do SIN
#top6()



# distinctive colours
# https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/
# https://stackoverflow.com/questions/9563711/r-color-palettes-for-many-data-classes/41230685#41230685
colors_distintct <- function(n){
  # colors_distintc <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', 
  #                      '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', 
  #                      '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', 
  #                      '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080'
  # )
  #n = 28
  black_blue_red <- c("#000033", "#0000FF", "#FF0000")
  pg <- pals::glasbey(n)
  #pie(rep(1, length(pg)), col = pg)
  pg <- c(pg[!(pg %in% black_blue_red)], black_blue_red[c(1,1,2:3)])
  pg[pg == "#201A01"] <- "#BEBEBE"
  #pg[c(26:27)] <- "#000000"
  #pie(rep(1, length(pg)), col = pg)
  pg
  #pie(rep(1, length(pg)), col = pg)
}



# library(Polychrome)
## build-in color palette
# n = 24
# pie(rep(1, n), col = Polychrome::palette36.colors(n = 24))
# BOA OPCAO
# pie(rep(1, n), col = Polychrome::glasbey.colors(n = 24))

# library(randomcoloR)
# n <- 24
# palette <- distinctColorPalette(n)
# pie(rep(1, n), col=palette)


# library(RColorBrewer)
# distinct_colors <- function(n = 28) {
#   qual_col_pals <- brewer.pal.info[brewer.pal.info$category == "qual", ]
#   col_vector <- unlist(
#     mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))
#   )
#   cores <- sample(col_vector, n)
#   pie(rep(1, n), col = cores)
# }


#-------------------------------------------------------------------------------
# Obtem nomes das variaveis com os membros e ordena
.members_names <- function(data_long, col = "previsao") {
  up <- unique(data_long[[col]])
  up <- up[grep("member", up)]
  up
}



#------------------------------------------------------------------------------
# previsoes membros, modelos e media ensemble
data_nmme_cru_file <- here("output/qs/basin-avgs/weighted/nmme-cru-mly-weighted-avg-basins-ons-ens-members-models-ens-mean-prec-1982-2010.qs")
data_nmme_cru <- import_bin_file(data_nmme_cru_file)


plot_members_nmme <- function(
                              data_nmme = data_nmme_cru,
                              imodel = "CanSIPS-IC3",
                              cod_ons = 6,
                              months = c(1, 4, 8),
                              leads = 0:2,
                              highlights = c("climatology", "obs_avg", "ens_avg", "model_avg"),
                              alpha_memb = 0.4,
                              ncols_legend = 8) {

  #checkmate::assert_
  titulo <- paste0("Modelo: ", imodel)
  subtitulo <- paste0("Bacia Hidrográfica: ", stn_name(code = cod_ons))

  data_plot <- data_nmme %>%
    dplyr::relocate(month, .after = "L") %>%
    dplyr::select(
      model:date,
      all_of(highlights),
      contains("member"),
      -Sr
    ) %>%
    dplyr::mutate(month = ordered(month, levels = sort(unique(month)))) %>%
    dplyr::filter(
      month %in% months,
      L %in% leads,
      codONS == cod_ons,
      model == imodel
    ) %>%
    tidyr::pivot_longer(
      cols = -c(model:date),
      names_to = "previsao",
      values_to = "prec"
    ) %>%
    tidyr::drop_na(prec)

  memb_nms <- .members_names(data_plot, "previsao")
  n_memb <- length(memb_nms)

  # ordenar previsoes por prioridade para dar enfase aos highlights
  data_plot <- dplyr::mutate(data_plot,
    previsao = ordered(previsao,
      levels = c(memb_nms, highlights)
    )
  )

  n_colors <- length(c(memb_nms, highlights))
  
  p <- data_plot %>%
    # dplyr::filter(!previsao %in% vars_exc) %>%
    ggplot(aes(
      x = date, y = prec,
      colour = previsao, 
      linetype = previsao, 
      size = previsao,
      alpha = previsao
    )) +
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
    # scale_colour_manual(
    #    values = c(colors_distintct(), c("black", "black", "red", "blue")),
    #   labels = levels(data_plot$previsao)
    # )
    scale_size_manual(
      values = c(
        rep(0.5, n_memb),
        c(1, 1.2, 1.6, 1.4)
      ),
      labels = levels(data_plot$previsao)
    ) +
    scale_linetype_manual(
      values = c(
        rep(1, n_memb),
        c(2, 1, 3, 4)
      ),
      labels = levels(data_plot$previsao)
    ) +
    scale_alpha_manual(
      values = c(
        rep(alpha_memb, n_memb),
        rep(1, length(highlights))
      ),
      labels = levels(data_plot$previsao)
    ) +
    scale_colour_manual(
      values = colors_distintct(n_colors-1),
      labels = levels(data_plot$previsao)
    ) +
    ggtitle(titulo, subtitulo) +
    guides(col = guide_legend(ncol = ncols_legend))
    
  p
}







# levels(data_plot$previsao)

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
# ! OK

