.tidy_data_for_clim_plots <- function(.monthly_data = nmme_cru_basin_clim,
                                      .ibasin = 6,
                                      .var_name = "prec",
                                      .var_stat = c("avg", "sd")
) {
  
  # medidas de centralidade e dispersao
  cent <- .var_stat[[1]]
  disp <- .var_stat[[2]]
  
  # obter stats das obs
  obs_stat <- glue::glue("obs_{.var_stat}")
  regex_str <- ifelse(length(obs_stat) > 1, paste0(obs_stat, collapse = "|"), obs_stat)
  
  vars_obs <- grep(
    regex_str, 
    names(.monthly_data), 
    value = TRUE
  )
  # obter stats dos models
  vars_model <- str_replace(vars_obs, "obs", "model")
  
  # para labels no x com a 1a letra do mes 
  m <- month.abb %>% set_names(., 1:12)
  
  .plot_data <- .monthly_data %>%
    dplyr::filter(codONS %in% .ibasin) %>%
    dplyr::select(model:month, one_of(vars_obs, vars_model)) %>%
    tidyr::pivot_longer(
      cols = dplyr::contains(.var_name),
      names_to = .var_name,
      values_to = "valor"
    ) %>%
    dplyr::mutate(!!var_name := stringr::str_replace_all(
      !!sym(.var_name),
      glue::glue("{var_name}_"),
      ""
    )) %>%
    tidyr::separate(!!sym(.var_name), c("type", "stat")) %>%
    tidyr::pivot_wider(names_from = "stat", values_from = "valor") %>%
    dplyr::mutate(
      L = as.character(L),
      # para centralizar em dez e jan (ano hidrologico)
      month = ordered(month, levels = c(7:12, 1:6)),
      month = dplyr::recode(month, !!!m)
    ) %>%
    dplyr::mutate(
      yupper = !!sym(cent) + !!sym(disp),
      ylower = !!sym(cent) - !!sym(disp)
    )
  
  .plot_data
}





# Climatologia dos modelos com lead times no mesmo plot-----------------------
plot_clim_grpleads_by_model <- function(.plot_data, .cent, .tit, .subt){
  ggp_clim_by_grp_lead_models <- 
    ggplot(filter(.plot_data, type == "model") %>% select(-type), 
           aes(x = month, y = !!sym(.cent), group = L)) +
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
      # data = filter(plot_data, type == "obs")%>% distinct(month, !!sym(cent)),
      # PQ OS PERIODOS DE ANOS VARIAM POR MODELO
      data = filter(.plot_data, type == "obs") %>%
        group_by(month) %>% 
        summarise(across(sym(.cent):ylower, mean)) %>%
        full_join(.,
                  filter(plot_data, type == "obs") %>% select(model:month),
                  by = "month"
        ),
      aes(x = month, y = !!sym(.cent)),
      size = 1.1, alpha = 0.9
    ) + 
    scale_colour_material_d() +
    ggtitle(.tit, .subt)
}


plot_clim_by_models_lead <- function(.plot_data, .cent, .tit, .subt) {
  
  # Climatologia dos modelos por lead time ---------------------------------------
  ggp_clim_by_models_lead <-
    ggplot(
      .plot_data,
      aes(x = month, y = !!sym(.cent), group = type)
    ) +
    geom_ribbon(aes(
      ymin = ylower,
      ymax = yupper,
      fill = type
    ),
    alpha = 0.3
    ) +
    geom_line(aes(colour = type), size = 1) +
    facet_grid(vars(L), vars(model)) + # ,scales = "free", independent = "y"
    scale_x_discrete(labels = ~ .x %>% str_sub(1, 1)) +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      legend.position = "top",
      legend.direction = "horizontal"
    ) +
    scale_colour_material() +
    ggtitle(.tit, .subt)
  
  ggp_clim_by_models_lead
} 





ggp_climatologia <- function(monthly_data = nmme_cru_basin_clim,
                             var_name = "prec",
                             ibasin = 6,
                             # var_stat <- c("avg", "sd")
                             var_stat = c("med", "mad")) {
  
  
  # para labels no x com a 1a letra do mes
  m <- month.abb %>% set_names(., 1:12)
  cent <- var_stat[[1]]
  # dados
  plot_data <- .tidy_data_for_clim_plots(monthly_data, ibasin, var_name, var_stat)
  
  titulo <- paste0("NMME Climatology: ",
                   "Ensemble ",
                   ifelse(cent == "med", 
                          "median",
                          "mean"
                   )
  )
  subtitulo <- paste0("Bacia Hidrográfica: ", stn_name(code = ibasin))
  
  
  list(
    by_lead_model = plot_clim_by_models_lead(
      plot_data, cent, titulo, subtitulo
    ),
    by_grp_leads = plot_clim_grpleads_by_model(
      plot_data, cent, titulo, subtitulo
    )
  )
}
