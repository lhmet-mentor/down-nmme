
pcks <- c(
  "tidyverse", "here", "HEobs",
  "checkmate", "lubridate",
  "tictoc", "openair", "ggpubr", "ggExtra", "viridis", "see", "ggh4x"
)

easypackages::libraries(pcks)


# devtools::install_github("lhmet-ped/HEobs")

#------------------------------------------------------------------------------
# funcoes auziliares
source("R/utils.R")
source("R/data-proc-rds.R")



# Metadata - codigos bacias e nomes -----------------------------------------
# data_link <- "https://www.dropbox.com/s/d40adhw66uwueet/VazoesNaturaisONS_D_87UHEsDirceuAssis_2018.dat?dl=1"
# qnat_meta <- extract_metadata(file = data_link, informative = TRUE)
# fs::dir_create(here("input/obs/qnat"))
# readr::write_rds(qnat_meta, file = here("input/obs/qnat", "qnat_meta_ons.RDS"))
##
qnat_meta <- readr::read_rds(here("input/obs/qnat", "qnat_meta_ons.RDS"))
glimpse(qnat_meta)
arrange(qnat_meta, estacao_codigo)


(models_summary <- import_bin_file("output/qs/model_counts.qs"))

# para escolher um posto ONS a partir do codONS
top6()



# dados das previsoes climaticas nmme e CRU------------------------------
avg_type <- "weighted" # melhores resultados
extension <- "qs"

nmme_cru_basin_data <- import_bin_file(
  .filename_basin_data(avg_type, extension)
  ) %>%
  dplyr::rename("n_L" = L)

# primeiro modelo
nmme_cru_basin_data[["data"]][[1]] %>%
  select(-S) %>%
  dplyr::mutate(L = factor(trunc(L))) %>%
  group_by(codONS, L, month = month(date)) %>%
  summarise(across(contains("prec"), 
                   list(avg = mean, 
                        med = median,
                        sd = sd,
                        mad = mad
                        )),
            .groups = "drop"
            ) %>% View()

# nmme_cru_basin_data <- nmme_cru_basin_data %>%
#   mutate(data = map(data,
#                     ~ .x %>% mutate(S = NULL)
#   )
# )

# visualizacao de um modelo para uma bacia para 4 meses e todos leads ----------

imodel <- "CanSIPS-IC3"
ibasin <- 6
months_plot <- c(1, 4, 7, 10)

checkmate::assert_choice(imodel, unique(models_summary$modelo))
checkmate::assert_choice(ibasin, unique(qnat_meta$estacao_codigo))

titulo <- paste0("Modelo: ", imodel)
subtitulo <- paste0("Bacia Hidrográfica: ", stn_name(code = ibasin))

d <- dplyr::filter(nmme_cru_basin_data, model == imodel)[["data"]][[1]] %>%
  # filter(year(Sr) %in% 2001) %>%
  dplyr::filter(codONS == ibasin) %>%
  dplyr::mutate(L = factor(trunc(L)), 
                L = ordered(L, levels = sort(unique(L))),
                month = as.integer(lubridate::month(date)),
                month = ordered(month, levels = sort(unique(month))),
                S = NULL) %>%
  tidyr::pivot_wider(
    names_from = "member",
    values_from = "prec_model",
    names_glue = "{.value}_{member}"
  ) %>%
  # ensemble mean and median
  group_by(L, month = as.integer(lubridate::month(date))) %>%
  tidyr::pivot_longer(contains("prec"), names_to = "source", values_to = "value") %>%
  dplyr::mutate(
    source = factor(source)
    #L = ordered(L, levels = sort(unique(L))),
    #month = as.integer(month(date)),
    #month = ordered(month, levels = sort(unique(month)))
  ) %>%
  ungroup()


data_plot <- d %>%
  dplyr::filter(month %in% months_plot)

cli <- filter(data_plot, source == "prec_obs") %>%
  dplyr::group_by(L, month) %>%
  dplyr::summarise(climatology = mean(value), .groups = "drop") 

p <- data_plot %>%
  dplyr::filter(source != "prec_obs") %>%
  ggplot(aes(x = date, y = value, colour = source)) +
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
    data = filter(data_plot, source == "prec_obs"),
    aes(x = date, y = value, colour = source), size = 1
  )

g <- ggplot_build(p)
cols <- unique(g$data[[1]]["colour"])
labs <- data_plot %>%
  dplyr::filter(source != "prec_obs") %>%
  dplyr::pull(source) %>%
  unique() %>%
  as.character()



p <- p +
  scale_colour_manual(
    values = c(cols[[1]], "black"),
    labels = c(labs, "prec_obs")
  ) +
  geom_hline(
    data = cli,
    aes(yintercept = climatology), colour = "black", lty = 2
  ) +
  ggtitle(titulo, subtitulo)
p





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

