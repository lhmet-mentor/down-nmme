
pcks <- c(
  "tidyverse", "here",
  "checkmate", "lubridate",
  "tictoc", "openair", "ggh4x"
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
#11.528 sec elapsed
nmme_cru_basin_data[["data"]][[1]]

(models_summary <- import_bin_file("output/qs/model_counts.qs"))
top6()



## ensemble averages
d <- nmme_cru_basin_data[["data"]][[8]] %>%
  #filter(year(Sr) %in% 2001) %>%
  filter(codONS == 24) %>%
  mutate(L = factor(trunc(L))) %>%
  pivot_wider(names_from = "member",
              values_from = "prec_model",
              names_glue = "{.value}_{member}"
              ) %>%
  pivot_longer(contains("prec"), names_to = "source", values_to = "value") %>%
  mutate(source = factor(source), 
         L = ordered(L, levels = sort(unique(L))),
         month = as.integer(month(date)),
         month = ordered(month, levels = sort(unique(month)))
         ) %>%
  mutate(mes_horiz = paste0("mês: ", month, "|", "Horiz.: ", L),
         mes_horiz = ordered(mes_horiz, levels = unique(mes_horiz))) 
  

data_plot <- d %>%
  filter(month %in% c(1, 4, 7, 10))

p <-  data_plot %>%
  filter(source != "prec_obs") %>%
  ggplot(aes(x = date, y = value, colour = source)) +
  geom_line() +
  facet_grid2(vars(L), vars(month), 
              scales = "free", independent = "y") +
              #scales = "free_y", independent = "y", space = "free_x") +
  #scale_x_date(date_breaks = "6 years", date_labels = "%y") +
  scale_x_date(date_labels = "%y") +
  theme_bw() +
  theme(strip.background = element_blank(), 
        strip.placement = "outside", 
        legend.position = "top",
        legend.direction = "horizontal") +
  geom_line(data = filter(data_plot, source == "prec_obs"),
            aes(x = date, y = value, colour = source), size = 1) 

g <- ggplot_build(p)
cols <- unique(g$data[[1]]["colour"])
labs <- data_plot %>%
  filter(source != "prec_obs") %>% 
  pull(source) %>% unique() %>% as.character()

cli <- filter(data_plot, source == "prec_obs") %>%
  group_by(L, month) %>%
  summarise(climatology = mean(value)) %>%
  ungroup()

p <- p +
  scale_colour_manual(values = c(cols[[1]], "black"),
                      labels = c(labs, "prec_obs")
                      ) + 
  geom_hline(data = cli,
             aes(yintercept=climatology), colour="black", lty = 2)
p
  



