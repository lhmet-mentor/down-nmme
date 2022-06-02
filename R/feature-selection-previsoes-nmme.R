pcks <- c(
  "tidyverse", "here", "HEobs",
  "checkmate", "lubridate",
  "tictoc", "openair", "ggpubr", "ggExtra", "viridis", "see", "ggh4x",
  "ensemblepp",
  "ensembleBMA",
  "ensembleMOS",
  "downscaleR",
  "tidymodels",
  "crch",
  "MBC",
  "DataExplorer" 
)

easypackages::libraries(pcks)

#https://www.machinelearningplus.com/machine-learning/feature-selection/
  
#------------------------------------------------------------------------------
# funcoes auxiliares
source("R/utils.R")
source("R/data-proc-rds.R")
source(here("R/plot-nmme-members-cru-prec-funs.R"))

# para escolher um posto ONS a partir do codONS
top6()

# importa dados combinados das medias dos modelos e da media ensemble-----------
data_nmme_cru_file <- here("output/qs/basin-avgs/weighted/",
                           "nmme-cru-mly-weighted-avg-basins-ons-ens-members-models-ens-mean-prec-1982-2010.qs")
data_nmme_cru <- import_bin_file(data_nmme_cru_file)
range(data_nmme_cru$date)

# nomes dos modelos limpos para usar nas colunas-------------------------------
model_nms <- unique(data_nmme_cru$model)
models_nms_clean <- data.frame(matrix(NA, ncol = length(model_nms))) %>%
  set_names(tolower(model_nms)) %>%
  janitor::clean_names() %>%
  names()

level_key <- setNames(models_nms_clean, model_nms)

# nomes com ponto para juntar com nomes dos modelos para deixar dados em formato
# amplo.
data_nmme_cru <- data_nmme_cru %>%
  dplyr::mutate(model = recode(model, !!!level_key)) %>%
  dplyr::rename_with(~ str_replace_all(.x, "_", "\\.")) %>%
  dplyr::rename_with(~ str_replace_all(.x, "member|model", "m"), -model) %>%
  dplyr::rename_with(~ str_replace_all(.x, "avg", "mean"))



#------------------------------------------------------------------------------
# Dados de todas previsoes nas colunas
#imodel = "cansips_ic3" # 20 membros
#imodel = "gfdl_spear" # 15 membros, 1991-2009
#imodel = "cancm4i" # 10 membros
#imodel = c("cansips_ic3", "cancm4i")
ibasin = 6
imonth = 1
lead_time = 1

function()

data_pp_wide <- data_nmme_cru %>% 
  select(-obs.mean, -climatology) %>%
  dplyr::filter(model != "gfdl_spear") %>%
  dplyr::filter(codONS == ibasin, 
                month(date) == imonth, 
                #model %in% imodel, 
                L == lead_time
  ) %>% #pull(month) %>% unique()
  tidyr::pivot_longer(-c(model:date, month), 
               names_to = "previsao", 
               values_to = "valor"
               )  %>%
  tidyr::unite("forecast", c("model", "previsao")) %>%
  tidyr::pivot_wider(names_from = "forecast", values_from = "valor") %>%
  janitor::remove_empty(which = "cols") 
  
obs_per_basins <- data_nmme_cru %>% 
  dplyr::filter(model != "gfdl_spear") %>%
  dplyr::filter(codONS == ibasin, 
                month(date) == imonth, 
                #model %in% imodel, 
                L == lead_time
  ) %>%
  dplyr::select(codONS, date, obs.mean) %>%
  dplyr::distinct(codONS, date, obs.mean)


data_pp_all <- inner_join(data_pp_wide, obs_per_basins) %>%
  dplyr::relocate(obs.mean, .after = month)
names(data_pp_all)

data_pp_all
#select(data_pp_all, contains("ens.mean"))
#select(data_pp_all, contains("ens.sd"))

# remove variaveis redundantes 
data_pp_all <- data_pp_all %>%
  dplyr::rename("ens.mean" = cancm4i_ens.mean, 
         "ens.sd" = cancm4i_ens.sd) %>%
  dplyr::select(-contains("_ens.mean"), -contains("_ens.sd"))


#-------------------------------------------------------------------------------

data4cor <- data_pp_all %>% select(-c(codONS:month)) 
#plot_correlation(data4cor)

correls <-  cor(data4cor, use = "complete.obs")
cor_obs <- round(as.data.frame(correls)[1], 2)
cor_obs_order <- arrange(cor_obs, desc(abs(obs.mean)))
cor_obs_order %>% slice(-1) %>% head(20)

alpha <- 0.1
res <- corrplot::cor.mtest(
  data4cor,
  conf.level = 1-alpha
)

models_nms_rsig <- names(res$p["obs.mean",][res$p["obs.mean",] <= alpha])
# [1] "obs.mean"           "ens.mean"           "cancm4i_m.3"       
# [4] "cansips_ic3_m.13"   "cansips_ic3_m.20"   "cansipsv2_m.6"     
# [7] "cansipsv2_m.13"     "cmc1_cancm3_m.9"    "cmc2_cancm4_m.mean"
# [10] "cmc2_cancm4_m.1"    "cmc2_cancm4_m.10"   "gem_nemo_m.sd"     
# [13] "gem_nemo_m.6"       "nasa_geoss2s_m.sd"  "nasa_geoss2s_m.4"  
# [16] "ncep_cfsv2_m.3"     "ncep_cfsv2_m.5"     "ncep_cfsv2_m.13"  

is_rsig <- colnames(correls) %in% models_nms_rsig

correls_sig <- correls[is_rsig, is_rsig]


col2 <- colorRampPalette(c('#67001F', '#B2182B', '#D6604D', '#F4A582',
                           '#FDDBC7', '#FFFFFF', '#D1E5F0', '#92C5DE',
                           '#4393C3', '#2166AC', '#053061'))
corrplot::corrplot(correls_sig,
                   p.mat = res$p[is_rsig, is_rsig],
                   method = "color", 
                   type = "upper",
                   #sig.level = c(.001, 0.01, alpha),
                   sig.level = c(.001, 0.01, 0.05),
                   pch.cex = 1.2,
                   insig = "label_sig",
                   pch.col = "green",
                   #order = "hclust",
                   is.corr = FALSE,
                   diag = FALSE,
                   col = rev(col2(30)), 
                   number.cex = 0.7,
                   addCoef.col = 'black'
)
#GGally::ggpairs(data4cor) + theme_bw()

#plot_correlation(data4cor)

my_alpha <- 0.1
models_nms_rsig <- names(res$p["obs.mean",][res$p["obs.mean",] <= my_alpha])


data_pp_bests <- data_pp_all %>%
  #select(codONS:month, all_of(models_nms_rsig)) %>%
  select(codONS:month, all_of(models_nms_rsig)) 


data_pp_bests %>%
  timePlot(., 
           models_nms_rsig, 
         group = TRUE, 
         key.columns = 5,
         ylab = "Prec"
         ) 

rng <- data_pp_bests %>%
  range(na.rm = TRUE)
rng <- c(trunc(rng[1]), ceiling(rng[2]))


data_pp_bests_long <- data_pp_bests %>%
  pivot_longer(cols = -c(codONS:obs.mean), names_to = "previsao", values_to = "value")

data_pp_bests_long %>%
  ggplot(aes(x = date, y = value, color = factor(previsao))) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_fill_material_d() +
  geom_hline(yintercept = mean(data_pp_bests_long$obs.mean))

openair::scatterPlot(data_pp_bests_long, 
                     x = "obs.mean", 
                     y = "value", type = "previsao",
                     linear = TRUE, 
                     mod.line = TRUE,
                     #type = c("model", "codONS")
                     pch = 20, 
                     xlim = c(rng), 
                     ylim = rng
                     
)


# remover colinearidade