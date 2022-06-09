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

## Referencias ------------------------------------------------------------------
# https://www.machinelearningplus.com/machine-learning/feature-selection/
# https://towardsdatascience.com/effective-feature-selection-recursive-feature-elimination-using-r-148ff998e4f7
# https://bookdown.org/max/FES/stroke-preprocessing.html
#  https://github.com/topepo/FES/blob/master/02_Predicting_Risk_of_Ischemic_Stroke/02_02_Preprocessing.R
# https://github.com/FrancisArgnR/R-FeatureSelection-Packages
# https://cran.r-project.org/web/packages/FeatureTerminatoR/vignettes/feature_terminatoR_howto.html
# https://www.tmwr.org/pre-proc-table.html
# https://www.lobdata.com.br/2020/10/13/effective-approach-to-analyze-correlation-coefficients/
#------------------------------------------------------------------------------
# funcoes auxiliares
source(here("R/utils.R"))
source(here("R/data-proc-rds.R"))
source(here("R/aggregate-nmme.R"))
source(here("R/plot-nmme-members-cru-prec-funs.R"))


# para escolher um posto ONS a partir do codONS
top6()

#-------------------------------------------------------------------------------
# fncao para retornmar nome dos modelos com as n maiorres correlacoes com obs
.topn_cor <- function(cor_mat, target = "obs.mean", n, names = FALSE) {
  # cor_mat = correls; target = "obs.mean"; n = 5
  cor_obs <- round(as.data.frame(cor_mat)[target], 2)
  cor_obs_order <- arrange(cor_obs, desc(abs(obs.mean)))
  res <- cor_obs_order %>% slice(-1) %>% head(n) 
  if(names) return(rownames(res))
  res
}

# paleta de cores para correlacao
pal_correlation <- function(n){
  col2 <- colorRampPalette(c('#67001F', '#B2182B', '#D6604D', '#F4A582',
                             '#FDDBC7', '#FFFFFF', '#D1E5F0', '#92C5DE',
                             '#4393C3', '#2166AC', '#053061'))
  rev(col2(n))  
}

#-------------------------------------------------------------------------------
# importa dados combinados das medias dos modelos e da media ensemble
data_nmme_cru_file <- here("output/qs/basin-avgs/weighted/",
                           "nmme-cru-mly-weighted-avg-basins-ons-ens-members-models-ens-mean-prec-1982-2010.qs")
data_nmme_cru <- import_bin_file(data_nmme_cru_file)
range(data_nmme_cru$date)


#------------------------------------------------------------------------------
# Dados de todas previsoes nas colunas
data_pp <- spread_all_nmme(data_nmme_cru, 
                           ibasin = 6, 
                           imonth = 1, 
                           lead_time = 1, 
                           model_exclude = "gfdl_spear"
                           )


#-------------------------------------------------------------------------------
# Selecao das previsoes com r significativa ao n.c 90%
data4cor <- data_pp %>% select(-c(codONS:month)) 
#plot_correlation(data4cor)

correls <-  cor(data4cor, use = "complete.obs")
.topn_cor(correls, n = 5)
.topn_cor(correls, n = 5, names = TRUE)



# teste de significancia da correlacao
alpha <- 0.11 # baixado para incluir membro com maior correl!
res <- corrplot::cor.mtest(
  data4cor,
  conf.level = 1-alpha
)

# cbind(r=cor_obs ,p = round(as.data.frame(res)[1], 2)) %>%
#   arrange(p.obs.mean)

models_nms_rsig <- names(res$p["obs.mean",][res$p["obs.mean",] <= alpha])
# [1] "obs.mean"           "ens.mean"           "cancm4i_m.3"       
# [4] "cansips_ic3_m.13"   "cansips_ic3_m.20"   "cansipsv2_m.6"     
# [7] "cansipsv2_m.13"     "cmc1_cancm3_m.9"    "cmc2_cancm4_m.mean"
# [10] "cmc2_cancm4_m.1"    "cmc2_cancm4_m.10"   "gem_nemo_m.sd"     
# [13] "gem_nemo_m.6"       "nasa_geoss2s_m.sd"  "nasa_geoss2s_m.4"  
# [16] "ncep_cfsv2_m.3"     "ncep_cfsv2_m.5"     "ncep_cfsv2_m.13"  

is_rsig <- colnames(correls) %in% models_nms_rsig
correls_sig <- correls[is_rsig, is_rsig]


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
                   col = pal_correlation(30), 
                   number.cex = 0.7,
                   addCoef.col = 'black'
)
#GGally::ggpairs(data4cor) + theme_bw()

#plot_correlation(data4cor)

#------------------------------------------------------------------------------
# dados com correl significativa
data_pp_bests <- data_pp %>%  select(date, all_of(models_nms_rsig)) 

# data_pp_bests %>%
#   timePlot(., 
#            models_nms_rsig, 
#          group = TRUE, 
#          key.columns = 5,
#          ylab = "Prec"
#          ) 

# visualizacao das series
data_pp_bests_long <- data_pp_bests %>%
  pivot_longer(cols = -c(date:obs.mean),
               names_to = "previsao", 
               values_to = "value")

# data_pp_bests_long %>%
# mutate(
#   sex = fct_reorder(previsao, value, .fun = ~, na.rm = TRUE)
# )

data_pp_bests_long %>%
  ggplot(aes(x = date, y = value, color = factor(previsao))) +
  #geom_point() +
  geom_line(alpha = 0.5) +
  theme_bw() +
  scale_colour_material_d() +
  # gghighlight::gghighlight(
  #   previsao %in% .topn_cor(cor_obs_order, 5),
  #   label_key = previsao, use_direct_label = FALSE
  # ) +
  geom_hline(yintercept = mean(data_pp_bests$obs.mean), linetype = 2) +
  geom_line(data = select(data_pp_bests, date, obs.mean),
            aes(x = date, y = obs.mean), 
            colour = 1, size = 2
  ) 

# unique(data_pp_bests_long$previsao) %>% length()
# 21 previsoes
# dispersao
rng <- data_pp_bests %>%
  select(where(is.numeric)) %>%
  range(na.rm = TRUE)
rng <- c(trunc(rng[1]), ceiling(rng[2]))

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


  


#DataExplorer::plot_histogram(data_pp_bests[,-1], geom_histogram_args = list(bins = 15))
#DataExplorer::plot_qq(data_pp_bests[,-1], by = "obs.mean")

# remover colinearidade--------------------------------------------------------
library(tidymodels)

X <- data_pp_bests %>%
  select(all_of(models_nms_rsig)) %>%
  drop_na()

# X <- data_pp %>%
#   select(-(codONS:month)) %>% 
#   drop_na()


sort(round(abs(cor(X))[-1,1], 2)) #%>% length()

rec <- recipe(obs.mean ~ ., data = X) 
  
corr_filter <- rec %>%
  step_corr(all_numeric_predictors(), threshold = .9)
filter_obj <- prep(corr_filter, training = X)  
#summary(filter_obj)
# variaveis removidas
tidy(filter_obj, number = 1)
# so para ver os dados filtrados
bake(filter_obj, new_data = NULL) %>% names()



# # A tibble: 4 × 2
# terms            id        
# <chr>            <chr>     
#   1 cansips_ic3_m.13 corr_aoUDh
# 2 cancm4i_m.3      corr_aoUDh
# 3 cancm4i_m.5      corr_aoUDh
# 4 cansipsv2_m.6    corr_aoUDh


ggplot(data = X, aes(x = obs.mean)) + 
  geom_histogram(bins = 8, col= "white") 


#-------------------------------------------------------------------------------

furnas11 <- data_pp_bests %>%
  select(date, all_of(models_nms_rsig)) %>%
  drop_na() 

# furnas11 %>% View()
# furnas11 %>% select(ncep_cfsv2_m.5, ncep_cfsv2_m.13) %>% View()

#furnas11_split <- initial_split(furnas11, prop = 0.8)
#furnas11_split <- initial_time_split(furnas11, prop = 0.8) # BEST
furnas11_split <- initial_time_split(furnas11, prop = 0.68)

furnas11_train <- training(furnas11_split)
furnas11_test  <-  testing(furnas11_split)


furnas11_rec <- recipe(obs.mean ~ ., data = select(furnas11_train, -date))  %>%
  step_normalize(all_numeric_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = .9) %>% #BEST
  #step_corr(all_numeric_predictors(), threshold = .7) %>% 
  step_YeoJohnson(all_numeric_predictors()) 

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(furnas11_rec)



# resultados
model_res <- lm_fit %>%
  tune::extract_fit_engine() 

model_res %>% tidy()
model_res %>% glance()


# previsao
#predict(lm_fit, new_data = furnas11_test)




prevs_test <- furnas11_test %>% 
  select(date, obs.mean) %>% 
  bind_cols(predict(lm_fit, furnas11_test)) %>% 
  # Add 95% prediction intervals to the results:
  bind_cols(predict(lm_fit, furnas11_test, type = "pred_int")) %>%
  mutate(id = "test")


prevs_train <- furnas11_train %>% 
  select(date, obs.mean) %>% 
  bind_cols(predict(lm_fit, furnas11_train)) %>% 
  # Add 95% prediction intervals to the results:
  bind_cols(predict(lm_fit, furnas11_train, type = "pred_int")) %>%
  mutate(id = "train")

plot_data <- bind_rows(prevs_train, prevs_test)  %>% 
  janitor::clean_names() %>%
  mutate(id = ordered(id, levels = c("train", "test")))
  
timePlot(plot_data, c("obs_mean", "pred"), 
         group = TRUE,
         ref.x = list(v = min(plot_data$date[plot_data$id == "test"]), 
                      lty = 1, lwd = 2)
        )



final_lm_res <- last_fit(lm_wflow, furnas11_split)
fitted_lm_wflow <- extract_workflow(final_lm_res)

collect_metrics(final_lm_res)
collect_predictions(final_lm_res) #%>% slice(1:5)

#-------------------------------------------------------------------------------
# resample

# furnas11_split <- initial_split(furnas11, prop = 0.8)
# furnas11_train <- training(furnas11_split)
# furnas11_test  <-  testing(furnas11_split)
# 
# folds <- vfold_cv(furnas11_train, v = 3)
# lm_fit_rs <- lm_wflow %>%
# fit_resamples(folds)
