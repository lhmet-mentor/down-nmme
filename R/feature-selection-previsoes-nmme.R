pcks <- c(
  "tidyverse", "here", "HEobs",
  "checkmate", "lubridate",
  "tictoc", "openair", "ggpubr", "ggExtra", "viridis", "see", "ggh4x",
  # "ensemblepp",
  # "ensembleBMA",
  # "ensembleMOS",
  # "downscaleR",
  # "crch",
  # "MBC",
  "DataExplorer",
  "tidymodels"
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
# importa dados combinados das medias dos modelos e da media ensemble
data_nmme_cru_file <- here("output/qs/basin-avgs/weighted/",
                           "nmme-cru-mly-weighted-avg-basins-ons-ens-members-models-ens-mean-prec-1982-2010.qs")
data_nmme_cru <- import_bin_file(data_nmme_cru_file)
range(data_nmme_cru$date)


#------------------------------------------------------------------------------
# Dados de todas previsoes nas colunas
furnas11 <- spread_all_nmme(data_nmme_cru,
                            ibasin = 6,
                            imonth = 1,
                            lead_time = 1,
                            model_exclude = "gfdl_spear"
) %>%
  relocate(date) %>%
  select(-(codONS:month)) %>% 
  drop_na()


#------------------------------------------------------------------------------
# dados com correl significativa

tab_r_p <- filter_by_pval_corr(furnas11[,-1], alpha = 0.11, plot = FALSE)
#filter_by_pval_corr(data_pp, alpha = 0.11, plot = TRUE)

models_nms_rsig <- tab_r_p$member
data_pp_bests <- furnas11 %>%  select(date, obs.mean, all_of(models_nms_rsig)) 

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
X <- furnas11[,-1]
rec <- recipe(obs.mean ~ ., data = X) 
corr_filter <- rec %>%
  step_corr(all_numeric_predictors(), threshold = .35)
filter_obj <- prep(corr_filter, training = X)  
#summary(filter_obj)

# variaveis removidas
tidy(filter_obj, number = 1) %>% pull(terms)
# variaveis mantidas
uncorrelated <- bake(filter_obj, new_data = NULL) %>%
  select(-obs.mean) %>%
  names()
uncorrelated

library(corrplot)
tmwr_cols <- colorRampPalette(c("#91CBD765", "#CA225E"))
bake(filter_obj, new_data = NULL) %>% 
  relocate(obs.mean) %>%
  cor() %>% 
  corrplot(col = tmwr_cols(30), tl.col = "black", method = "ellipse")

# avaliar efeito num modelo linear ---------------------------------------------

set.seed(1)

#furnas11_split <- initial_split(data_pp_bests, prop = 0.7) # BEST
furnas11_split <- initial_split(furnas11, prop = 0.8) 
furnas11_train <- training(furnas11_split)
furnas11_test  <-  testing(furnas11_split)

furnas11_rec <- recipe(obs.mean ~ ., 
                       data = select(furnas11_train, -date)
                       )  %>%
  step_normalize(all_numeric_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = .35) %>%
  step_YeoJohnson(all_numeric_predictors()) 

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(furnas11_rec)

lm_fit <- fit(lm_wflow, furnas11_train)

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


scatterPlot(plot_data, x = "obs_mean", y = "pred", 
            type = "id", linear = TRUE, mod.line = TRUE)


timePlot(plot_data, c("obs_mean", "pred"), 
         group = TRUE,
         ref.x = list(v = min(plot_data$date[plot_data$id == "test"]), 
                      lty = 1, lwd = 2)
)

final_lm_res <- last_fit(lm_wflow, furnas11_split)
fitted_lm_wflow <- extract_workflow(final_lm_res)
collect_metrics(final_lm_res)
collect_predictions(final_lm_res)

