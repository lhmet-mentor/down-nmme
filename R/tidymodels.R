

pcks <- c(
  "tidyverse", "here", "HEobs","magrittr",
  "checkmate", "lubridate",
  "tictoc", "openair", "ggpubr", "ggExtra", "viridis", "see", "ggh4x",
  # "ensemblepp",
  # "ensembleBMA",
  # "ensembleMOS",
  # "downscaleR",
  # "crch",
  # "MBC",
  "embed",
  "bestNormalize",
  "corrplot",
  "DataExplorer",
  "tidymodels"
)
easypackages::libraries(pcks)

#------------------------------------------------------------------------------
# funcoes auxiliares
source(here("R/utils.R"))
source(here("R/data-proc-rds.R"))
source(here("R/aggregate-nmme.R"))
source(here("R/plot-nmme-members-cru-prec-funs.R"))
source(here("R/corr-functions.R"))


# para escolher um posto ONS a partir do codONS
top6()

# fit a workflow on analysis data splits from training set ---------------------
fit_analysis <- function(train_splits, w_flow){
  # train_splits = furnas11_folds$splits
  # w_flow = lm_wflow
  analysis_samples <- map_df(train_splits, 
                             function(x) {
                               analysis(x)
                             }
  )
  # fit metrics 
  #fit(lm_wflow, analysis_samples) %>% broom::glance()
  # model pars
  #fit(lm_wflow, analysis_samples) %>% broom::tidy()
  # 
  analysis_res <- fit(w_flow, analysis_samples) %>% 
    #predict(analysys_samples)
    broom::augment(analysis_samples) %>%
    select(date, obs.mean, .pred) %>%
    mutate(dataset = "analysis")
  
  analysis_res
}

# fit a workflow on testing set ------------------------------------------------
fit_test <- function(data_test, w_flow) {
  # data_test = furnas11_test
  # w_flow = lm_wflow
  fit(w_flow, data_test) %>% # broom::glance()
    # predict(analysys_samples)
    broom::augment(data_test) %>%
    select(date, obs.mean, .pred) %>%
    mutate(dataset = "test")
}


# plot scatterplot of training (analysis and assess splits) and testing--------
scatter_plot_splits <-  function(analysis_df, assess_df, test_df){
  # analysis_df = analysis_res ;assess_df = assess_res; test_df = test_res
  all_data <- bind_rows(analysis_df, assess_df, test_df) 
  
  sct_plot <- all_data %>% 
    ggpubr::ggscatter(x = "obs.mean",
                      y = ".pred", 
                      color = "dataset", 
                      add = "reg.line", 
                      conf.int = TRUE) +
    geom_hline(yintercept = mean(unique(all_data$obs.mean)), col = "gray") +
    facet_wrap(~dataset) +
    ggpubr::stat_regline_equation(aes(color = dataset), 
                                  label.x.npc = 0.05, 
                                  label.y.npc = 0.999
    ) +
    stat_cor(aes(color = dataset), 
             label.x.npc = 0.05, 
             label.y.npc = 0.94
    ) +
    geom_abline(slope = 1, intercept = 0, col = "black") +
    # climatologia
    #+
    tune::coord_obs_pred()
  
  sct_plot
}   



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
  model_exclude = c("gfdl_spear") # 28 anos
  #model_exclude = c("gfdl_spear", "ncep_cfsv2") # para ter max anos (29)
) %>%
  relocate(date) %>%
  select(-(codONS:month)) %>% #nrow() %>%
  drop_na()


# grep("ncep_cfsv2", names(furnas11))
#dim(furnas11 %>% drop_na)



# td <- furnas11 %>%
#   #select(date, contains("mean")) %>%
#   pivot_longer(-(date:obs.mean), names_to = "member", values_to = "forecast") %>%
#   arrange(member) %>%
#   as.data.frame() %>%
#   openair::TaylorDiagram(., 
#                          obs = "obs.mean",
#                          mod = "forecast", 
#                          group = "member"
#                          )

# selecao pela correlacao-------------------------------------------------------
# 15 mais correlacionados preditores
furnas11_bestc <- furnas11[,-1] %>% 
  #filter_by_corr(n = 15) %>% 
  filter_by_pval_corr(alpha = 0.11, plot = FALSE) %>%
  pull(member) %>%
  c("date", "obs.mean", .) %>% 
  magrittr::extract(furnas11, .)

grep("ncep_cfsv2", names(furnas11_bestc))

# furnas11_bestc %>%
#   pivot_longer(-(obs.mean), names_to = "member", values_to = "forecast") %>%
#   arrange(member) %>%
#   as.data.frame() %>%
#   openair::TaylorDiagram(., 
#                          obs = "obs.mean",
#                          mod = "forecast", 
#                          group = "member"
#   )


#-------------------------------------------------------------------------------
# Refs
## validacao cruzada
# https://medium.datadriveninvestor.com/resampling-methods-the-solution-to-small-datasets-5b9e5c390eb5
# resample
set.seed(12)
furnas11_split <- initial_split(furnas11_bestc, prop = 0.7)
# furnas11_split <- initial_split(furnas11, prop = 0.7)
furnas11_train <- training(furnas11_split)
furnas11_test <- testing(furnas11_split)
#
# folds <- vfold_cv(furnas11_train, v = nrow(furnas11_train))
furnas11_folds <- vfold_cv(furnas11_train, v = 5)
# When V = 3, the analysis sets are 2/3 of the training set and
# each assessment set is a distinct 1/3.
# analysis(furnas11_folds$splits[[1]]) %>% dim()
# assessment(furnas11_folds$splits[[1]]) %>% dim()

furnas11_rec <- recipe(obs.mean ~ .,
  data = select(furnas11_train, -date)
) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = .9) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  # step_orderNorm(all_numeric_predictors()) #%>%
  step_pca(all_numeric_predictors(), num_comp = 4)
# step_pls(all_numeric_predictors(), outcome = "obs.mean", num_comp = 4) %>%
# step_umap(all_numeric_predictors(), num_comp = 4) %>%
# step_umap(all_numeric_predictors(), outcome = "obs.mean", num_comp = 4) %>%
# step_orderNorm(all_numeric_predictors()) %>%

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- workflow() %>%
  add_model(lm_model) %>%
  add_recipe(furnas11_rec)

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

lm_fit_rs <- lm_wflow %>%
  fit_resamples(furnas11_folds, control = keep_pred)

# lm_fit_rs$.metrics
collect_metrics(lm_fit_rs, summarize = TRUE)

assess_res <- collect_predictions(lm_fit_rs) %>%
  mutate(.,
    date = furnas11_bestc$date[.row],
    dataset = "assessment"
  ) %>%
  relocate(date)

analysis_res <- fit_analysis(furnas11_folds$splits, lm_wflow)
test_res <- fit_test(furnas11_test, lm_wflow)

scatter_plot_splits(analysis_res, assess_res, test_res)

# diferenciar folds por símbolos



# random forest ----------------------------------------------------------------
#set.seed(2907)

run_fun <- function(){
furnas11_split <- initial_split(furnas11_bestc, prop = 0.7)
# furnas11_split <- initial_split(furnas11, prop = 0.7)
furnas11_train <- training(furnas11_split)
furnas11_test <- testing(furnas11_split)
#
# folds <- vfold_cv(furnas11_train, v = nrow(furnas11_train))
furnas11_folds <- vfold_cv(furnas11_train, v = 5)
# When V = 3, the analysis sets are 2/3 of the training set and
# each assessment set is a distinct 1/3.
# analysis(furnas11_folds$splits[[1]]) %>% dim()
# assessment(furnas11_folds$splits[[1]]) %>% dim()

furnas11_rec <- recipe(obs.mean ~ .,
                       data = select(furnas11_train, -date)
) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = .9) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  # step_orderNorm(all_numeric_predictors()) #%>%
  step_pca(all_numeric_predictors(), num_comp = 4)
# step_pls(all_numeric_predictors(), outcome = "obs.mean", num_comp = 4) %>%
# step_umap(all_numeric_predictors(), num_comp = 4) %>%
# step_umap(all_numeric_predictors(), outcome = "obs.mean", num_comp = 4) %>%
# step_orderNorm(all_numeric_predictors()) %>%

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- workflow() %>%
  add_model(lm_model) %>%
  add_recipe(furnas11_rec)

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

lm_fit_rs <- lm_wflow %>%
  fit_resamples(furnas11_folds, control = keep_pred)

# lm_fit_rs$.metrics
collect_metrics(lm_fit_rs, summarize = TRUE)

assess_res <- collect_predictions(lm_fit_rs) %>%
  mutate(.,
         date = furnas11_bestc$date[.row],
         dataset = "assessment"
  ) %>%
  relocate(date)

analysis_res <- fit_analysis(furnas11_folds$splits, lm_wflow)
test_res <- fit_test(furnas11_test, lm_wflow)


plt <- scatter_plot_splits(analysis_res, assess_res, test_res)
plt

}
 

resultados_l <- map(1:30, function(i) run_fun())

#resultados_l[[1]]
#resultados_l[[30]]

resultados_l

