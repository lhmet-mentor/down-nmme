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

filter_by_corr<- function(data, n = 10){
  data %>% 
    cor() %>%
    #abs() %>%
    round(., 2) %>% 
    as_tibble() %>% 
    mutate(member = names(.)) %>%
    select(member, cor_obs = obs.mean) %>% 
    slice(-1) %>%
    arrange(desc(abs(cor_obs))) %>%
    slice(1:n)  
}

# funcao para retornar correlacao e significancia ou plotar matriz de cor
filter_by_pval_corr <- function(data_pp, alpha = 0.11, plot = FALSE) {
  # Selecao das previsoes com r significativa ao n.c 90%
  data4cor <- data_pp #%>% select(-c(codONS:month))
  # plot_correlation(data4cor)
  
  correls <- cor(data4cor, use = "complete.obs")
  .topn_cor(correls, n = 5)
  .topn_cor(correls, n = 5, names = TRUE)
  
  # teste de significancia da correlacao
  # alpha <- 0.11 # baixado para incluir membro com maior correl!
  res <- corrplot::cor.mtest(
    data4cor,
    conf.level = 1 - alpha
  )
  
  
  cor_vals <- as.data.frame(correls)[1]
  p_vals <- as.data.frame(res)[1]
  
  r_p_sign <- tibble(
    member = rownames(cor_vals),
    r = cor_vals[[1]],
    p = p_vals[[1]]
  ) %>%
    filter(p <= alpha, member != "obs.mean") %>%
    arrange(p)
  
  
  if(!plot) return(r_p_sign)
  
  models_nms_rsig <- names(res$p["obs.mean", ][res$p["obs.mean", ] <= alpha])
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
                     # sig.level = c(.001, 0.01, alpha),
                     sig.level = c(.001, 0.01, 0.05),
                     pch.cex = 1.2,
                     insig = "label_sig",
                     pch.col = "green",
                     # order = "hclust",
                     is.corr = FALSE,
                     diag = FALSE,
                     col = pal_correlation(30),
                     number.cex = 0.7,
                     addCoef.col = "black"
  )
  
  # GGally::ggpairs(data4cor) + theme_bw()
}




omega <- function(ens_mean, ens_members) {
  
  ens_members <- data.frame(1:10, 1:10, 1:10, 1:10)
  
  # converte para data.frame
  ens_members <- as.data.frame(ens_members)
  ens_members <- as.data.frame(scale(ens_members, center = TRUE, scale = TRUE))
  
  # converte para vetor
  if (!missing(ens_mean)) {
    ens_mean <- c(t(ens_mean))
    ens_mean <- (ens_mean - mean(ens_mean))/sd(ens_mean)
  }

  # caso comparacao do ensemble com a media do proprio ensemble
  if (missing(ens_mean) && ncol(ens_members) > 1) {
    # media do ensemble
    ens_mean <- rowMeans(ens_members)
    m <- ncol(ens_members)
  }

  # caso comparacao de obs e pred
  if (!missing(ens_mean) && ncol(ens_members) == 1) {
    m <- 2
  }

  # caso comparacao de um ensemble com uma obs
  if (!missing(ens_mean) && ncol(ens_members) > 1) {
    m <- ncol(ens_members)
  }

  sigma2_b <- var(c(t(ens_mean)))
  sigma2 <- var(c(t(ens_members)))
  
  omega_ind <- (m * sigma2_b - sigma2) / ((m - 1) * sigma2)
  omega_ind
}

ens_d <- data.frame(1:10, 1:10, 1:10, 1:10)
omega(ens_members = ens_d)

# X <- scale(data.frame(cos(0:9),1:10, 10:1), center = TRUE, scale = TRUE)
# matplot(X, type = "l")
# omega(ens_members = X)
# 
# # membros
# i <- 2:8
# # tempos
# j <- 1:10
# delta_tau1 <- seq(0, 2*pi, by  = pi/4)
# n <- 10
# 
# tab <- expand.grid(i, j, delta_tau1) %>%
#   as_tibble() %>%
#   set_names(c("i", "j", "delta_tau1")) %>%
#   arrange(i)
# 
# x_ij <- map_dbl(1:nrow(tab), 
#  function(ir){
#    # ir <- 1
#    i <- tab[["i"]][ir]
#    j <- tab[["j"]][ir]
#    delta_tau1 <- tab[["delta_tau1"]][ir]
#    
#    x_ij <- sin(2*pi*j/n - (i-1)*delta_tau1)
#    x_ij
# })
# 
# tab <- mutate(tab,
#               x_ij, 
#               tau_id = rep(1:data.table::uniqueN(tab$delta_tau1), each = unique(table(tab$delta_tau1)))
#               )
# 
# # 2 membros
# X <- filter(tab, i == 2) %>%
#   pivot_wider(names_from = "i", values_from = "x_ij")
#   
# 
# ggplot(data = tab, aes(x = delta_tau1, y = )) 





