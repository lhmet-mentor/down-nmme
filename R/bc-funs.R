# Bias correction functions

## Simple bias adjustment technique described in Torralba et al. (2017). 
## The adjusted forecasts have an equivalent standard deviation and mean to
## that of the reference dataset.
## Adaptado de CSTools:::.sbc

sbc <- function (var_obs, var_exp, var_cor = NULL, na.rm = FALSE) {
  #nmembers <- dim(var_exp)["member"][]
  #ntime <- dim(var_exp)["sdate"][]
  ntime <- length(var_exp)
  corrected <- NA * var_exp
  if (is.null(var_cor)) {
    for (t in 1:ntime) {
      sd_obs <- sd(var_obs[-t], na.rm = na.rm)
      sd_exp <- sd(var_exp[, -t], na.rm = na.rm)
      clim_exp <- mean(var_exp[, -t], na.rm = na.rm)
      clim_obs <- mean(var_obs[-t], na.rm = na.rm)
      corrected[, t] <- ((var_exp[, t] - clim_exp) * (sd_obs/sd_exp)) + 
        clim_obs
      names(dim(corrected)) <- c("member", "sdate")
    }
  }
  else {
    sd_obs <- sd(var_obs, na.rm = na.rm)
    sd_exp <- sd(var_exp, na.rm = na.rm)
    clim_exp <- mean(var_exp, na.rm = na.rm)
    clim_obs <- mean(var_obs, na.rm = na.rm)
    corrected <- ((var_cor - clim_exp) * (sd_obs/sd_exp)) + 
      clim_obs
    #names(dim(corrected)) <- c("member")
  }
  return(corrected)
}


#-------------------------------------------------------------------------------
# variance inflation
# x <- data.frame(obs=treino$obs_avg,
#                 prev = treino$model_avg,
#                 ens = treino$ens_avg,
#                 prev_vi = variance_inflation()
#                 )
# matplot(x, type = "l", lwd = c(2, 1, 1, 2))
# sd(x$obs)
# sd(x$prev_vi)
# cor(x)
# 
# CSTools:::.calc.fc.quant
# CSTools:::.calc.fc.quant(fc = fc, na.rm = na.rm)
# 
# CSTools:::.calc.obs.fc.quant
# quant.obs.fc.tr <- CSTools:::.calc.obs.fc.quant(obs = obs.tr, 
#                                                 fc = fc.tr, 
#                                                 na.rm = na.rm)
# 
# CSTools:::.calc.evmos.par
# init.par <- c(.calc.evmos.par(quant.obs.fc.tr, 
#                               na.rm = na.rm))
# 
# CSTools:::.correct.evmos.fc
# .correct.evmos.fc(fc.ev , 
#                   init.par, 
#                   na.rm = na.rm)

variance_inflation <- function(
    obs = treino$obs_avg,
    prev = treino$model_avg,
    ens = treino$ens_avg,
    ens_sd = treino$ens_sd,
    prev_new = NULL,
    ens_new = NULL
){
  
  sigma_ref <- sd(obs, na.rm = TRUE)
  sigma_em <- sd(ens, na.rm = TRUE)
  #sigma_e <- sd(prev, na.rm = TRUE)
  sigma_e <- mean(ens_sd, na.rm = TRUE)
  
  pho <- cor(obs, ens, use = "complete.obs")
  alpha <- abs(pho) * sigma_ref/sigma_em
  beta <- ((1 - pho^2)^0.5) * (sigma_ref/sigma_e)
  
  if(is.null(prev_new)){
    z <- prev - ens
    y <- alpha * ens + beta * z
    return(y)  
  }
  z <- prev_new - ens_new
  y <- (alpha * ens_new) + (beta * z)
  y
}

# ---------------------------------------------------------------------------
# Model output statist (deterministic) 
MOS <- function(obs = treino$obs_avg,
                prev = treino$ens_avg,
                prev_new = teste$ens_avg
){
  dados_treino <- tibble::tibble(obs, prev)
  reg <- lm(obs ~ prev, data = dados_treino)
  
  # broom::tidy(reg)
  # broom::glance(reg)
  
  if(is.null(prev_new)){
    return(broom::augment(reg)$.fitted)
  }
  
  modelr::add_predictions(
    data = tibble(prev = prev_new), 
    model = reg
    )[["pred"]][[1]]
}  
  
# ---------------------------------------------------------------------------
# Model output statist (deterministic) 
  
NGR2 <- function(obs = treino$obs_avg,
                ensmean = treino$ens_avg,
                enssd = treino$ens_sd,
                ensmean_new = teste$ens_avg,
                enssd_new = teste$ens_sd
){
  dados_treino <- tibble::tibble(obs, ensmean, enssd)
  ngr2_reg <- crch::crch(obs ~ ensmean|I(enssd^2), 
                   data = dados_treino,
                   link.scale = "quad",
                   type = "crps"
                   )
    #coef(ngr2_reg)
  #summary(ngr2_reg)
  if(is.null(ensmean_new)){
    as.vector(predict(ngr2_reg))
  }
  
  
  mean_NGR2 <- predict(ngr2_reg,
                       newdata = tibble::tibble(ensmean = ensmean_new,
                                                enssd = enssd_new),
                       type = "location")
  # sd_NGR2 <- predict(ngr2_reg,
  #                      newdata = tibble::tibble(ensmean = ensmean_new,
  #                                               enssd = enssd_new),
  #                      type = "scale")
  
  as.vector(mean_NGR2)
  
} 
  
  
#funcoes do pacote MBC -----------------------------------------------
# Sample CanESM2 (T63 grid) and CanRCM4 (0.22-deg grid) data (122.5 deg W, 50 deg N).

# a list of with elements consisting of:
#   
#   gcm.c	
# matrix of CanESM2 variables for the calibration period.
# 
# gcm.p	
# matrix of CanESM2 variables for the validation period.
# 
# rcm.c	
# matrix of CanRCM4 variables for the calibration period.
# 
# rcm.p	
# matrix of CanRCM4 variables for the validation period.
# 
# ratio.seq	
# vector of logical values indicating if samples are of a ratio quantity.
# 
# trace	
# numeric values indicating trace thresholds for each ratio quantity.
#
# pr: precipitation (mm day-1) 
# tas: average surface temperature (deg. C)
# dtr: diurnal temperature range (deg. C)
# sfcWind: surface wind speed (m s-1)
# ps: surface pressure (ps)
# huss: surface specific humidity (kg kg-1)
# rsds: surface downwelling shortwave radiation (W m-2)
# rlds: surface downwelling longwave radiation (W m-2)
#



# print(load("input/cccma.RData"))
# str(cccma)
# str(cccma[[1]])
# data(cccma)
# set.seed(1)
# 
# # Univariate quantile mapping
# data_pp_1model
# fit.qdm <- QDM(
#     o.c = data_pp_1model$obs_avg[1:27], 
#     m.c = data_pp_1model$model_avg[1:27],
#     m.p = data_pp_1model$model_avg[21:29], 
#     ratio = FALSE,
#     trace = Inf
#     )[[2]] 
# 
# matplot(data.frame(obs = 
#                      data_pp_1model$model_avg[21:29],
#                    prev = fit.qdm
#                    ), type = "l"
#         )

#! QDM NAO FUNCIONA COM LOO!

# ## Not run: 
# data(cccma)
# set.seed(1)
# 
# # Univariate quantile mapping
# qdm.c <- cccma$gcm.c*0
# qdm.p <- cccma$gcm.p*0
# for(i in seq(ncol(cccma$gcm.c))){
#   fit.qdm <- QDM(o.c=cccma$rcm.c[,i], m.c=cccma$gcm.c[,i],
#                  m.p=cccma$gcm.p[,i], ratio=cccma$ratio.seq[i],
#                  trace=cccma$trace[i])
#   qdm.c[,i] <- fit.qdm$mhat.c
#   qdm.p[,i] <- fit.qdm$mhat.p
# }
# 
# # Multivariate MBCp bias correction
# fit.mbcp <- MBCp(o.c=cccma$rcm.c, m.c=cccma$gcm.c,
#                  m.p=cccma$gcm.p, ratio.seq=cccma$ratio.seq,
#                  trace=cccma$trace)
# mbcp.c <- fit.mbcp$mhat.c
# mbcp.p <- fit.mbcp$mhat.p
# 
# # Multivariate MBCr bias correction
# fit.mbcr <- MBCr(o.c=cccma$rcm.c, m.c=cccma$gcm.c,
#                  m.p=cccma$gcm.p, ratio.seq=cccma$ratio.seq,
#                  trace=cccma$trace)
# mbcr.c <- fit.mbcr$mhat.c
# mbcr.p <- fit.mbcr$mhat.p
# 
# # Multivariate MBCn bias correction
# fit.mbcn <- MBCn(o.c=cccma$rcm.c, m.c=cccma$gcm.c,
#                  m.p=cccma$gcm.p, ratio.seq=cccma$ratio.seq,
#                  trace=cccma$trace)
# mbcn.c <- fit.mbcn$mhat.c
# mbcn.p <- fit.mbcn$mhat.p
# colnames(mbcn.c) <- colnames(mbcn.p) <-
#   colnames(cccma$rcm.c)
# 
# # Correlation matrices (Pearson and Spearman)
# # MBCp
# dev.new()
# par(mfrow=c(2, 2))
# plot(c(cor(cccma$rcm.c)), c(cor(qdm.c)), col='black',
#      pch=19, xlim=c(-1, 1), ylim=c(-1, 1),
#      xlab='CanRCM4', ylab='CanESM2 MBCp',
#      main='Pearson correlation\nMBCp calibration')
# abline(0, 1)
# grid()
# points(c(cor(cccma$rcm.c)), c(cor(mbcp.c)), col='red')
# plot(c(cor(cccma$rcm.p)), c(cor(qdm.p)),
#      col='black', pch=19, xlim=c(-1, 1), ylim=c(-1, 1),
#      xlab='CanRCM4', ylab='CanESM2 MBCp',
#      main='Pearson correlation\nMBCp evaluation')
# abline(0, 1)
# grid()
# points(c(cor(cccma$rcm.p)), c(cor(mbcp.p)), col='red')
# plot(c(cor(cccma$rcm.c, m='s')), c(cor(qdm.c, m='s')),
#      col='black', pch=19, xlim=c(-1, 1), ylim=c(-1, 1),
#      xlab='CanRCM4', ylab='CanESM2 MBCp',
#      main='Spearman correlation\nMBCp calibration')
# abline(0, 1)
# grid()
# points(c(cor(cccma$rcm.c, m='s')), c(cor(mbcp.c, m='s')),
#        col='red')
# plot(c(cor(cccma$rcm.p, m='s')), c(cor(qdm.p, m='s')),
#      col='black', pch=19, xlim=c(-1, 1), ylim=c(-1, 1),
#      xlab='CanRCM4', ylab='CanESM2 MBCp',
#      main='Spearman correlation\nMBCp evaluation')
# abline(0, 1)
# grid()
# points(c(cor(cccma$rcm.p, m='s')), c(cor(mbcp.p, m='s')),
#        col='red')
# 
# # MBCr
# dev.new()
# par(mfrow=c(2, 2))
# plot(c(cor(cccma$rcm.c)), c(cor(qdm.c)), col='black',
#      pch=19, xlim=c(-1, 1), ylim=c(-1, 1),
#      xlab='CanRCM4', ylab='CanESM2 MBCr',
#      main='Pearson correlation\nMBCr calibration')
# abline(0, 1)
# grid()
# points(c(cor(cccma$rcm.c)), c(cor(mbcr.c)), col='blue')
# plot(c(cor(cccma$rcm.p)), c(cor(qdm.p)),
#      col='black', pch=19, xlim=c(-1, 1), ylim=c(-1, 1),
#      xlab='CanRCM4', ylab='CanESM2 MBCr',
#      main='Pearson correlation\nMBCr evaluation')
# abline(0, 1)
# grid()
# points(c(cor(cccma$rcm.p)), c(cor(mbcr.p)), col='blue')
# plot(c(cor(cccma$rcm.c, m='s')), c(cor(qdm.c, m='s')),
#      col='black', pch=19, xlim=c(-1, 1), ylim=c(-1, 1),
#      xlab='CanRCM4', ylab='CanESM2 MBCr',
#      main='Spearman correlation\nMBCr calibration')
# abline(0, 1)
# grid()
# points(c(cor(cccma$rcm.c, m='s')), c(cor(mbcr.c, m='s')),
#        col='blue')
# plot(c(cor(cccma$rcm.p, m='s')), c(cor(qdm.p, m='s')),
#      col='black', pch=19, xlim=c(-1, 1), ylim=c(-1, 1),
#      xlab='CanRCM4', ylab='CanESM2 MBCr',
#      main='Spearman correlation\nMBCr evaluation')
# abline(0, 1)
# grid()
# points(c(cor(cccma$rcm.p, m='s')), c(cor(mbcr.p, m='s')),
#        col='blue')
# 
# # MBCn
# dev.new()
# par(mfrow=c(2, 2))
# plot(c(cor(cccma$rcm.c)), c(cor(qdm.c)), col='black',
#      pch=19, xlim=c(-1, 1), ylim=c(-1, 1),
#      xlab='CanRCM4', ylab='CanESM2 MBCn',
#      main='Pearson correlation\nMBCn calibration')
# abline(0, 1)
# grid()
# points(c(cor(cccma$rcm.c)), c(cor(mbcn.c)), col='orange')
# plot(c(cor(cccma$rcm.p)), c(cor(qdm.p)),
#      col='black', pch=19, xlim=c(-1, 1), ylim=c(-1, 1),
#      xlab='CanRCM4', ylab='CanESM2 MBCn',
#      main='Pearson correlation\nMBCn evaluation')
# abline(0, 1)
# grid()
# points(c(cor(cccma$rcm.p)), c(cor(mbcn.p)), col='orange')
# plot(c(cor(cccma$rcm.c, m='s')), c(cor(qdm.c, m='s')),
#      col='black', pch=19, xlim=c(-1, 1), ylim=c(-1, 1),
#      xlab='CanRCM4', ylab='CanESM2 MBCn',
#      main='Spearman correlation\nMBCn calibration')
# abline(0, 1)
# grid()
# points(c(cor(cccma$rcm.c, m='s')), c(cor(mbcn.c, m='s')),
#        col='orange')
# plot(c(cor(cccma$rcm.p, m='s')), c(cor(qdm.p, m='s')),
#      col='black', pch=19, xlim=c(-1, 1), ylim=c(-1, 1),
#      xlab='CanRCM4', ylab='CanESM2 MBCn',
#      main='Spearman correlation\nMBCn evaluation')
# abline(0, 1)
# grid()
# points(c(cor(cccma$rcm.p, m='s')), c(cor(mbcn.p, m='s')),
#        col='orange')
# 
# # Pairwise scatterplots
# dev.new()
# pairs(cccma$gcm.c, main='CanESM2 calibration', col='#0000001A')
# dev.new()
# pairs(cccma$rcm.c, main='CanRCM4 calibration', col='#0000001A')
# dev.new()
# pairs(qdm.c, main='QDM calibration', col='#0000001A')
# dev.new()
# pairs(mbcp.c, main='MBCp calibration', col='#FF00001A')
# dev.new()
# pairs(mbcr.c, main='MBCr calibration', col='#0000FF1A')
# dev.new()
# pairs(mbcn.c, main='MBCn calibration', col='#FFA5001A')
# 
# # Energy distance skill score relative to univariate QDM
# escore.qdm <- escore(cccma$rcm.p, qdm.p, scale.x=TRUE)
# escore.mbcp <- escore(cccma$rcm.p, mbcp.p, scale.x=TRUE)
# escore.mbcr <- escore(cccma$rcm.p, mbcr.p, scale.x=TRUE)
# escore.mbcn <- escore(cccma$rcm.p, mbcn.p, scale.x=TRUE)
# 
# cat('ESS (MBCp):', 1-escore.mbcp/escore.qdm, '\n')
# cat('ESS (MBCr):', 1-escore.mbcr/escore.qdm, '\n')
# cat('ESS (MBCn):', 1-escore.mbcn/escore.qdm, '\n')
