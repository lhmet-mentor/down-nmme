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
# downscaleR


# biasCorrection1D <- function(o, p, s,
#                              method, 
#                              scaling.type,
#                              fitdistr.args,
#                              precip, 
#                              pr.threshold,
#                              n.quantiles,
#                              extrapolation, 
#                              theta,
#                              detrend,
#                              isimip3.args,
#                              parallel = FALSE,
#                              max.ncores = 16,
#                              ncores = NULL) {
#   parallel.pars <- transformeR::parallelCheck(parallel, max.ncores, ncores)
#   mapply_fun <- transformeR::selectPar.pplyFun(parallel.pars, .pplyFUN = "mapply")
#   if (parallel.pars$hasparallel) on.exit(parallel::stopCluster(parallel.pars$cl))
#   if (method == "delta") {
#     mapply(downscaleR:::delta, o, p, s)
#   } else if (method == "scaling") {
#     mapply_fun(downscaleR:::scaling, o, p, s, MoreArgs = list(scaling.type = scaling.type))
#   } else if (method == "eqm") {
#     suppressWarnings(
#       mapply_fun(eqm, o, p, s, MoreArgs = list(precip, pr.threshold, n.quantiles, extrapolation))
#     )
#   } else if (method == "pqm") {
#     suppressWarnings(
#       mapply_fun(pqm, o, p, s, MoreArgs = list(fitdistr.args, precip, pr.threshold))
#     )
#   } else if (method == "gpqm") {
#     mapply_fun(gpqm, o, p, s, MoreArgs = list(precip, pr.threshold, theta))
#   } else if (method == "mva") {
#     mapply_fun(mva, o, p, s) 
#   } else if (method == "variance") {
#     mapply_fun(variance, o, p, s, MoreArgs = list(precip))
#   } else if (method == "loci") {
#     mapply_fun(loci, o, p, s, MoreArgs = list(precip, pr.threshold))
#   } else if (method == "ptr") {
#     mapply_fun(ptr, o, p, s, MoreArgs = list(precip))
#   } else if (method == "dqm") {
#     mapply_fun(dqm, o, p, s, MoreArgs = list(precip, pr.threshold, n.quantiles, detrend))
#   } else if (method == "qdm") {
#     mapply_fun(qdm, o, p, s, MoreArgs = list(precip, pr.threshold, n.quantiles))
#   } else if (method == "isimip3") {
#     mapply_fun(isimip3, o, p, s, MoreArgs = isimip3.args) #this method is in a separate file
#   }
#   #INCLUIR AQUI METODOS NUEVOS
# }