
# estimativa da resolução-------------------------------------------------------
res_estim <- function(grid){
  xy <- dplyr::distinct(grid, X, Y)
  dx <- unique(diff(unique(sort(xy$X))))
  dy <- unique(diff(unique(sort(xy$Y))))  
  c(dx, dy)
}

# build raster from dataframe with xyz -----------------------------------------
raster_from_points <- function(datagrid, prj = "+proj=longlat +datum=WGS84"){
  # datagrid = ens_data[["data"]][[1]]
  mat <- datagrid %>%
    as.matrix()
  
  #tic()
  #r <- raster::rasterFromXYZ(mat, res = res_estim(datagrid))
  r <- terra::rast(mat, type = "xyz")
  terra::crs(r) <- prj 
  #toc()
  
  r
}

# basin averages over pols -----------------------------------------------------
basin_average <- function(datagrid, pols = pols_inc_sp, raster = FALSE){
  # datagrid = ens_data[["data"]][[1]]; summary(datagrid); pols = pols_inc_sp
  
  r <- raster_from_points(datagrid)

  
  if(raster){
    # demora demaiiiiisssssssssss!
    #tic()
    avg_basin <- raster::extract(
      raster::stack(r),
      pols,
      weights = TRUE,
      normalizeWeights = TRUE,
      fun = mean
    ) %>% tibble::as_tibble()
    #toc()
    # 5s
    
    # tic()
    # avg_basin2 <- terra::extract(
    #   r,
    #   terra::vect(pols),
    #   fun = mean,
    #   exact = TRUE
    #   #method = "bilinear"
    # )[, "prec_ensmean"]
    # toc()
    #check <- tibble(avg_basin, avg_basin2, dif = avg_basin2-avg_basin)
    #tail(check)
    
  } else {
    #tic()
    avg_basin <- terra::extract(
      r,
      terra::vect(pols),
      fun = mean,
      touches = TRUE,
      method = "bilinear"
    ) %>%
      dplyr::select(-ID) %>%
      tibble::as_tibble()
    #toc()
    # 0.4 s
    
  }
  
  tibble(codONS = pols$codONS, avg_basin)
}


# Constroi nome para os arquivos das medias nas bacias por S e L-----------------
basin_avg_file_name <- function(slm, weight_mean = FALSE, .format = c("qs", "RDS")) {
  # slm <- iSLM; weight_mean = FALSE; .format = c("qs")
  
  fname <- paste0(
    c("", "S", "L"),
    c(slm$model, format(slm$S, "%Y%m%d"), slm$L)
  ) %>%
    paste(collapse = "_") 
  
  if(weight_mean) {
    return(paste0(fname, glue::glue("_basin-weigthed-avg.{.format}")))
  }
  
  paste0(fname, glue::glue("_basin-arithmetic-avg.{.format}"))
  
}



## definicao/criacao do dir de saida das medias espaciais ---------------------
output_path_basin_avgs <- function(file, w_mean, .dest_path, .format = c("qs", "RDS")){
  # file = file_model; w_mean = TRUE; .dest_path = here("output/qs/basin-avgs"); .format = "qs"
  imodel <- fs::path_file(file) %>%
    stringr::str_replace("ensemble-", "") %>%
    stringr::str_replace(glue::glue("\\.{.format}"), "") %>%
    stringr::str_replace("-mean|-median|-identity", "")
  
  out_path <- ifelse(w_mean, 
                      here(.dest_path, "weighted", imodel), 
                      here(.dest_path, "arithmetic", imodel)
  )
  
  if(!fs::dir_exists(out_path)) fs::dir_create(out_path)
  out_path
}



## medias espaciais ------------------------------------------------------------
basin_avg_model <- function(file_model, 
                            pols_sp = pols_inc_sp, 
                            weighted_mean = FALSE,
                            dest_path = here("output/{rds,qs}/basin-avgs"),
                            format = c("qs", "rds")
                            ) {
  # file_model <- ens_files[1]; pols_sp = pols_inc_sp; weighted_mean = TRUE;dest_path = here("output/qs/basin-avgs"); format = "qs" 
  ens_data <- import_bin_file(file_model)
  
  
  # 28 sec elapsed
  # 7.127 sec elapsed

  ## definicao/criacao do dir de saida das medias espaciais
  dest_path <- output_path_basin_avgs(file_model, 
                                      weighted_mean, 
                                      .dest_path = dest_path,
                                      .format = format
                                      )
    
  ## caso incompleto de NCEP-CFSv2, CMC2-CanCM4
  # ens_data <- ens_data %>%
  #   dplyr::filter(S >= lubridate::as_date("1997-07-01"))
  #   dplyr::filter(S >= lubridate::as_date("1994-07-01"))
  #  dplyr::filter(S >= lubridate::as_date("2005-03-01"))
    
    
  ens_data <- ens_data %>%
    dplyr::mutate(S = lubridate::as_date(S)) %>%
    dplyr::group_by(model, S, L) %>%
    tidyr::nest() %>%
    ungroup()

  
  
  # tic()
  # basin_average(datagrid = ens_data[["data"]][[1]])
  # diff(unique(ens_data$S)) # 30 dias entre as inicializações
  # length(unique(ens_data$S))/12 # 30 anos

  # toc()
  # 5.526 sec elapsed
  # length(ens_data[["data"]]) * 5/3600
  # [1] 6.6 horas

  tic()
  
  #model_basin_avg_files <- purrr::map(
  model_basin_avg_files <- parallel::mclapply(
    1:nrow(ens_data),
    #1:12,
    function(isl) {
      # isl = 1
      
      iSLM <- select(ens_data, S:model)[isl, ]
      basin_file <- basin_avg_file_name(slm = iSLM, 
                                        weight_mean = weighted_mean,
                                        .format = format)
      basin_file_path <- here(dest_path, basin_file)
      
      if(fs::file_exists(basin_file_path)) {
        #message("file already exists ... ", path_file(basin_file_path), "\n")
        return(basin_file_path)
      }
      
      bas_avg <- basin_average(
        datagrid = ens_data[["data"]][[isl]], 
        pols = pols_sp, 
        raster = weighted_mean
        #raster = FALSE
      )
      
      gc()
      basin_data <- dplyr::bind_cols(iSLM, bas_avg)
      export_bin_file(basin_data, basin_file_path)
      
      #assert_file_exists(basin_file_path)
      message("saving ... ", path_file(basin_file_path), "\n")
      basin_file_path
      
    }, mc.cores = parallel::detectCores()-1
    )

  toc()
  # 35 s com raster
  # 21 s com terra e furrr
  # 2.7 sec com terra e map
  gc()
  model_basin_avg_files
}


# dates from rds files with basin averages ------------------------------------
dates_from_model_rds_files <- function(dir_model_rds){
  x <- fs::dir_ls(dir_model_rds) %>%
    fs::path_file() %>%
    str_extract_all("S[0-9]{8}", simplify = TRUE) %>%
    as.Date("S%Y%m%d")  
  x
}
# imodel <- "CanCM4i"
# rds_dts <- dates_from_model_rds_files(here("output/rds/basin-avgs", imodel))
# range(rds_dts)



# agrupando dados por mes de inicialização(S) e leadtime (L)
# resulta na coluna 'data' com os pontos de grade do domínio
# cada linha corresponde a previsão de um membro para um mês de inicialização
# 456 (varia por modelo) meses x 456 linhas
# dimensions : 76, 56, 4256, 1  (nrow, ncol, ncell, nlayers)

# ATENCAO: NAO HÀ PREVISOES INICIADAS EM FEVEREIRO???
# if(check_ref_months){
#
#   out <- ens_data %>%
#     dplyr::mutate(#S = lubridate::as_date(S),
#       year = lubridate::year(S)
#       #month = lubridate::month(S)
#       ) %>%
#     select(-data) %>%
#     group_by(S) %>%
#     #summarise(nL = length(L)) %>% View()
#     mutate(model = imodel)
#
#     return(out)
# }


# -----------------------------------------------------------------------------
# Media espacial na area das bacias ONS para os dados de prec do CRU
basin_average_cru <- function(ncfile_obs = obs_nc_file,
                              vname = "pre",
                              prj = "+proj=longlat +datum=WGS84",
                              pols = pols_inc_sp, 
                              weighted_mean = TRUE) {
  cru_prec <- raster::brick(obs_nc_file, varname = vname)
  # recorte da AS para regiao dos poligonos das bacias
  cru_prec_basins <- raster::crop(cru_prec, pols)
  rm(cru_prec); gc()
  # define nome das datas
  cru_prec_basins <- raster::setZ(cru_prec_basins,
                                  z = getZ(cru_prec_basins),
                                  name = "Date"
  )
  
  # raster::writeRaster(cru_prec_basins, 
  #                     filename = "input/cru_ts4.04.1901.2019.bacias.nc", 
  #                     overwrite = TRUE)
  
  #plot(cru_prec_basins)
  
  if(weighted_mean){
    # demora mais!
    tic()
    avg_basin <- raster::extract(
      cru_prec_basins,
      pols,
      weights = TRUE,
      normalizeWeights = TRUE,
      fun = mean,
      df = TRUE
    )
    toc()
    # 5s
  } else {
    tic()
    avg_basin <- terra::extract(
      terra::rast(cru_prec_basins),
      terra::vect(pols),
      fun = mean,
      touches = TRUE,
      method = "bilinear"
    )
    toc()
     
  }
  
  prec_cru_avg_basin <- avg_basin %>%
    as.data.frame() %>%
    as_tibble() %>%
    pivot_longer(cols = contains("X"), names_to = "date", values_to = "prec") %>%
    mutate(
      date = as_date(date, format = "X%Y.%m.%d"),
      codONS = pols$codONS[ID]
    ) %>%
    relocate(codONS, ID, date, prec) %>% 
    rename(prec_obs = prec)
  
  
  #tail(prec_cru_avg_basin)
  prec_cru_avg_basin
  
}



