pcks <- c("raster", "terra", "tidyverse", "here", 
          "checkmate", "metR", "fs", "lubridate",
          "tictoc", "furrr")
easypackages::libraries(pcks)

source(here("R", "data-proc-rds.R"))
source(here("R", "data-proc-basin.R"))

## poligonos bacias ------------------------------------------------------------
# arquivo RDS disponibilizado em 
path_pols_bhs <- here("input", "poligonos-bacias-incrementais.RDS")
pols_inc_sp <- readr::read_rds(path_pols_bhs) %>%
  sf::st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  dplyr::select(codONS, nome, area) %>%
  sf::as_Spatial()



#plot(pols_inc_sp)

## dados ensemble -------------------------------------------------------------

sp_avg_nmme_basin(
  path_ensemb_files = here("output", ext),
  suffix_ensemb_files = glue::glue("ensemble.*{stat}.{ext}"),
  pols = pols_inc_sp,
  stat = c('mean', 'median', 'identity'),
  ext = "qs"
)
 


# check dispersao entre os membros ----------------------------------------
# source("R/utils.R")
# out_p <- here("output/qs/basin-avgs/weighted/CMC1-CanCM3")
# fls <- dir_ls(out_p, glob = "*.qs")
# data_fls <- map_df(fls, qs::qread)
# tail(data_fls)
# 
# x <- dplyr::filter(data_fls, codONS == 6, L == 1.5) %>%
#   dplyr::mutate(
#     Sr = floor_date(S + ddays(15), "month"),
#     date_lead = Sr + dmonths(trunc(L)) + ddays(15),
#     date_lead = lubridate::as_date(lubridate::floor_date(date_lead, "month")),
#     S = NULL, 
#     date = date_lead, date_lead = NULL,
#     L = trunc(L)
#   ) %>%
#   dplyr::relocate(model, L, Sr, date)
# 
#  x_clim <- x %>%
#    group_by(mes = month(Sr)) %>%
#    summarise(dplyr::across(contains("prec"), ~median(.x, na.rm = TRUE)*30.25)) %>%
#    mutate(date = lubridate::ym(paste0("1980-", mes))) %>%
#    relocate(date)
#  x_clim %>% 
#   openair::timePlot(., names(.)[-c(1:2)], 
#                     group = TRUE, 
#                     key.columns = 4)


# tail(ens_data_nest)
# # datas variam por modelo
# min(ens_data_nest$S)
# max(ens_data_nest$S)
# seq(as.Date(min(ens_data_nest$S)), as.Date(max(ens_data_nest$S)), by = "months") %>%
#    length()
# 456 meses


# vamos gerar um raster para 1 tempo de inic. e 1 membro
# grid <- ens_data_nest[["data"]][[1]] 
# plot(raster_from_points(grid))
# basin_average(grid)
#conferido com 
#http://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CanCM4i/.HINDCAST/.MONTHLY/.prec/figviewer.html?my.help=&map.L.plotvalue=0.5&map.M.plotvalue=1.0&map.S.plotvalue=0000+1+Jan+1981&map.Y.units=degree_north&map.Y.plotlast=15.5N&map.url=X+Y+fig-+colors+coasts+-fig&map.domain=+%7B+%2Fprec+1.23088903E-05+23.955059+plotrange+%2FL+0.5+plotvalue+%2FM+1.0+plotvalue+%2FS+252.0+plotvalue+X+274.5+389.5+plotrange+Y+-60.5+15.5+plotrange+%7D&map.domainparam=+%2Fplotaxislength+432+psdef+%2Fplotborder+72+psdef+%2FXOVY+null+psdef&map.zoom=Zoom&redraw.x=-1&redraw.y=13&map.Y.plotfirst=60.5S&map.X.plotfirst=85.5W&map.X.units=degree_east&map.X.modulus=360&map.X.plotlast=29.5W&map.prec.plotfirst=1.2308890E-05&map.prec.units=mm%2Fday&map.prec.plotlast=23.95506&map.newurl.grid0=X&map.newurl.grid1=Y&map.newurl.land=draw+coasts&map.newurl.plot=colors&map.plotaxislength=432&map.plotborder=72&map.fnt=NimbusSanLSymbol&map.fntsze=12&map.color_smoothing=1&map.XOVY=auto&map.iftime=25&map.mftime=25&map.fftime=200

# cr <- crop(raster_grid(grid), pols_inc_sp)
# cr_pols <- raster::rasterToPolygons(cr)
# cr_pols <- sf::st_as_sfc(cr_pols)
# cr_pols_inters <- sf::st_intersection(pols_inc_large_ll, cr_pols)
# plot(cr_pols_inters, add = TRUE, border = "blue", lwd = 0.9, col = NA)
# plot(sf::st_geometry(pols_inc_large_ll), border = "black", col = NA, add = TRUE)

# reamostrar cru para grade do modelo
# fazer media na area das bhs


