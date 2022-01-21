## --------------------------------------------------------------------------------------##
##
## Script name: calc_water_balance.R
##
## Purpose of the script: reads wat file for each hillslope and calculates 
##                        water balace for individual hillslope over the simulation period
##
## Author: Chinmay Deval
##
## Created On: 2022-01-20
##
## Copyright (c) Chinmay Deval, 2022
## Email: chinmay.deval91@gmail.com
##
## --------------------------------------------------------------------------------------##
##  Notes:
##   
##
## --------------------------------------------------------------------------------------##

## ----------------------------------Load packages---------------------------------------##
library(tidyverse)
library(rvest)
library(tictoc)
library(furrr)
library(sf)
library(sp)
library(leaflet)

## --------------------------------------------------------------------------------------##

#proj_url = "https://wepp.cloud/weppcloud/runs/lt_202012_26_Bliss_Creek_CurCond/lt-wepp_bd16b69-snow/"

proj_runid = "lt-wepp_bd16b69-snow"

## -----------------------------------Functions-----------------------------------------##

gethillwatfiles<- function(runid){
  link <- paste0("/geodata/weppcloud_runs/", runid,"/wepp/output/")
  wat_dat <- list.files(link, "*\\.wat.dat$")
  return(wat_dat)
}

calc_watbal <- function(link){
  a <- read.table(link, skip = 23,
                  col.names = c("OFE",	"J",	"Y",	"P",	"RM",	"Q",	"Ep",	"Es",
                                "Er",	"Dp",	"UpStrmQ",	"SubRIn",	"latqcc",
                                "Total_Soil_Water",	"frozwt",	"Snow_Water",	"QOFE",
                                "Tile",	"Irr",	"Area")) %>%
    dplyr::mutate_if(is.character,as.numeric)
  
  
  a <- a %>%  dplyr::mutate(wb = P-Q-Ep - Es- Er - Dp - latqcc +
                              dplyr::lag(Total_Soil_Water) - Total_Soil_Water +
                              dplyr::lag(frozwt) - frozwt+ dplyr::lag(Snow_Water) - Snow_Water) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, 3)) %>% dplyr::select(wb) %>%
    dplyr::summarise_all(.funs = sum, na.rm = TRUE) %>%
    dplyr::mutate(WeppID =readr::parse_number(gsub("^.*/", "", link)))
  
  return(as.data.frame(a))
}

get_geometry <- function(runid){
  link <- paste0("/geodata/weppcloud_runs/", runid,"/export/arcmap/subcatchments.json")
  geometry <- sf::st_read(link)%>%
    dplyr::select(WeppID, geometry) %>%
    sf::st_transform(4326) %>%
    dplyr::group_by(WeppID)%>%
    dplyr::summarize(geometry = sf::st_union(geometry))

  return(geometry)
  
}


## --------------------------------------------------------------------------------------##
## Definitely a faster alternative on linux--- tested out in WSL ubuntu
tictoc::tic()
watfilepaths <- gethillwatfiles(proj_runid)
future::plan(multisession, workers = 6)
watbal<- furrr::future_map(watfilepaths, calc_watbal)%>%
  dplyr::bind_rows() %>%    
  dplyr::mutate_if(is.list, purrr::simplify_all) %>%  
  tidyr::unnest(cols = c("wb", "WeppID"))
shp <- get_geometry(proj_runid)
Hwatbal_spdf = dplyr::left_join(shp, watbal)
tictoc::toc()


## --------------------------------------------------------------------------------------##
## Viz

pal <- leaflet::colorNumeric("viridis", domain = Hwatbal_spdf$wb)

leaflet::leaflet(Hwatbal_spdf) %>%
  leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
  leaflet::addPolygons(fillColor = ~pal(wb),
                       weight = 2,
                       opacity = 1,
                       color = "white",
                       dashArray = "3",
                       fillOpacity = 0.7,
                       popup = ~paste("Hillslope ID:", Hwatbal_spdf$WeppID,
                                      "<br>","WaterBalance Error (mm):", Hwatbal_spdf$wb),
                       label = ~WeppID,
                       highlightOptions = leaflet::highlightOptions(
                         weight = 5,
                         color = "#666",
                         dashArray = "",
                         fillOpacity = 0.7,
                         bringToFront = TRUE))%>% leaflet::addLegend(pal = pal,
                                                                     values = ~wb)
