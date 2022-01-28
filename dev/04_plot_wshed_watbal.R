## --------------------------------------------------------------------------------------##
##
## Script name: 04_plot_wshed_watbal.R
##
## Purpose of the script: reads totwatsed.csv from weppcloud and creates a watbal chart
##
## Author: Chinmay Deval
##
## Created On: 2022-01-27
##
## Copyright (c) Chinmay Deval, 2022
## Email: chinmay.deval91@gmail.com
##
## --------------------------------------------------------------------------------------##
##  Notes:
##   
##
## --------------------------------------------------------------------------------------##

## --------------------------clear environment and console-------------------------------##
rm(list = ls())
cat("\014")

## ----------------------------------Load packages---------------------------------------##
library(echarts4r)
library(tidyverse)
library(furrr)
library(data.table)
library(lubridate)
library(janitor)

## --------------------------------------------------------------------------------------##
runid = "https://wepp.cloud/weppcloud/runs/lt_202012_26_Bliss_Creek_CurCond/lt-wepp_bd16b69-snow/"


# args = commandArgs(trailingOnly=TRUE)

# proj_runid = args[1]


## --------------------------------------------------------------------------------------##


process_totwatsed_csv = function(proj_runid){
  
  totalwatseddf <- data.table::fread(paste0(proj_runid,"resources/wepp/totalwatsed.csv")) %>% 
    janitor::clean_names()%>%
    dplyr::rename("WY" = "water_year")%>%
    dplyr::mutate(Date = lubridate::make_date(year,mo,da))
  
  return(totalwatseddf)
  
}

wshed_watbal_df = function(totwatsed_daily) {
  
  wys = as.numeric(length(unique(totwatsed_daily$WY)))
  
  totalwatsed_wbal = totwatsed_daily %>% dplyr::select("WY",
                                                       "Date",
                                                       "precipitation_mm",
                                                       "rain_melt_mm",
                                                       "transpiration_mm",
                                                       "evaporation_mm",
                                                       "percolation_mm",
                                                       "runoff_mm",
                                                       "lateral_flow_mm")%>% 
  dplyr::filter(Date >= paste0(lubridate::year(Date[1]),"-10-01"))%>%
  dplyr::select(- c(Date,WY))%>%
  dplyr::summarise_all(.funs = sum) %>%
  dplyr::mutate(
    precipitation_mm = precipitation_mm / wys,
    rain_melt_mm = rain_melt_mm / wys,
    transpiration_mm = transpiration_mm / wys,
    evaporation_mm = evaporation_mm / wys,
    percolation_mm = percolation_mm / wys,
    runoff_mm = runoff_mm / wys,
    lateral_flow_mm = lateral_flow_mm / wys) %>%
  dplyr::mutate(
    rain_melt_mm = rain_melt_mm / precipitation_mm * 100,
    transpiration_mm = transpiration_mm / precipitation_mm *
      100,
    evaporation_mm = evaporation_mm / precipitation_mm *
      100,
    percolation_mm = percolation_mm / precipitation_mm *
      100,
    runoff_mm = runoff_mm / precipitation_mm * 100,
    lateral_flow_mm = lateral_flow_mm / precipitation_mm *
      100,
    WbalErr_mm = rain_melt_mm - (
      transpiration_mm + evaporation_mm + percolation_mm + runoff_mm + lateral_flow_mm
    )
  ) %>%
  dplyr::rename(
    "Precipitation (mm)" = "precipitation_mm",
    "Rain+Melt (%)" = "rain_melt_mm",
    "Transpiration (%)" = "transpiration_mm",
    "Evaporation (%)" = "evaporation_mm",
    "Percolation (%)" = "percolation_mm",
    "Runoff(%)" = "runoff_mm",
    "Lateral flow(%)" = "lateral_flow_mm",
    "Water Balance Error(%)" = "WbalErr_mm"
  ) %>%
  tidyr::gather(key = "variable") %>% 
  dplyr::mutate(dplyr::across(where(is.numeric), round, 2))
  
  return(totalwatsed_wbal)
}


## --------------------------------------------------------------------------------------##
totwatsed_d_df = process_totwatsed_csv(proj_runid=runid)

totwatsed_wat_wbal = wshed_watbal_df(totwatsed_d_df)


## --------------------------------------------------------------------------------------##
## Plot
## --------------------------------------------------------------------------------------##
wshed_watbal = totwatsed_wat_wbal %>% dplyr::filter(variable != "Precipitation (mm)")  %>%
  echarts4r::e_charts(variable)  %>%
  echarts4r::e_pie(
    value,
    roseType = "radius",
    radius = c("55%", "70%"),
    hoverAnimation = TRUE
  )%>%
  echarts4r::e_tooltip()

htmlwidgets::saveWidget(wshed_watbal, file = "wshed_watbal.html")