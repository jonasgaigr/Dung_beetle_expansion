#----------------------------------------------------------#
#
#
#       Exploring Coprimorphus scrutator occurrence
#
#                     Config file
#
#
#                      Jonáš Gaigr
#                         2025
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# Load packages -----
#----------------------------------------------------------#
if(!isTRUE(require(tidyverse, quietly = TRUE))) {
  install.packages("tidyverse", dependencies = TRUE); library(tidyverse)
} else {
  require(tidyverse)}

if(!isTRUE(require(sf, quietly = TRUE))) {
  install.packages("sf", dependencies = TRUE); library(sf)
} else {
  require(sf)}

if(!isTRUE(require(sp, quietly = TRUE))) {
  install.packages("sp", dependencies = TRUE); library(sp)
} else {
  require(sp)}

if(!isTRUE(require(proj4, quietly = TRUE))) {
  install.packages("proj4", dependencies = TRUE); library(proj4)
} else {
  require(proj4)}

if(!isTRUE(require(openxlsx, quietly = TRUE))) {
  install.packages("openxlsx", dependencies = TRUE); library(openxlsx)
} else {
  require(openxlsx)}

if(!isTRUE(require(lme4, quietly = TRUE))) {
  install.packages("lme4", dependencies = TRUE); library(lme4)
} else {
  require(lme4)}

if(!isTRUE(require(lmerTest, quietly = TRUE))) {
  install.packages("lmerTest", dependencies = TRUE); library(lmerTest)
} else {
  require(lmerTest)}

if(!isTRUE(require(Matrix, quietly = TRUE))) {
  install.packages("Matrix", dependencies = TRUE); library(Matrix)
} else {
  require(Matrix)}

if(!isTRUE(require(vegan, quietly = TRUE))) {
  install.packages("vegan", dependencies = TRUE); library(vegan)
} else {
  require(vegan)}

if(!isTRUE(require(GLMMadaptive, quietly = TRUE))) {
  install.packages("GLMMadaptive", dependencies = TRUE); library(GLMMadaptive)
} else {
  require(GLMMadaptive)}

if(!isTRUE(require(RCzechia, quietly = TRUE))) {
  install.packages("RCzechia", dependencies = TRUE); library(RCzechia)
} else {
  require(RCzechia)}

if(!isTRUE(require(openxlsx, quietly = TRUE))) {
  install.packages("openxlsx", dependencies = TRUE); library(openxlsx)
} else {
  require(openxlsx)}

if(!isTRUE(require(remotes, quietly = TRUE))) {
  install.packages("remotes", dependencies = TRUE); library(remotes)
} else {
  require(remotes)}

if(!isTRUE(require(elevatr, quietly = TRUE))) {
  install.packages("elevatr", dependencies = TRUE); library(elevatr)
} else {
  require(elevatr)}

if(!isTRUE(require(rgbif, quietly = TRUE))) {
  install.packages("rgbif", dependencies = TRUE); library(rgbif)
} else {
  require(rgbif)}

if(!isTRUE(require(rn2kcz, quietly = TRUE))) {
  remotes::install_github("jonasgaigr/rn2kcz", dependencies = TRUE); library(rn2kcz)
} else {
  require(rn2kcz)}

if(!isTRUE(require(rndop, quietly = TRUE))) {
  remotes::install_github("kalab-oto/rndop", dependencies = TRUE); library(rndop)
} else {
  require(rndop)}


library(rvest)
library(httr)
library(xml2)
#----------------------------------------------------------#
# Load data -----
#----------------------------------------------------------#
#--------------------------------------------------#
## Load remote data -----
#--------------------------------------------------#
# Borders of Czechia
czechia_border <- 
  RCzechia::republika(
    resolution = "high"
  ) %>%
  sf::st_transform(
    ., 
    st_crs("+init=epsg:5514")
  ) 

# Data on protected areas and mapping fields
endpoint <- "http://gis.nature.cz/arcgis/services/Aplikace/Opendata/MapServer/WFSServer?"
caps_url <- base::paste0(endpoint, "request=GetCapabilities&service=WFS")

layer_name_evl <- "Opendata:Evropsky_vyznamne_lokality"
layer_name_mzchu <- "Opendata:Maloplosna_zvlaste_chranena_uzemi__MZCHU_"
layer_name_sitmap1rad <- "Opendata:Mapovaci_sit_-_deleni_1.radu"

getfeature_url_evl <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_evl
)
getfeature_url_mzchu <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_mzchu
)
getfeature_url_sitmap1rad <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_sitmap1rad
)


evl <- sf::st_read(getfeature_url_evl) %>%
  sf::st_transform(
    ., 
    st_crs("+init=epsg:5514")
  ) 
mzchu <- sf::st_read(getfeature_url_mzchu) %>%
  sf::st_transform(
    ., 
    st_crs("+init=epsg:5514")
  )
sitmap <- sf::st_read(getfeature_url_sitmap1rad) %>%
  sf::st_transform(
    ., 
    st_crs("+init=epsg:5514")
  ) %>%
  sf::st_crop(
    .,
    czechia_border  # crop by the border of Czechia
  )

#--------------------------------------------------#
## Load species data 2019 - 2024 -----
#--------------------------------------------------#
data <- 
  readr::read_csv2(
    "Data/Input/data_scrutator.csv",
    locale = locale(encoding = "Windows-1250")
  ) %>%
  dplyr::mutate(
    date = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
    # Redukce data na den
    day = as.numeric(substring(DATUM_OD, 7, 8)),
    # Redukce data na měsíc
    month = as.numeric(substring(DATUM_OD, 5, 6)),
    # Redukce data na rok
    year = as.numeric(substring(DATUM_OD, 1, 4)),
    ) %>%
  sf::st_as_sf(
    coords = c(
      "X", 
      "Y"
      ), 
    crs = 5514
    ) %>%
  sf::st_transform(
    .,
    crs = 4326
    ) %>%
  dplyr::mutate(
    decimalLongitude = sf::st_coordinates(.)[, 1],
    decimalLatitude = sf::st_coordinates(.)[, 2]
  ) %>%
  sf::st_filter(
    .,
    czechia_border %>%
      sf::st_transform(
        .,
        crs = 4326
      ) 
  )

#--------------------------------------------------#
## Load species GBIF data  -----
#--------------------------------------------------#

data_world <- 
  rgbif::occ_data(
    taxonKey = 1065995, 
    limit = 15000
    )

data_world_counts <- 
  data_world$data %>% 
  count(
    datasetKey, 
    sort = TRUE
    ) 

data_w <- 
  data_world$data %>%
  dplyr::mutate(
    decade = floor(year / 10) * 10,
    zone = case_when(
      decimalLatitude >= 0 & decimalLatitude < 23.5 ~ "Tropical_N",
      decimalLatitude >= 23.5 & decimalLatitude < 35 ~ "Subtropical_N",
      decimalLatitude >= 35 & decimalLatitude < 55 ~ "Temperate_N",
      decimalLatitude >= 55 & decimalLatitude < 66.5 ~ "Boreal_N",
      decimalLatitude >= 66.5 ~ "Arctic",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(
    is.na(zone) == FALSE
  ) %>%
  filter(
    year >= 1970
    )

nrow(data_w)
#----------------------------------------------------------#
# End config -----
#----------------------------------------------------------#