library(tidyverse)
library(leaflet)
library(abjData) # remotes::install_github("abjur/abjData")

muni <- abjData::muni

muni |>
  filter(uf_sigla == "MT") |>
  leaflet() |>
  addTiles(
    urlTemplate = "https://tiles.stadiamaps.com/tiles/alidade_smooth_dark/{z}/{x}/{y}{r}.png"
  ) |>
  addMarkers(
    lng = ~lon,
    lat = ~lat,
    popup = ~muni_nm,
    clusterOptions = markerClusterOptions()
  )


# mapa tematico -----------------------------------------------------------

# remotes::install_github("ipeaGIT/geobr", subdir = "r-package")

dados_geobr <- geobr::read_municipality("MT")
# readr::read_rds("dados/geobr/dados_geobr.rds")

dados_com_pnud <- dados_geobr |>
  mutate(muni_id = as.character(code_muni)) |>
  inner_join(abjData::pnud_min, "muni_id") |>
  filter(ano == "2010")

pal <- colorNumeric("Purples", dados_com_pnud$idhm)
pal(0.628)

dados_com_pnud |>
  leaflet() |>
  addTiles(
    urlTemplate = "https://tiles.stadiamaps.com/tiles/alidade_smooth_dark/{z}/{x}/{y}{r}.png"
  ) |>
  addPolygons(
    fillColor = ~pal(idhm),
    fillOpacity = 0.9,
    color = "black",
    weight = 1,
    label = ~muni_nm
  )
