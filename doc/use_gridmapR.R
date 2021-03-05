## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(gridmapR)
library(sf)
library(tidyverse)
library(lwgeom)
library("rnaturalearth")

## -----------------------------------------------------------------------------
sf_file <- ne_countries(scale = "medium", returnclass = "sf") %>% mutate(myid=row_number()) %>% filter(continent=="Africa")
ggplot(sf_file) + geom_sf()

## ----message=FALSE, warning=FALSE---------------------------------------------
grid_file <- make_gridsf(sf_file,id_name="myid",cellsize_value = 3,stepsize = 1.1)
ggplot() + 
   geom_sf(data=sf_file,color="blue") +
   geom_sf(data=grid_file,color="red", alpha=.3) + theme_minimal()

