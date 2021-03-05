# Purpose Create a Hexagon Shapefile to represent areas of a shapefile equally according to indicator
library(sf)
library(tidyverse)
library(lwgeom)

# Needs an sf File to work


make_gridsf <- function(data,id_name,cellsize_value = .1,stepsize=1.025){
  if (stepsize<1){
    stepsize<- stepsize+1
  }
  areas <-  data %>%
    st_transform(.,"+proj=longlat +datum=WGS84") %>%
    rename(".temp_id_name"=id_name) %>%
    mutate(area_id = `.temp_id_name`) %>%
    rename(id_name=".temp_id_name")
  area_centroid <- areas %>% select(area_id) %>% st_centroid()

  grid <- st_make_grid(areas, cellsize = cellsize_value,
                       square = FALSE) %>% st_sf(.) %>%
    mutate(grid_id=row_number())
  num_cells <- dim(grid)[1]
  num_areas <-dim(areas)[1]

  while (num_cells>num_areas){
    cellsize_value <- cellsize_value*stepsize
    grid <- st_make_grid(areas, cellsize = cellsize_value,
                         square = FALSE) %>% st_sf(.) %>%
      mutate(grid_id=row_number())
    num_cells <- dim(grid)[1]
  }
  cellsize_value <- cellsize_value*1/stepsize
  grid <- st_make_grid(areas, cellsize = cellsize_value,
                       square = FALSE) %>% st_sf(.) %>%
    mutate(grid_id=row_number())

  # Compute distances
  grid_centroid <- grid %>% st_centroid()

  distance_matrix <-st_distance(area_centroid, grid_centroid) %>%
    as_tibble()
  colnames(distance_matrix) <- grid_centroid %>% pull(grid_id)
  distance_matrix.df <- distance_matrix %>%
    mutate(area_id = area_centroid %>% pull(area_id)) %>%
    relocate(area_id)  %>% gather(grid_id,distance, 2:ncol(.)) %>%
    arrange(area_id,distance)

  # Solve iterative

  # First try nearest neigbor
  first_try <- distance_matrix.df %>% group_by(area_id) %>%
    filter(row_number()==1)

  problems_first_try <- first_try %>% group_by(grid_id) %>%
    mutate(count=n()) %>% filter(count>1) %>% ungroup() %>% select(area_id)
  solved <- anti_join(first_try,problems_first_try,by="area_id") %>%
    ungroup() %>% select(grid_id,area_id)

  remaining_options <- anti_join(distance_matrix.df,
                                 solved %>%
                                   select(grid_id),by="grid_id")

  # Then remaining Areas iteratively
  area_list <-   remaining_options %>%
    anti_join(.,solved %>% select(area_id),by="area_id") %>% count(area_id) %>% pull(area_id)
  for(area in area_list){
    solution <- remaining_options %>% filter(area_id==area) %>% head(1) %>% select(-distance)
    solved <- bind_rows(solved,solution)
    remaining_options <- remaining_options %>% filter(grid_id!=solution$grid_id)
  }

  # Then remaining grids to nearest neighbor
  solved <- bind_rows(solved,remaining_options %>% arrange(grid_id,distance) %>% group_by(grid_id) %>% filter(row_number()==1) %>% select(area_id,grid_id))

  # Grid-sf

  grid_agg <- grid %>% left_join(solved %>% mutate(grid_id=as.numeric(grid_id)), by="grid_id") %>%
    group_by(area_id) %>% summarise(  ) %>% left_join(areas %>% st_set_geometry(NULL)) %>%
    ungroup()

  grid_agg

}

