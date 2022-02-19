#' @name plot_map_ggplot
#'
#' @title Plot static trajectory on a map using ggplot
#'
#' @param data a tibble of Time, Longitude, and Latitude data
#'
#' @author Neil E. Klepeis
#'
#' @details This function plots a static map showing where one or more
#'sensors were located at successive times
#'
#' Uses the osmdata (Open Stream Map) package and ggplot to create
#' the plot.
#'
# ----------------------------------------------------------------

# TODO:  Use osmdata  with ggplot   openstreet map

#  See packages:  maps, ggspatial, rnaturalearth, ggmap, rvest
##  urbnmap  tmap  USABoundaries sf

# see:  http://joshuamccrain.com/tutorials/maps/streets_tutorial.html
# see:  https://www.connorrothschild.com/post/map-springfield

#  see https://taraskaduk.com/posts/2021-01-18-print-street-maps/

## nice:   https://hohenfeld.is/posts/streetmaps-in-r-with-ggplot2/

##  and here good stuff.  https://ghanadatastuff.com/post/mapping_street_names/

##   https://www.r-bloggers.com/2018/08/how-to-quickly-enrich-a-map-with-natural-and-anthropic-details/

#  urbn

#   See plot_AT_streams for a calender plot
#  showing concentrations by day/hour/etc

plot_map_ggplot <- function(data, by="1 day", streets=TRUE,
                               place=TRUE, water=TRUE, rail=TRUE,
                               building=TRUE) {

  # library(tidyverse)
  # library(osmdata)
  # library(sf)
  # library(timetk)

  #  Make the map data

  data <- data %>%
    select(`Date (GMT)`, starts_with(c("Latitude","Longitude"))) %>%
    #pivot_longer(
    #  cols=starts_with(c("Latitude","Longitude")),
    #  names_to="Response",
    #  values_to="Value"
    #) %>%
    rename(Time = `Date (GMT)`)

  #print(data)

  #data <- data %>%
  #  #group_by() %>%
  #  summarize_by_time(.by="hour",
  #                    Latitude = mean(Latitude),
  #                    Longitude=mean(Longitude))

  # Get unique lat/lon points
  data <- data %>%
    distinct(Latitude, Longitude)

  #return(data)

  data_sf <-
    st_as_sf(data,
             coords = c("Longitude", "Latitude"), crs = 4326)

  #return(data_sf)

  latr <- range(data$Latitude)
  lonr <- range(data$Longitude)
  bbox <- c(lonr[1],latr[1],lonr[2],latr[2])

  cat("Bounding Box:\n")
  print(bbox)

  m <- opq(bbox = bbox)

  #return(m)

  if (streets) streets_sf <- m %>%
    add_osm_feature(key = "highway",
                    value = c("motorway", "primary", "motorway_link",
                              "primary_link","secondary", "tertiary",
                              "secondary_link", "tertiary_link",
                              "residential", "living_street",
                              "unclassified","service", "footway")) %>%
    osmdata_sf()


  if (water) water_sf <- m %>%
    add_osm_feature(key = "water", value = c("lake","river",
                                             "reservoir")) %>%
    osmdata_sf()


  if (rail) rail_sf <- m %>%
    add_osm_feature(key = "railway", value="rail") %>%
    osmdata_sf()

  if (place) place_sf <- m %>%
    add_osm_feature(key = 'place', value = c('village', 'town')) %>%
    osmdata_sf()

  if (building) building_sf <- m %>%
    add_osm_feature(key = 'building', value="!tree_house") %>%
    osmdata_sf()

  #return(place_sf)

  #print(water_sf)

  #print(building_sf)

  # Plot the map

  ggplot() +
    geom_sf(data = building_sf$osm_polygons,
            inherit.aes = FALSE,
            color = "orange",
            size=0.4) +
    geom_sf(data = streets_sf$osm_lines,
            inherit.aes = FALSE,
            color = "black") +
    geom_sf(data = water_sf$osm_lines,
            inherit.aes = FALSE,
            color = "blue") +
    geom_sf(data = water_sf$osm_polygons,
            inherit.aes = FALSE,
            size=1.2, color = "blue") +
    geom_sf(data = water_sf$osm_multipolygons,
            inherit.aes = FALSE,
            size=1.2, color = "blue") +
    #geom_sf(data = water_sf$osm_points,
    #        inherit.aes = FALSE,
    #        color = "blue") +
    geom_sf(data = rail_sf$osm_lines,
            inherit.aes = FALSE,
            color = "gray", linetype="dashed",
            size=1.2) +
    #geom_sf(data=place_sf$osm_points) +
    geom_sf(data = data_sf,
            color="red", size=1.2) +
    scale_x_continuous(limits=lonr) +
    scale_y_continuous(limits=latr)



}
