###########################################################################################
## set working directory
setwd("/Users/avahoffman/Dropbox/Research/EDGE_Science/EDGE-science/src/")
source("config.R")
setwd(wd)

###########################################################################################
## load libraries
library(ggplot2)  # FYI you need v2.0
library(mapproj)
library(cowplot)
library(ggthemes) # theme_map and tableau colors
library(maps)
library(rgdal)
library(dplyr)

###########################################################################################

fill_region <- function(name, dataframe) {
  # Add region identifier
  dataframe$region <- rep(name, nrow(dataframe))
  return(dataframe)
}

generate_shapefile_data <- function() {
  sf_shortgrass <-
    fill_region(shortgrass_name, fortify(readOGR(shapefile_dir, "shortgrass_prairie"))) %>% 
    filter(lat <= 41 | long >= -102)
  
  sf_shortgrass_chunk <-
    fill_region(shortgrass_name, fortify(readOGR(shapefile_dir, "shortgrass_prairie"))) %>% 
    filter(lat >= 41 & long <= -102) %>%  mutate(region =replace(region, 
                                                                 region == "Shortgrass Steppe", 
                                                                 "Northern Mixed Grass"))
  sf_northern <-
    fill_region(northern_name, fortify(readOGR(shapefile_dir, "northern_steppe")))
  sf_northern <-
    sf_northern[sf_northern$hole == FALSE,] # Allows Black Hills to be included

  sf_desert_chunk <-
    fill_region(shortgrass_name, fortify(readOGR(shapefile_dir, "nm_mtns"))) %>% 
    filter(lat <= 35 & long >= -107.5)
  
  sf_desert <-
    fill_region(desert_name, fortify(readOGR(shapefile_dir, "chihuahuan_desert")))
  
  usa_map <- map_data("state")
  
  sf_data <- list(
    "sf_shortgrass" = sf_shortgrass,
    "sf_shortgrass_chunk" = sf_shortgrass_chunk,
    "sf_northern" = sf_northern,
    "sf_desert_chunk" = sf_desert_chunk,
    "sf_desert" = sf_desert,
    "usa_map" = usa_map
  )
  return(sf_data)
}

plot_site_map_with_ecoregions <- function(sf_data) {
  gg <- ggplot() +
    
    # Add shapefile polygons for different ecoregions, using different data
    geom_polygon(
      data = sf_data$sf_shortgrass,
      aes(
        x = long,
        y = lat,
        group = group,
        fill = region
      ),
    ) +
    
    geom_polygon(
      data = sf_data$sf_shortgrass_chunk,
      aes(
        x = long,
        y = lat,
        group = group,
        fill = region
      ),
    ) +
    
    geom_polygon(
      data = sf_data$sf_northern,
      aes(
        x = long,
        y = lat,
        group = group,
        fill = region
      ),
    ) +
    
    geom_polygon(
      data = sf_data$sf_desert_chunk,
      aes(
        x = long,
        y = lat,
        group = group,
        fill = region
      )
    ) +
    
    geom_polygon(
      data = sf_data$sf_desert,
      aes(
        x = long,
        y = lat,
        group = group,
        fill = region
      )
    ) +
  
    # Add base map on which to plot
    geom_map(
      data = sf_data$usa_map,
      map = sf_data$usa_map,
      aes(x = long, y = lat, map_id = region),
      fill = NA
    ) + #add size command to make country lines visible
    
    geom_map(
      data = sf_data$usa_map,
      map = sf_data$usa_map,
      aes(x = long, y = lat, map_id = region),
      fill = NA,
      colour = "grey" # State outlines
    ) +
    
    theme_map() + # Remove axes
    coord_map(xlim = c(-110, -95), ylim = c(27, 45)) + # Crop map
    
    # Add points
    geom_point(aes(x = -104.77, y = 40.82), size = 1.5, color = SGS_color) + ## SGS
    geom_point(aes(x = -104.88, y = 41.2), size = 1.5, color = CHY_color) + ## CHY
    geom_point(aes(x = -106.6, y = 34.2), size = 1.5, color = SEV_Blue_color) +  ## Sev Blue
    geom_point(aes(x = -106.7, y = 34.1), size = 1.5, color = SEV_Black_color) +  ## Sev Black
    
    # Add labels
    geom_text(aes(x = -104.8, y = 40.3), label = "SGS", size=3) + ## SGS
    geom_text(aes(x = -104.8, y = 41.7), label = "CHY", size=3) + ## CHY
    geom_text(aes(x = -105.35, y = 34.75), label = "SEV Blue", size=3) + ## Sev
    geom_text(aes(x = -107.55, y = 33.55), label = "SEV Black", size=3) + ## Sev
    geom_text(aes(x = -105, y = 30), label = desert_name, color = desert_color) + ## Desert grassland label
    geom_text(aes(x = -102.5, y = 38), label = shortgrass_name, color = shortgrass_color) + ## Shortgrass label
    geom_text(aes(x = -103, y = 44), label = northern_name, color = northern_color) + ## Northern grassland label
    
    # Add border and remove legend
    theme(
      panel.border = element_rect(
        colour = "black",
        fill = NA,
        size = 2
      ),
      legend.position = "none"
    ) +
    
    # Add custom colors
    scale_fill_manual(values = climate_colors)
  
  gg
  ggsave(file = "figures/site_map_with_ecoregions.pdf",
         height = 5,
         width = 3)
  return(gg)
}

plot_site_map_with_ecoregions(generate_shapefile_data())
