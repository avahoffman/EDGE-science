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

###########################################################################################

fill_region <- function(name, dataframe) {
  # Add region identifier
  dataframe$region <- rep(name, nrow(dataframe))
  return(dataframe)
}

generate_shapefile_data <- function() {
  sf_shortgrass <-
    fill_region(shortgrass_name, fortify(readOGR(shapefile_dir, "shortgrass_mtn")))
  # sf_shortgrass <- sf_shortgrass[sf_shortgrass$long > -107,] # Don't take too much of the Rockies
  
  sf_northern <-
    fill_region(northern_name, fortify(readOGR(shapefile_dir, "northern_steppe")))
  sf_northern <-
    sf_northern[sf_northern$hole == FALSE, ] # Allows Black Hills to be included
  
  sf_desert <-
    fill_region(desert_name, fortify(readOGR(shapefile_dir, "chihuahuan_desert")))
  usa_map <- map_data("state")
  
  sf_data <- list(
    "sf_shortgrass" = sf_shortgrass,
    "sf_northern" = sf_northern,
    "sf_desert" = sf_desert,
    "usa_map" = usa_map
  )
  return(sf_data)
}

plot_site_map_with_ecoregions <- function(sf_data) {
  gg <- ggplot() +
    
    # Add base map on which to plot
    geom_map(
      data = sf_data$usa_map,
      map = sf_data$usa_map,
      aes(x = long, y = lat, map_id = region),
      fill = 'black',
      alpha = 0.05
    ) + #add size command to make country lines visible
    
    geom_map(
      data = sf_data$usa_map,
      map = sf_data$usa_map,
      aes(x = long, y = lat, map_id = region),
      fill = NA,
      colour = "grey"
    ) +
    
    theme_map() + # Remove axes
    coord_map(xlim = c(-110,-95), ylim = c(27, 45)) + # Crop map
    
    # Add shapefile polygons for different ecoregions, using different data
    geom_polygon(
      data = sf_data$sf_shortgrass,
      aes(
        x = long,
        y = lat,
        group = group,
        fill = region
      ),
      alpha = 0.3
    ) +
    
    geom_polygon(
      data = sf_data$sf_northern,
      aes(
        x = long,
        y = lat,
        group = group,
        fill = region
      ),
      alpha = 0.3
    ) +
    
    geom_polygon(
      data = sf_data$sf_desert,
      aes(
        x = long,
        y = lat,
        group = group,
        fill = region
      ),
      alpha = 0.3
    ) +
    
    # Add points
    geom_point(aes(x = -104.8, y = 40.9), size = 1.5, color = SGS_color) + ## SGS
    geom_point(aes(x = -104.8, y = 41.5), size = 1.5, color = CHY_color) + ## CHY
    geom_point(aes(x = -106.3, y = 34), size = 1.5, color = SEV_Blue_color) +  ## Sev Blue
    geom_point(aes(x = -106.7, y = 34), size = 1.5, color = SEV_Black_color) +  ## Sev Black
    
    # Add labels
    geom_text(aes(x = -104.8, y = 40.5), label = "SGS") + ## SGS
    geom_text(aes(x = -104.8, y = 42), label = "CHY") + ## CHY
    geom_text(aes(x = -105.7, y = 34), label = "SEV\nBlue") + ## Sev
    geom_text(aes(x = -107.3, y = 34), label = "SEV\nBlack") + ## Sev
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
         height = 6,
         width = 4)
  
}

plot_site_map_with_ecoregions(generate_shapefile_data())