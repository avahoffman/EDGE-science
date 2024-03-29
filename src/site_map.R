# This script plots the site map and ecorergions for the EDGE project.
###########################################################################################
# Load libraries
library(ggplot2)  # REQUIRES: v2.0
library(mapproj)
library(cowplot)
library(ggthemes) # theme_map
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
  # Generate data for geom polygons
  
  # Shortgrass steppe
  sf_shortgrass <-
    fill_region(shortgrass_name,
                fortify(readOGR(shapefile_dir,
                                "shortgrass_prairie"))) %>%
    # Create the break at about the CO border with WY per Lauenroth 2008.. 
    # will be slightly off because of the projection used
    filter(lat <= 41 | 
             long >= -102)
  
  # The portion of shortgrass steppe that should actually be mixed grass
  sf_shortgrass_chunk <-
    fill_region(shortgrass_name,
                fortify(readOGR(shapefile_dir,
                                "shortgrass_prairie"))) %>%
    # Above the CO border
    filter(lat >= 41 & 
             long <= -102) %>%
    # Change the region to mixed grass
    mutate(region = replace(region,
                            region == "Shortgrass Steppe",
                            "Northern Mixed Grass"))
  
  # True northern mixed grass prairie
  sf_northern <-
    fill_region(northern_name, 
                fortify(readOGR(shapefile_dir, 
                                "northern_steppe")))
  # There is a hole at the Black Hills.. but including it for simplicity
  sf_northern <-
    sf_northern[sf_northern$hole == FALSE, ] # Allows Black Hills to be included
  
  # Include a small stretch of the southern Rockies as desert grassland
  sf_desert_chunk <-
    fill_region(shortgrass_name, 
                fortify(readOGR(shapefile_dir, 
                                "nm_mtns"))) %>%
    # Stop above our SEV sites
    filter(lat <= 35 & 
             long >= -107.5)
  
  # Main portion of desert grassland
  sf_desert <-
    fill_region(desert_name, 
                fortify(readOGR(shapefile_dir, 
                                "chihuahuan_desert")))
  
  usa_map <- 
    map_data("state")
  
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

plot_site_map_with_ecoregions <- function(sf_data, filename = NA, ecoregions_only = F) {
  gg <- ggplot() +
    
    # Add shapefile polygons for different ecoregions, using different data
    # "Region" determines the color
    geom_polygon(data = sf_data$sf_shortgrass,
                 aes(
                   x = long,
                   y = lat,
                   group = group,
                   fill = region
                 ),) +
    
    geom_polygon(data = sf_data$sf_shortgrass_chunk,
                 aes(
                   x = long,
                   y = lat,
                   group = group,
                   fill = region
                 ),) +
    
    geom_polygon(data = sf_data$sf_northern,
                 aes(
                   x = long,
                   y = lat,
                   group = group,
                   fill = region
                 ),) +
    
    geom_polygon(data = sf_data$sf_desert_chunk,
                 aes(
                   x = long,
                   y = lat,
                   group = group,
                   fill = region
                 )) +
    
    geom_polygon(data = sf_data$sf_desert,
                 aes(
                   x = long,
                   y = lat,
                   group = group,
                   fill = region
                 )) +
    
    # Overlay state lines
    geom_map(
      data = sf_data$usa_map,
      map = sf_data$usa_map,
      aes(map_id = region), 
      fill = NA,
      colour = "grey" # State outlines
    ) +
    
    # Remove axes
    theme_map() +
    # Crop to smaller area
    coord_map(xlim = c(-112,-95), ylim = c(29, 45))
    
    # Add points for EDGE sites
    # May not be perfect, but want to be able to see each one
    if (!(ecoregions_only)){
    gg <- 
      gg +
      geom_point(
      aes(x = -104.77, 
          y = 40.82), 
      size = 1, 
      color = SGS_color) + ## SGS
    
    geom_point(
      aes(x = -104.88, 
          y = 41.2), 
      size = 1, 
      color = CHY_color) + ## CHY
    
    geom_point(
      aes(x = -106.45, 
          y = 34.2), 
      size = 1, 
      color = SEV_Blue_color) +  ## Sev Blue
    
    geom_point(
      aes(x = -106.7, 
          y = 34.1), 
      size = 1, 
      color = SEV_Black_color) +  ## Sev Black
    
    # Add labels for sites
    geom_text(
      aes(x = -104.8, 
          y = 40.3), 
      label = "SGS", 
      size = 3) + ## SGS
    
    geom_text(
      aes(x = -104.8, 
          y = 41.7), 
      label = "CHY", 
      size = 3) + ## CHY
    
    geom_text(
      aes(x = -105.35, 
          y = 34.75), 
      label = "SEV Blue", 
      size = 3) + ## Sev
    
    geom_text(
      aes(x = -107.55, 
          y = 33.55), 
      label = "SEV Black", 
      size = 3) + ## Sev
    
    # Add labels for ecoregions
    geom_text(
      aes(x = -105, 
          y = 30), 
      label = desert_name, 
      color = desert_label_color) + ## Desert grassland label
    
    geom_text(
      aes(x = -102.5, 
          y = 38), 
      label = shortgrass_name, 
      color = shortgrass_label_color) + ## Shortgrass label
    
    geom_text(
      aes(x = -103, 
          y = 44), 
      label = northern_name, 
      color = northern_label_color) + ## Northern grassland label
    
    # Add border and remove legend
    theme(
      panel.border = element_rect(
        colour = "black",
        fill = NA,
        size = 1
      ),
      legend.position = "none"
    ) +
    
    # Add custom colors
    scale_fill_manual(values = climate_colors)
    } else {
      gg <- 
        gg +
        
        # Add labels for ecoregions
        geom_text(
          aes(x = -105, 
              y = 30), 
          label = desert_name, 
          color = desert_label_color) + ## Desert grassland label
        
        geom_text(
          aes(x = -102.5, 
              y = 38), 
          label = shortgrass_name, 
          color = shortgrass_label_color) + ## Shortgrass label
        
        geom_text(
          aes(x = -103, 
              y = 44), 
          label = northern_name, 
          color = northern_label_color) + ## Northern grassland label
        
        # Add border and remove legend
        theme(
          panel.border = element_rect(
            colour = "black",
            fill = NA,
            size = 1
          ),
          legend.position = "none"
        ) +
        
        # Add custom colors
        scale_fill_manual(values = climate_colors)
    }
  
  gg
  if (!(is.na(filename))) {
    ggsave(file = filename,
           height = 5,
           width = 3)
  }
  return(gg)
}

