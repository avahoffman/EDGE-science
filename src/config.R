## Config file for EDGE project

## General
# Set working directory for the repository (should be the git repo)
wd <-
  "/Users/avahoffman/Dropbox/Research/EDGE_Science/EDGE-science/"
data_dir <- paste(toString(wd), "data/", sep = "")

# Ecoregion colors (will be reused to stay theme-consistent)
shortgrass_color <- "#CC0000"
northern_color <- "#0000CC"
desert_color <- "#CC9900"
# When you don't want to use alpha (no transparency)
shortgrass_color_pale <- "#F5D6D6"
northern_color_pale <- "#D4E0F7"
desert_color_pale <- "#FFE6CC"

# Colors for points representing these sites
CHY_color <- "#CC0000"
SGS_color <- "#0000CC"
SEV_Blue_color <- "#00CC33"
SEV_Black_color <- "#FF9900"

# Map
shapefile_dir <- paste(toString(wd), "data/shapefiles/", sep = "")
shortgrass_name <- "Shortgrass Steppe"
northern_name <- "Northern Mixed Grass"
desert_name <- "Desert Grassland"
climate_colors <- c(desert_color, northern_color, shortgrass_color)

# C4 vs C3 grass coverage
c4_c3_sites <- c("CHY", "SGS")
c4_grasses <- c("BOGR", "BOHI", "BUDA", "C4")
c3_grasses <- c("CAREX", "PASM", "C3")
c3_c4_years <- c(2017, 2016, 2015, 2014)
include_in_drt_trt <- c("chr")

# eripoda v gracilis coverage
eri_grac_sites <- c("CHY", "SGS")
eri_grac_years <- c(2017, 2016, 2015, 2014)
eri_grac_grasses <- c("BOGR", "BUDA", "C4")