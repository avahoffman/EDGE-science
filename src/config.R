# Config file for EDGE project
# Includes relevant constants for all analysis
###########################################################################################


## General
# Set working directory for the repository (should be the git repo):
wd <-
  "/Users/avahoffman/Dropbox/Research/EDGE_Science/EDGE-science/"
# Directories for specific outputs/inputs:
data_dir <- paste(toString(wd), "data/", sep = "")
figure_dir <- paste(toString(wd), "figures/", sep = "")
write_dir <- paste(toString(wd), "output/", sep = "")

# Ecoregion colors (bold) (will be reused to stay theme-consistent):
shortgrass_color <- "#CC0000"
northern_color <- "#0000CC"
desert_color <- "#CC9900"
# Ecoregion colors (pale):
shortgrass_color_pale <- "#F5D6D6"
northern_color_pale <- "#D4E0F7"
desert_color_pale <- "#FFE6CC"


###########################################################################################
## Map
# Colors for points representing these sites
CHY_color <- "#CC0000"
SGS_color <- "#0000CC"
SEV_Blue_color <- "#00CC33"
SEV_Black_color <- "#FF9900"

# Directory where map shapefiles representing ecoregions should be:
shapefile_dir <- paste(toString(wd), "data/shapefiles/", sep = "")
# Names for ecoregions:
shortgrass_name <- "Shortgrass Steppe"
northern_name <- "Northern Mixed Grass"
desert_name <- "Desert Grassland"
# Colors for ecoregions:
climate_colors <-
  c(desert_color_pale,
    northern_color_pale,
    shortgrass_color_pale)


###########################################################################################
## C4 vs C3 grass coverage
# Sites to include:
c4_c3_sites <- c("CHY", "SGS")
# C4 or C3 Grasses to include:
c4_grasses <- c("BOGR", "BOHI", "BUDA", "C4")
c3_grasses <- c("CAREX", "PASM", "C3")
# Years to include:
c3_c4_years <- c(2017, 2016, 2015, 2014)
# Drought treatments to include in effect:
include_in_drt_trt <- c("chr")


###########################################################################################
## Bouteloua eripoda v gracilis coverage at Sevilleta sites 
# Sites to include:
eri_grac_sites <- c("SEV.blue", "SEV.black")
# Years to include:
eri_grac_years <- c(2017, 2016, 2015, 2014)
# Grasses to include, especially in "other C4":
eri_grac_grasses <- c(
  "ARAD",
  "ARPU",
  "BOBA2",
  "BOER4",
  "BOGR",
  "DAPU7",
  "ENDE",
  "MUAR2",
  "MUSQ3",
  "PLJA",
  "SCBR2",
  "SPCO4",
  "SPCR",
  "SPFL2",
  "SPORO",
  "ANGE",
  "BOCU",
  "BOHI",
  "BUDA",
  "SPAS",
  "C4"
)


###########################################################################################
## Genetic comparison
# Very important not to mess around with these unless cross validation has been performed!!
genetic_data <-
  paste(toString(wd), "data/genetic/genind_all.R", sep = "")
genetic_pops_to_use <- c("SGS", "Sevilleta", "Konza", "Cedar Point")
DAPC_colors_bold <-
  c(shortgrass_color, northern_color, "black", "darkgrey") # order of pops and colors will be matched
DAPC_colors_pale <-
  c(shortgrass_color_pale,
    northern_color_pale,
    "grey",
    "lightgrey")
XV_skip <- TRUE
n_prin_comp <- 15
