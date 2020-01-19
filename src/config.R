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

## Colors

# Ecoregion labels:
shortgrass_label_color <- "#41ab5d"
northern_label_color <- "#08519c"
desert_label_color <- "#cb181d"

# Ecoregion colors (pale):
shortgrass_color <- "#e5f5e0"
northern_color <- "#deebf7"
desert_color <- "#fee0d2"

# Colors for points representing these sites
CHY_color <- "#08519c"
SGS_color <- "#41ab5d"
SEV_Blue_color <- "#fd8d3c"
SEV_Black_color <- "#cb181d"

# Type colors
C4_color <- "white"
C3_color <- "#deebf7"
eriopoda_color <- "#fb6a4a"
gracilis_color <- "#fdae6b"
sev_grac_color <- "#f16913"
sgs_grac_color <- "#fdae6b"

# Drought treatments to include in effect:
include_in_drt_trt <- c("chr")


###########################################################################################
## Map and Huxman data with inset (sensitivity)

# Directory where map shapefiles representing ecoregions should be:
shapefile_dir <- paste(toString(wd), "data/shapefiles/", sep = "")

# Names for ecoregions:
shortgrass_name <- "Shortgrass Steppe"
northern_name <- "Northern Mixed Grass"
desert_name <- "Desert Grassland"

# Colors for ecoregions:
climate_colors <-
  c(desert_color,
    northern_color,
    shortgrass_color)

# Years for inset sensitivity
sensitivity_years <- c(2017, 2016, 2015, 2014)
sensitivity_sites <- c("CHY","SGS","SEV.blue","SEV.black")

# Figure details
#y_lab_inset <- "Percent Decline in Total ANPP"
y_lab_inset <- ""
x_ticks_inset <- c("SEV Black", "SEV Blue", "SGS", "CHY")


###########################################################################################
## C4 vs C3 grass coverage

# Sites to include:
c4_c3_sites <- c("CHY", "SGS")

# C4 or C3 Grasses to include:
c4_grasses <- c("BOGR", "BOHI", "BUDA", "C4")
c3_grasses <- c("CAREX", "PASM", "C3")

# Years to include:
c3_c4_years <- c(2017, 2016, 2015, 2014)

# Figure details
x_ticks_3 <- c("CHY","SGS")
y_lab_3 <- "Percent of Grass ANPP"
legend_names_3 <- c("C3 Grasses", "C4 Grasses")
y_lab_4 <- "Percent Change in ANPP"


###########################################################################################
## Bouteloua eripoda v gracilis coverage at Sevilleta sites 

# Sites to include:
eri_grac_sites <- c("SEV.blue", "SEV.black")

# Years to include:
eri_grac_years <- c(2017, 2016, 2015, 2014)


# Figure details
x_ticks_5 <- c("SEV Blue", "SEV Black")
y_lab_5 <- "Percent of Grass ANPP"
legend_names_5 <- c(expression(~italic("B. eriopoda")),
                    expression(~italic("B. gracilis")),
                    "Other C4")
y_lab_6 <- "Percent of Change in ANPP"
legend_names_6 <- c(expression(~italic("B. eriopoda")),
                   expression(~italic("B. gracilis")))


###########################################################################################
## Bouteloua gracilis comparison at SGS and Sevilleta sites 

# Sites to include:
sev_sgs_sites <- c("SEV.blue", "SGS")

# Years to include:
sev_sgs_years <- c(2017, 2016, 2015, 2014)

# Figure details
x_ticks_7 <- c("SEV Blue", "SGS")
y_lab_7 <- "Percent of Grass ANPP"
legend_names_7 <- c(expression(~italic("B. gracilis")),
                    "C3 Grasses",
                    "Other C4")
y_lab_8 <- "Percent Change in ANPP"

######################################################s#####################################
## Genetic comparison
# Very important not to mess around with these unless cross validation has been performed!!
genetic_data <-
  paste(toString(wd), "data/genetic/genind_all.R", sep = "")
genetic_pops_to_use <- c("SGS", "Sevilleta", "Konza", "Cedar Point")
DAPC_colors_pale <-
  c(sgs_grac_color,
    sev_grac_color,
    "grey",
    "lightgrey")
XV_skip <- TRUE
n_prin_comp <- 15

# Figure details
x_lab_9 <- "LD1"
y_lab_9 <- "LD2"
legend_names_9 <- c("NB", "KNZ", "SEV Blue", "SGS")
y_lab_10 <- expression(paste(italic("B. gracilis"), " Biomass (g)"))
x_ticks_10 <- c("Dry", "Wet")
legend_names_10 <- c("SEV Blue", "SGS")

