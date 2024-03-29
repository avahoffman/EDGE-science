# Config file for EDGE project
# Includes relevant constants for all analysis
###########################################################################################

## Colors

# Ecoregion labels:
shortgrass_label_color <- "#41ab5d"
northern_label_color <- "#08519c"
desert_label_color <- "#cb181d"

# Ecoregion colors (pale):
shortgrass_color <- "#e5f5e0"
northern_color <- "#deebf7"
desert_color <- "#fee0d2"

# Colors for points representing these sites on the map:
CHY_color <- "#08519c"
SGS_color <- "#08519c"
SEV_Blue_color <- "#fb6a4a"
SEV_Black_color <- "#fb6a4a"

# Type colors:
C4_color <- "white"
C3_color <- "#c9dff2"
eriopoda_color <- "#fb6a4a"
gracilis_color <- "#fdae6b"
sev_grac_color <- "#f16913"
sgs_grac_color <- "#fdae6b"

# Drought treatments to include in effect:
include_in_drt_trt <- c("chr")

## Time

# Years to take into account:
experiment_years <-
  c(2017,
    2016,
    2015,
    2014)

###########################################################################################
## Data inputs / sources

# Directory where map shapefiles representing ecoregions should be:
shapefile_dir <-
  paste(toString(wd),
        "/data/shapefiles/",
        sep = "")

taxa_dat <- 
  read.csv("data/Taxa_info.csv")

precip_dat <- 
  read.csv("data/precip.csv")

# !Note: precip_dat_2 reflects updated and corrected data received from 
# Ingrid Slette (October 2022)
precip_dat_2 <- 
  read.csv("data/EDGE_North_South_PPT_ANPP.csv")

huxman_dat <- 
  read.csv("data/huxman_2004.csv")

biomass_dat <- 
  read.csv("data/genetic/gracilis_traits.csv")

genetic_data <-
  paste(toString(wd),
        "/data/genetic/genind_all.R",
        sep = "")


###########################################################################################
## Data outputs

# Where to write stats output:
statsfile <- 
  "output/statistical/tests.txt"

# Where to write the DAPC loadings:
DAPC_loadings_write_name <- 
  "output/genetic/DAPC_loadings.csv"

# Where to write variance and prin comps used:
DAPC_stat_write_name <- 
  "output/genetic/statistics.txt"

# Figure directory:
figure_dir <-
  paste(toString(wd),
        "/figures/",
        sep = "")

###########################################################################################
## Map and Huxman data with inset (sensitivity)

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
sensitivity_years <- experiment_years

sensitivity_sites <-
  c("CHY",
    "SGS",
    "SEV.blue",
    "SEV.black")

# Figure details
y_lab_inset <- "Percent decline in total ANPP"
y_lab_inset <- NULL
x_ticks_inset <-
  c("SEV\nBlack",
    "SEV\nBlue",
    "SGS",
    "CHY")

# Exclude woody spp here if needed (eg., "WOODY" or "PYD")
total_bio_exlusions <- c("PYD")
exclude_SGS_2016_plot2 <- TRUE

# Exclude 2013 from precip record? Scatterplot only.
exclude_2013 <- TRUE

###########################################################################################
## C4 vs C3 grass coverage

# Sites to include:
c3_c4_sites <- c("CHY", "SGS")

# C4 or C3 Grasses to include:
c4_grasses <-
  c("BOGR",
    "BOHI",
    "BUDA",
    "C4")
c3_grasses <-
  c("CAREX",
    "PASM",
    "C3")

# Years to include:
c3_c4_years <- experiment_years

# Figure details
x_ticks_3 <- c("CHY", "SGS")
y_lab_3 <- "% of Grass ANPP"
legend_names_3 <- c("C3 Grasses", "C4 Grasses")
legend_names_3_with_bogr <- c(expression(~ italic("B. gracilis")), "C3", "Other C4")
legend_names_3_with_bogr_diff <- c(expression(~ italic("B. gracilis")), "C3 Grasses")
y_lab_4 <- "% Change in ANPP"


###########################################################################################
## Bouteloua eripoda v gracilis coverage at Sevilleta sites

# Sites to include:
eri_grac_sites <- c("SEV.blue", "SEV.black")

# Years to include:
eri_grac_years <- experiment_years

# Figure details
x_ticks_5 <- c("SEV Black", "SEV Blue")
y_lab_5 <- "% of Grass ANPP"
legend_names_5 <-
  c(expression(~ italic("B. eriopoda")),
    expression(~ italic("B. gracilis")),
    "Other C4")
y_lab_6 <- "% Change in ANPP"
legend_names_6 <-
  c(expression(~ italic("B. eriopoda")),
    expression(~ italic("B. gracilis")))


###########################################################################################
## Bouteloua gracilis comparison at SGS and Sevilleta sites

# Sites to include:
sev_sgs_sites <- c("SEV.blue", "SGS")

# Years to include:
sev_sgs_years <- experiment_years

# Figure details
x_ticks_7 <- c("SEV Blue", "SGS")
y_lab_7 <- "% of Grass ANPP"
legend_names_7 <-
  c(expression( ~ italic("B. gracilis")),
    "C3 Grasses",
    "Other C4")
y_lab_8 <-
  expression(paste("% Change in ",
                   italic("B. gracilis"),
                   " Biomass (g)"))

######################################################s#####################################
## Genetic comparison
# Very important not to mess around with these unless cross validation has been performed!!

genetic_pops_to_use <-
  c("SGS",
    "Sevilleta",
    "Konza",
    "Cedar Point")

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
legend_names_9 <-
  c("HP",
    "MP",
    "SEV Blue",
    "SGS")
y_lab_10 <-
  expression(paste(italic("B. gracilis"),
                   " biomass (g)"))
y_lab_10_ab <-
  expression(paste(italic("B. gracilis"),
                   " above:below biomass ratio"))
x_ticks_10 <- c("Dry", "Wet")
legend_names_10 <- c("SEV Blue", "SGS")
