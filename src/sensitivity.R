###########################################################################################
# Set working directory
setwd("/Users/avahoffman/Dropbox/Research/EDGE_Science/EDGE-science/")
source("src/config.R")
source("src/utils.R")
setwd(wd)

###########################################################################################
# Load libraries
library(ggplot2)
library(dplyr)

###########################################################################################

get_huxman_2004_data <- function() {
  setwd(data_dir)
  huxman_dat <- read.csv("huxman_2004.csv")
  
  return(huxman_dat)
}


get_slope <- function(df, sitename) {
  # Calculate the slope of the production / precip relationship
  est <- summary(glm(
    formula = anpp_gm.2 ~ precip_mm,
    data = df %>% filter(site == sitename)
  ))$coefficients[2]
  return(est)
}


get_edge_data <- function(){
  setwd(data_dir)
  edge_dat <- drop_na( read.csv("precip.csv") )
  
  # Gather slopes
  CHY_est <- get_slope(edge_dat, "CHY")
  SBK_est <- get_slope(edge_dat, "SBK")
  SBL_est <- get_slope(edge_dat, "SBL")
  SGS_est <- get_slope(edge_dat, "SGS")
  
  # Concatenate slopes
  slopes <- c(CHY_est, SBK_est, SBL_est, SGS_est)
  
  # Get average precip for plotting edge dots
  precip <- edge_dat %>% group_by(site) %>% summarise(MAP = mean(precip_mm))
  
  # datapoints
  edge_dat <- cbind(precip, slopes)
  setwd(wd)
  return(edge_dat)
}


make_sensitivity_plot <- function(huxman_dat, edge_dat, filename = NA) {
  gg <- ggplot() +
    
    # Add panel for arid sites
    geom_rect(aes(
      xmin = 0,
      xmax = 500,
      ymin = -0.2,
      ymax = 1
    ),
    fill = desert_color_pale) +
    
    # Add points
    geom_point(data = huxman_dat,
               aes(x = map, y = slope),
               color = "black") +
    
    # Style and axes
    theme_sigmaplot() +
    ylab("Sensitivity") +
    xlab("MAP (mm yr-1)") +
    scale_x_continuous(
      limits = c(0, 2650),
      expand = c(0, 0),
      breaks = c(0, 500, 1000, 1500, 2000, 2500),
      sec.axis = dup_axis(labels = NULL, name = "")
    ) +
    scale_y_continuous(
      limits = c(-0.2, 1.0),
      expand = c(0, 0),
      breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
      sec.axis = dup_axis(labels = NULL, name = "")
    ) +
    # Remove awkward tick mark on axis
    theme(axis.ticks.x = element_line(
      color = c(
        "transparent",
        "black",
        "black",
        "black",
        "black",
        "black",
        "black"
      )
    ),
    axis.ticks.y = element_line(
      color = c("black", "black", "black", "black", "black", "transparent")
    )) +
    
    # Add trendline
    geom_smooth(
      data = huxman_dat,
      aes(x = map, y = slope),
      method = "glm",
      formula = y ~ log(x),
      se = FALSE,
      color = "black"
    ) +
    
    # Add site points
    geom_point(data = edge_dat %>% filter(site == "SGS"),
               aes(x = MAP, y = slopes), size = 2, color = SGS_color) + ## SGS
    geom_point(data = edge_dat %>% filter(site == "SBL"),
               aes(x = MAP, y = slopes), size = 2, color = SEV_Blue_color) + ## SBL
    geom_point(data = edge_dat %>% filter(site == "SBK"),
               aes(x = MAP, y = slopes), size = 2, color = SEV_Black_color) + ## SBK
    geom_point(data = edge_dat %>% filter(site == "CHY"),
               aes(x = MAP, y = slopes), size = 2, color = CHY_color) ## CHY
  
  
  gg
  if (!(is.na(filename))) {
    ggsave(file = filename,
           height = 3,
           width = 4)
  }
  return(gg)
}
