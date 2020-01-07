###########################################################################################
## set working directory
setwd("/Users/avahoffman/Dropbox/Research/EDGE_Science/EDGE-science/src/")
source("config.R")
source("utils.R")
setwd(wd)

###########################################################################################
## load libraries
library(ggplot2)

###########################################################################################

get_huxman_2004_data <- function() {
  setwd(data_dir)
  huxman_dat <- read.csv("huxman_2004.csv")
  return(huxman_dat)
}


make_sensitivity_plot <- function(huxman_dat) {
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
    )
  
  
  gg
}