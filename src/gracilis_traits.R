###########################################################################################
## set working directory
setwd("/Users/hoffman ava/EDGE-science/")
#setwd("/Users/avahoffman/Dropbox/Research/EDGE_Science/EDGE-science/")
source("src/config.R")
source("src/utils.R")
setwd(wd)

###########################################################################################
## load libraries
library(ggplot2)
library(dplyr)

###########################################################################################


load_and_clean_trait_data <- function() {
  # Load data
  biomass_dat <- read.csv("data/genetic/gracilis_traits.csv")
  # Calculate total biomass
  biomass_dat$total <-
    biomass_dat$biomass_aboveground +
    biomass_dat$biomass_belowground +
    biomass_dat$biomass_rhizome +
    biomass_dat$flwr_mass_lifetime
  # Summarize
  summary_dat <- biomass_dat %>% group_by(pop, trt) %>%
    summarise(mean = mean(total),
              se = sd(total) / sqrt(n()))
  
  return(summary_dat)
}


plot_traits <- function(summary_dat, filename = NA) {
  gg <- ggplot(data = summary_dat) +
    
    # Add standard error first
    geom_errorbar(
      position = position_dodge(width = 0.1),
      data = summary_dat,
      aes(
        group = pop,
        x = trt,
        ymin = mean - se,
        ymax = mean + se
      ),
      size = 1,
      width = 0
    ) +
    
    # Connect points
    stat_summary(
      aes(
        y = mean,
        x = trt,
        group = pop,
        color = pop
      ),
      geom = "line",
      position = position_dodge(0.1)
    ) +
    
    # Draw points
    geom_point(
      aes(fill = pop, y = mean, x = trt),
      color = "black",
      shape = 21,
      size = 4,
      stat = "identity",
      position = position_dodge(0.1)
    ) +
    
    # Add theme and adjust axes
    theme_sigmaplot(xticks = FALSE) +
    scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = "")) +
    ylab("B. gracilis Biomass (g)") +
    xlab(NULL) +
    
    # Adjust legend and colors
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_blank()
    ) +
    scale_fill_manual(
      values = c(northern_color_pale, shortgrass_color_pale),
      labels = c("SEV", "SGS")
    ) +
    scale_color_manual(values = c(northern_color_pale, shortgrass_color_pale)) +
    scale_x_discrete(labels = c("Dry", "Wet"))
  
  gg
  if (!(is.na(filename))) {
    ggsave(file = filename,
           height = 5,
           width = 3.5)
  }
  return(gg)
}
