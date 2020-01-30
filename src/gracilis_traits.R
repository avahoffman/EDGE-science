# This script analyzes and plots Bouteloua gracilis traits from both SGS and Sevilleta 
# sites.
###########################################################################################
# Load libraries
library(ggplot2)
library(dplyr)

###########################################################################################


load_and_clean_trait_data <- function() {
  # Load data
  biomass_dat <- 
    read.csv("data/genetic/gracilis_traits.csv")
  
  # Calculate total biomass trait
  biomass_dat$total <-
    biomass_dat$biomass_aboveground +
    biomass_dat$biomass_belowground +
    biomass_dat$biomass_rhizome +
    biomass_dat$flwr_mass_lifetime
  
  # Summarize data by population and treatment
  summary_dat <- 
    biomass_dat %>% 
    group_by(pop, trt) %>%
    summarise(mean = mean(total),
              se = sd(total) / sqrt(n()))
  
  return(summary_dat)
}


plot_traits <- function(summary_dat, filename = NA) {
  gg <- ggplot(data = summary_dat) +
    
    # Add standard error first (to fall behind the points)
    geom_errorbar(
      position = position_dodge(width = 0.1),
      data = summary_dat,
      aes(
        group = pop,
        x = trt,
        ymin = mean - se,
        ymax = mean + se
      ),
      size = 0.5,
      width = 0
    ) +
    
    # Connect points with a line
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
      aes(fill = pop, 
          y = mean, 
          x = trt),
      color = "black",
      shape = 21,
      size = 4,
      stat = "identity",
      position = position_dodge(0.1)
    ) +
    
    # Add theme and adjust axes
    theme_sigmaplot(xticks = FALSE) +
    scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = "")) +
    ylab(y_lab_10) +
    xlab(NULL) +
    
    # Adjust legend and colors
    
    # Legend positioning
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_blank()
    ) +
    
    # Point color
    scale_fill_manual(
      values = c(sev_grac_color, 
                 sgs_grac_color),
      labels = legend_names_10
    ) +
    
    # Slope (connecting) line color
    scale_color_manual(
      values = c(sev_grac_color, 
                 sgs_grac_color),
      labels = legend_names_10
    ) +
    
    # X axis category names
    scale_x_discrete(labels = x_ticks_10)
  
  gg
  if (!(is.na(filename))) {
    ggsave(file = filename,
           height = 5,
           width = 3.5)
  }
  return(gg)
}
