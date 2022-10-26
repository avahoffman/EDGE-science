# This script examines the precipitation binned up by northern (CHY and SGS) and 
# southern (SEV Blue and SEV Black) sites
###########################################################################################
# Load libraries
library(ggplot2)
library(dplyr)
library(ggpubr)

###########################################################################################

get_precip_data <- function(exclude_2013_ = exclude_2013){
 
  # Drop 2013 if relevant (it is a strange post-extreme-drought year)
  if(exclude_2013_){
    p_dat <- precip_dat_2 %>% filter(year != 2013)
  } else {
    p_dat <- precip_dat_2
  }
  
  return(p_dat)
}


plot_precip_data <- function(p_dat){
  # Get data for plotting
  p_dat <- get_precip_data()
  
  ggplot(data = p_dat,
         aes(x = ppt, y = anpp)) +
    
    # Add points
    geom_point(aes(color = region)) +
    
    # Add trendlines
    geom_smooth(
      aes(
        group = region,
        color = region
      ),
      method = "glm",
      formula = y ~ x,
      se = FALSE
    ) +
    
    # Style and axes
    theme_sigmaplot() +
    ylab(expression(paste("ANPP (g ", m ^ {
      -2
    }, ")"))) +
    xlab(expression(atop(
      "Growing season precipitation",
      paste("(mm y", r ^ {
        -1
      }, ")")
    ))) +
    
    # Regression formula on plot
    stat_regline_equation(data = p_dat[(p_dat$region == "North"),],
                          label.x = 70,
                          label.y = 195,
                          aes(label = ..eq.label.., color = region),
                          size = 3) +
    stat_regline_equation(data = p_dat[(p_dat$region == "South"),],
                          label.x = 200,
                          label.y = 80,
                          aes(label = ..eq.label.., color = region),
                          size = 3) +
    
    # R2 values
    stat_regline_equation(data = p_dat[(p_dat$region == "North"),],
                          label.x = 70,
                          label.y = 175,
                          aes(label = ..rr.label.., color = region),
                          size = 3) +
    stat_regline_equation(data = p_dat[(p_dat$region == "South"),],
                          label.x = 200,
                          label.y = 60,
                          aes(label = ..rr.label.., color = region),
                          size = 3) +
    
    # Add appropriate colors
    scale_color_manual(values = c(CHY_color, SEV_Black_color)) +
    
    # Facet by region
    facet_wrap(~ region, ncol = 1) +
    
    # Remove legend
    theme(legend.position = "none")
  
}
