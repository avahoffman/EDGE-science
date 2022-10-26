# This script examines the precipitation binned up by northern (CHY and SGS) and 
# southern (SEV Blue and SEV Black) sites
###########################################################################################
# Load libraries
library(ggplot2)
library(dplyr)

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
  
  ggplot() +
    
    # Add points
    geom_point(
      data = p_dat,
      aes(x = ppt, y = anpp, color = region)
      ) +
    
    # Add trendlines
    geom_smooth(
      data = p_dat,
      aes(x = ppt, y = anpp, group = region, color = region),
      method = "glm",
      formula = y ~ x,
      se = FALSE
    ) +
    
    # Style and axes
    theme_sigmaplot() +
    ylab(expression(paste("ANPP (g ", m ^ {
      -2
    }, ")"))) +
    xlab(expression(atop("Growing season precipitation", 
                         paste("(mm y", r ^ {-1}, ")")))) +

    # Add appropriate colors 
    scale_color_manual(values = c(CHY_color, SEV_Black_color)) +
    
    # Facet by region
    facet_wrap(
      ~ region, ncol = 1
    ) +
    
    # Remove legend
    theme(legend.position = "none") 
  
}
