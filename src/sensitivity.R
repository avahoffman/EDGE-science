# This script examines the sensitivity of different EDGE sites, with respect to data
# from the Huxman et al. 2004 publication.
###########################################################################################
# Load libraries
library(ggplot2)
library(dplyr)

###########################################################################################


get_slope <- function(df, sitename) {
  # Calculate the slope of the production / precip relationship
  est <- summary(glm(
    formula = anpp_gm.2 ~ precip_mm,
    data = df %>% filter(site == sitename)
  ))$coefficients[2]
  
  return(est)
}


get_edge_data <- function() {
  # Get anpp vs precip slope per site, and mean precip per site
  edge_dat <- drop_na(precip_dat)
  
  # Gather slopes
  CHY_est <- get_slope(edge_dat, "CHY")
  SBK_est <- get_slope(edge_dat, "SBK")
  SBL_est <- get_slope(edge_dat, "SBL")
  SGS_est <- get_slope(edge_dat, "SGS")
  
  # Concatenate slopes
  slopes <-
    c(CHY_est,
      SBK_est,
      SBL_est,
      SGS_est)
  
  # Get average precip for plotting edge dots
  precip <-
    edge_dat %>%
    group_by(site) %>%
    summarise(MAP = mean(precip_mm))
  
  # datapoints
  edge_dat <- cbind(precip, slopes)
  
  return(edge_dat)
}


get_percent_decline <- function(sum_across_years = TRUE) {
  # Sum across years option decides whether each year is a data point for site (FALSE), plot
  # OR whether to add up all the years cumulatively first (TRUE)
  # Keep only sites, years, and treatments of interest:
  dat <- filter_and_clean_raw_data(bio_dat,
                                   sites = sensitivity_sites,
                                   years = sensitivity_years)
  
  # Group to plot level by year, site, treatment
  by_plot_sensitivity <-
    dat %>%
    filter(!(category %in% total_bio_exlusions)) %>%
    group_by(Site,
             Block,
             Plot,
             Year,
             Trt) %>%
    summarise(biomass = sum(biomass))
  
  # IF cumulative, sum across all years to get a more stable number
  if (sum_across_years) {
    full_dat <-
      by_plot_sensitivity %>%
      group_by(Site,
               Block,
               Plot,
               Trt) %>%
      summarise(biomass = sum(biomass) / length(sensitivity_years))
  } else {
    full_dat <-
      by_plot_sensitivity
  }
  
  # Join tables
  compare_dat <-
    full_join(
      # Ambient data
      full_dat %>%
        filter(Trt == "con"),
      
      # Take the mean of drt treatments (including chr and int)
      full_dat %>%
        group_by(Site,
                 Block,
                 Trt) %>%
        summarise(biomass = mean(biomass)) %>%
        filter(Trt == "drt"),
      
      # Join
      by = c("Site", "Block")
    )
  
  # Calculate the difference
  compare_dat$diff <-
    100 * (compare_dat$biomass.y - compare_dat$biomass.x) / compare_dat$biomass.x
  
  # Summarize by site
  summary_dat <-
    compare_dat %>%
    group_by(Site) %>%
    summarise(mean = mean(diff),
              se = sd(diff) / sqrt(n()),
              type = "diff")
  
  return(summary_dat)
}


make_sensitivity_plot <-
  function(huxman_dat,
           edge_dat,
           filename = NA) {
    gg <- ggplot() +
      
      # Add panel for arid sites
      geom_rect(aes(
        xmin = 0,
        xmax = 500,
        ymin = -0.2,
        ymax = 1
      ),
      fill = desert_color) +
      
      # Add points
      geom_point(data = huxman_dat,
                 aes(x = map, y = slope),
                 color = "black") +
      
      # Style and axes
      theme_sigmaplot() +
      ylab("Sensitivity") +
      xlab(expression(paste("MAP (mm y", r ^ {
        -1
      }, ")"))) +
      scale_x_continuous(
        limits = c(0, 2650),
        expand = c(0, 0),
        breaks = c(0,
                   500,
                   1000,
                   1500,
                   2000,
                   2500),
        sec.axis = dup_axis(labels = NULL, name = "")
      ) +
      scale_y_continuous(
        limits = c(-0.2, 1.0),
        expand = c(0, 0),
        breaks = c(0,
                   0.2,
                   0.4,
                   0.6,
                   0.8,
                   1.0),
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
        color = c("black",
                  "black",
                  "black",
                  "black",
                  "black",
                  "transparent")
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
      
      geom_point(
        data = edge_dat %>%
          filter(site == "SGS"),
        aes(x = MAP, y = slopes),
        size = 2,
        color = SGS_color
      ) + ## SGS
      
      geom_point(
        data = edge_dat %>%
          filter(site == "SBL"),
        aes(x = MAP, y = slopes),
        size = 2,
        color = SEV_Blue_color
      ) + ## SBL
      
      geom_point(
        data = edge_dat %>%
          filter(site == "SBK"),
        aes(x = MAP, y = slopes),
        size = 2,
        color = SEV_Black_color
      ) + ## SBK
      
      geom_point(
        data = edge_dat %>%
          filter(site == "CHY"),
        aes(x = MAP, y = slopes),
        size = 2,
        color = CHY_color
      ) ## CHY
    
    gg
    if (!(is.na(filename))) {
      ggsave(file = filename,
             height = 3,
             width = 4)
    }
    return(gg)
  }


make_inset_decline_plot <- function(summary_dat,
                                    filename = NA) {
  # Add index for order
  summary_dat$position <- as.factor(c(4, 1, 2, 3))
  
  gg <- ggplot(data = summary_dat) +
    
    # Add zero line
    geom_hline(yintercept = 0, lty = "11") +
    
    # Draw bars - multiply by -1 to get % DECLINE
    geom_bar(
      aes(y = mean * -1, x = position, fill = position),
      stat = "identity",
      position = position_stack(reverse = TRUE),
      color = "black",
      width = 0.5,
    ) +
    
    # Add standard error
    geom_errorbar(
      data = summary_dat,
      aes(
        x = position,
        ymin = mean * -1 - se,
        ymax = mean * -1 + se
      ),
      size = 0.5,
      width = 0
    ) +
    
    # Add site colors to bars
    scale_fill_manual(values = c(SEV_Black_color,
                                 SEV_Blue_color,
                                 SGS_color,
                                 CHY_color)) +
    
    # Add theme and adjust axes
    theme_sigmaplot(xticks = FALSE, 
                    ticklen = -0.15) +
    scale_y_continuous(
      limits = c(-20, 105),
      breaks = c(0, 50, 100),
      expand = c(0, 0),
      labels = c(0, 50, 100),
      sec.axis = dup_axis(labels = NULL, name = "")
    ) +
    theme(axis.ticks.y = element_line(color = c("transparent",
                                                "black",
                                                "black"))) +
    
    ylab(y_lab_inset) +
    xlab(NULL) +
    
    # Adjust legend and colors
    theme(legend.position = "none") +
    scale_x_discrete(labels = x_ticks_inset) +
    
    # Add margins
    theme(plot.margin = unit(c(0,0,0,1), "cm"))
    
    # Remove white background and border on inset
    #theme(rect = element_rect(fill = "transparent"),
          #plot.background = element_rect(color = "transparent"))
  
  gg
  if (!(is.na(filename))) {
    ggsave(file = filename,
           height = 3,
           width = 4)
  }
  return(gg)
}
