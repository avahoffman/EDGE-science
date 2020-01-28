# This script compares the composition of C3 and C4 grasses at CHY and SGS sites.
###########################################################################################
# Load libraries
library(dplyr)
library(ggplot2)

###########################################################################################


collect_c3_c4_data <-
  function(sum_across_years = TRUE) {
    # Sum across years option decides whether each year is a data point for site (FALSE), plot
    # OR whether to add up all the years cumulatively first (TRUE)
    
    setwd(data_dir)
    raw_dat <- read.csv("EDGE_biomass_long_QAQC_final.csv")
    
    # Filter out old years
    dat <- raw_dat[(raw_dat$Year %in% 
                      c3_c4_years), ]
    
    # Keep only CHY, SGS
    dat <- dat[(dat$Site %in% 
                  c4_c3_sites), ]
    
    # Lump all experimental droughts into drought (if want to exclude one or the other, see config)
    dat <- dat[(dat$Trt %in% 
                  c(include_in_drt_trt, "con")), ]
    dat <- 
      dat %>%
      mutate(Trt = as.character(Trt)) %>% 
      mutate(Trt = replace(Trt, 
                           Trt == "chr" |
                             Trt == "int", "drt"))
    
    # Collect only C4 grasses
    c4_dat <- as_tibble(dat[(dat$category %in% 
                               c4_grasses), ])
    # Collect only C3 grasses
    c3_dat <- as_tibble(dat[(dat$category %in% 
                               c3_grasses), ])
    
    # Group to plot level by year, site, treatment
    by_plot_c4 <-
      c4_dat %>% 
      group_by(Site, Block, Plot, Year, Trt) %>% 
      summarise(c4_biomass = sum(biomass))
    by_plot_c3 <-
      c3_dat %>% 
      group_by(Site, Block, Plot, Year, Trt) %>% 
      summarise(c3_biomass = sum(biomass))
    # Join tables
    full_dat <-
      full_join(by_plot_c4,
                by_plot_c3,
                by = c("Site", "Block", "Plot", "Year", "Trt"))
    
    # Filter if there is no grass whatsoever
    full_dat <- 
      full_dat %>% 
      filter(c4_biomass > 0 & 
               c3_biomass > 0)
    
    # IF cumulative, sum across all years to get a more stable number
    if (sum_across_years) {
      full_dat <-
        full_dat %>% 
        group_by(Site, Block, Plot, Trt) %>% 
        summarise(
          c3_biomass = sum(c3_biomass) / length(c3_c4_years),
          c4_biomass = sum(c4_biomass) / length(c3_c4_years)
        )
    }
    
    # Calculate percentage C3 and C4 grass
    full_dat$c3_pct <-
      100 * full_dat$c3_biomass / (full_dat$c3_biomass + full_dat$c4_biomass)
    full_dat$c4_pct <-
      100 * full_dat$c4_biomass / (full_dat$c3_biomass + full_dat$c4_biomass)
    
    setwd(wd)
    return(full_dat)
  }


summarize_ambient_data <- function(full_dat) {
  # Keep only control plots
  full_dat <- 
    full_dat %>% 
    filter(Trt == "con")
  
  # Perform T.test
  chy <- 
    full_dat %>% 
    filter(Site == "CHY") %>% 
    pull(c3_pct)
  sgs <- 
    full_dat %>% 
    filter(Site == "SGS") %>% 
    pull(c3_pct)
  
  # Run test and write results
  sink("output/statistical/tests.txt", append = TRUE)
  print("T test of true difference in c3 percent (CHY vs SGS) is not equal to 0")
  ttest_with_var_check(chy, sgs)
  sink()
  
  # Summarize by site
  summary_dat <- 
    full_dat %>% 
    group_by(Site) %>%
    summarise(
      mean = mean(c3_pct),
      se = sd(c3_pct) / sqrt(n()),
      type = "c3"
    )
  
  # Repeat data for inverse of C3 percent (so that C3 and C4 bars can be stacked in the plot)
  summary_dat <- 
    rbind(
    summary_dat,
    full_dat %>% 
      group_by(Site) %>%
      summarise(
        mean = 100 - mean(c3_pct),
        se = sd(c3_pct) / sqrt(n()),
        type = "c4"
      )
  )
  
  setwd(wd)
  return(summary_dat)
}


summarize_difference_data <- function(full_dat) {
  # Ambient first
  full_dat_amb <- 
    full_dat %>% 
    filter(Trt == "con")
  # Take the mean of drt treatments (including chr and int)
  full_dat_drt <- 
    full_dat %>% 
    group_by(Site, Block, Trt) %>%
    summarise(c3_biomass = mean(c3_biomass),
              c4_biomass = mean(c4_biomass),
    ) %>% 
    filter(Trt == "drt")
  # Join tables
  compare_dat <-
    full_join(full_dat_amb, 
              full_dat_drt, 
              by = c("Site", "Block"))
  
  # Calculate the difference
  compare_dat$c3_diff <-
    100 * (compare_dat$c3_biomass.y - compare_dat$c3_biomass.x) / compare_dat$c3_biomass.x
  compare_dat$c4_diff <-
    100 * (compare_dat$c4_biomass.y - compare_dat$c4_biomass.x) / compare_dat$c4_biomass.x
  
  # Perform T.tests
  chy_c3 <- 
    compare_dat %>% 
    filter(Site == "CHY") %>% 
    pull(c3_diff)
  chy_c4 <- 
    compare_dat %>% 
    filter(Site == "CHY") %>% 
    pull(c4_diff)
  sgs_c3 <- 
    compare_dat %>% 
    filter(Site == "SGS") %>% 
    pull(c3_diff)
  sgs_c4 <- 
    compare_dat %>% 
    filter(Site == "SGS") %>% 
    pull(c4_diff)
  
  # Run test and write results
  sink("output/statistical/tests.txt", append = TRUE)
  
  print("T test of true difference in ANPP change (C3 vs C4 at CHY) is not equal to 0")
  ttest_with_var_check(chy_c3, chy_c4)
  
  print("T test of true difference in ANPP change (C3 vs C4 at SGS) is not equal to 0")
  ttest_with_var_check(sgs_c3, sgs_c4)
  
  print("T test of mean ANPP change (C3 at SGS) is not equal to 0")
  print(t.test(sgs_c3, alternative = "two.sided"))
  
  print("T test of mean ANPP change (C4 at SGS) is not equal to 0")
  print(t.test(sgs_c4, alternative = "two.sided"))
  
  sink()
  
  # Summarize by site
  summary_dat <- 
    compare_dat %>% 
    group_by(Site) %>%
    summarise(
      mean = mean(c3_diff),
      se = sd(c3_diff) / sqrt(n()),
      type = "c3"
    )
  
  # Repeat data for C4
  summary_dat <- 
    rbind(
    summary_dat,
    compare_dat %>% 
      group_by(Site) %>%
      summarise(
        mean = mean(c4_diff),
        se = sd(c4_diff) / sqrt(n()),
        type = "c4"
      )
  )
  
  setwd(wd)
  return(summary_dat)
}


plot_c3_v_c4 <-
  function(summary_dat,
           filename = NA) {
    gg <- ggplot(data = summary_dat) +
      
      # Draw bars
      geom_bar(
        aes(fill = type, 
            y = mean, 
            x = Site),
        stat = "identity",
        position = position_stack(reverse = TRUE),
        color = "black",
        width = 0.5,
      ) +
      
      # Add standard error
      geom_errorbar(
        data = summary_dat %>% 
          filter(type == "c3"),
        aes(
          x = Site,
          ymin = mean - se,
          ymax = mean + se
        ),
        size = 0.5,
        width = 0
      ) +
      
      # Add theme and adjust axes
      theme_sigmaplot(xticks = FALSE) +
      scale_y_continuous(
        limits = c(0, 105),
        breaks = c(0, 
                   20, 
                   40, 
                   60, 
                   80, 
                   100),
        expand = c(0, 0),
        labels = c(0, 
                   20, 
                   40, 
                   60, 
                   80, 
                   100),
        sec.axis = dup_axis(labels = NULL, name = "")
      ) +
      theme(axis.ticks.y = element_line(
        color = c(
          "transparent", 
          "black", 
          "black", 
          "black", 
          "black", 
          "black"
          )
      )) +
      
      ylab(y_lab_3) +
      xlab(NULL) +
      
      # Adjust legend and colors
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank()
      ) +
      scale_fill_manual(values = c(C3_color, 
                                   C4_color),
                        labels = legend_names_3) +
      scale_x_discrete(labels = x_ticks_3)
    
    gg
    if (!(is.na(filename))) {
      ggsave(file = filename,
             height = 5,
             width = 3.5)
    }
    return(gg)
  }


plot_c3_v_c4_diff <-
  function(summary_dat,
           filename = NA) {
    gg <- ggplot(data = summary_dat) +
      
      # Add zero line
      geom_hline(yintercept = 0, lty = 3) +
      
      # Add standard error first
      geom_errorbar(
        position = position_dodge(width = 0.3),
        data = summary_dat,
        aes(
          group = type,
          x = Site,
          ymin = mean - se,
          ymax = mean + se
        ),
        size = 0.5,
        width = 0
      ) +
      
      # Draw points
      geom_point(
        aes(fill = type, 
            y = mean, 
            x = Site),
        color = "black",
        shape = 21,
        size = 4,
        stat = "identity",
        position = position_dodge(0.3)
      ) +
      
      # Add theme and adjust axes
      theme_sigmaplot(xticks = FALSE) +
      scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = "")) +
      ylab(y_lab_4) +
      xlab(NULL) +
      
      # Adjust legend and colors
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank()
      ) +
      scale_fill_manual(values = c(C3_color, 
                                   C4_color),
                        labels = legend_names_3) +
      scale_x_discrete(labels = x_ticks_3)
    
    gg
    if (!(is.na(filename))) {
      ggsave(file = filename,
             height = 5,
             width = 3.5)
    }
    return(gg)
  }
