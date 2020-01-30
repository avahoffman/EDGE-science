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
    
    setwd(wd)
    dat <- filter_and_clean_raw_data(bio_dat,
                                     sites = c3_c4_sites,
                                     years = c3_c4_years)
    
    # Group to plot level by year, site, treatment, while separating C4 and C3 grasses into
    # different summed amounts
    full_dat <-
      full_join(
        # C3 grass data
        dat %>%
          filter(category %in%
                   c4_grasses) %>%
          group_by(Site,
                   Block,
                   Plot,
                   Year,
                   Trt) %>%
          summarise(c4_biomass = sum(biomass)),
        
        # C4 grass data
        dat %>%
          filter(category %in%
                   c3_grasses) %>%
          group_by(Site,
                   Block,
                   Plot,
                   Year,
                   Trt) %>%
          summarise(c3_biomass = sum(biomass)),
        
        # Rejoin data
        by = c("Site",
               "Block",
               "Plot",
               "Year",
               "Trt")
      ) %>%
      # Filter if there is no grass whatsoever
      filter(c4_biomass > 0 &
               c3_biomass > 0)
    
    # IF cumulative, sum across all years to get a more stable number
    if (sum_across_years) {
      full_dat <-
        full_dat %>%
        group_by(Site,
                 Block,
                 Plot,
                 Trt) %>%
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


ambient_data_c3_c4 <-
  function(full_dat = collect_c3_c4_data()) {
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
    
    # Summarize c3 percent by site then repeat data for inverse of C3
    # percent (so that C3 and C4 bars can be stacked in the plot)
    summary_dat <-
      rbind(
        full_dat %>%
          group_by(Site) %>%
          summarise(
            mean = mean(c3_pct),
            se = sd(c3_pct) / sqrt(n()),
            type = "c3"
          ),
        
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


diff_data_c3_c4 <-
  function(full_dat = collect_c3_c4_data()) {
    # Join ambient and drought data to get the difference
    compare_dat <-
      full_join(
        # Ambient data
        full_dat %>%
          filter(Trt == "con"),
        
        # Aggregate among drt treatments by mean (e.g., including chr and int)
        full_dat_drt <-
          full_dat %>%
          group_by(Site,
                   Block,
                   Trt) %>%
          summarise(
            c3_biomass = mean(c3_biomass),
            c4_biomass = mean(c4_biomass),
          ) %>%
          filter(Trt == "drt"),
        
        # Join
        by = c("Site", "Block")
      )
    
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
    
    summary_dat <-
      
      rbind(
        # Summarize by site for C3
        compare_dat %>%
          group_by(Site) %>%
          summarise(
            mean = mean(c3_diff),
            se = sd(c3_diff) / sqrt(n()),
            type = "c3"
          ),
        # Summarize by site for C4
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
  function(summary_dat, filename = NA) {
    gg <-
      ggplot(data = summary_dat) +
      # Draw bars
      geom_bar_custom(data = summary_dat, fill_name = "type") +
      # Add standard error
      geom_errorbar_custom(data = summary_dat %>%
                             filter(type == "c3")) +
      # Add theme and adjust axes
      theme_sigmaplot(xticks = FALSE) +
      scale_y_continuous_percent() +
      scale_y_continuous_percent_ticks() +
      ylab(y_lab_3) +
      xlab(NULL) +
      # Adjust legend and colors
      legend_custom() +
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
  function(summary_dat, filename = NA) {
    gg <-
      ggplot(data = summary_dat) +
      # Add zero line
      geom_hline(yintercept = 0, lty = 3) +
      # Add standard error first
      geom_errorbar_custom(data = summary_dat,
                           group = "type",
                           dodge = 0.3) +
      # Draw points
      geom_point_custom(data = summary_dat, dodge = 0.3) +
      # Add theme and adjust axes
      theme_sigmaplot(xticks = FALSE) +
      scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = "")) +
      ylab(y_lab_4) +
      xlab(NULL) +
      # Adjust legend and colors
      legend_custom() +
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
