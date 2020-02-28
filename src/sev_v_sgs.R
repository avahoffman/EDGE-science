# This script analyzes and plots differences between composition of Bouteloua species
# at the two Sevilleta sites, and relates it to sensitivity to precipitation limitation.
###########################################################################################
# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

###########################################################################################


collect_sev_sgs_data <-
  function(ambient_composition, sum_across_years = TRUE) {
    # Sum across years option decides whether each year is a data point for site (FALSE), plot
    # OR whether to add up all the years cumulatively first (TRUE)
    dat <- filter_and_clean_raw_data(bio_dat,
                                     sites = sev_sgs_sites,
                                     years = sev_sgs_years)
    
    # Collect only C4 grasses for filtering
    c4_taxa <-
      taxa_dat %>%
      filter(Photo.path == "C4" &
               Funct.grp == "grass") %>%
      pull(Plant.code)
    
    # Collect only C3 grasses for filtering
    c3_taxa <-
      taxa_dat %>%
      filter(Photo.path == "C3" &
               Funct.grp == "grass") %>%
      pull(Plant.code)
    
    # Combine data chunks
    full_dat <-
      rbind(
        # Non-BOGR C4 grasses
        dat %>%
          filter(category %in% c4_taxa &
                   category != "BOGR") %>%
          mutate(category = replace(category, !(is.na(
            category
          )),
          "C4")),
        # C3 grasses
        dat %>%
          filter(category %in% c3_taxa) %>%
          mutate(category = replace(category, !(is.na(
            category
          )),
          "C3")),
        # BOGR only
        dat %>%
          filter(category == "BOGR")
      )
    
    # IF cumulative, sum across all years to get a more stable number
    if (sum_across_years) {
      full_dat <-
        full_dat %>%
        group_by(Site, Block, Plot, Trt, category) %>%
        summarise(biomass = sum(biomass) / length(sev_sgs_years))
    }
    
    # Calculate total biomass for groups in question
    full_dat <-
      full_join(
        # Calculate total biomass
        full_dat %>%
          group_by(Site, Block, Plot, Trt) %>%
          summarise(total_biomass = sum(biomass)),
        
        # Convert species level to wide format
        dcast(full_dat, Site + Block + Plot + Trt ~ category,
              value.var = "biomass"),
        
        # Join total to wide format
        by = c("Site", "Block", "Plot", "Trt")
      )
    
    # Replace missing values with zeros (no biomass - species not appearing in plot)
    for (i in c("BOGR", "C3", "C4")) {
      full_dat[[i]] <- full_dat[[i]] %>% replace_na(0)
    }
    
    return(full_dat)
  }


ambient_data_sev_sgs <-
  function(full_dat = collect_sev_sgs_data()) {
    # Convert to percent of total
    for (i in c("BOGR", "C4", "C3")) {
      full_dat[[i]] <- 100 * full_dat[[i]] / full_dat$total_biomass
    }
    # Keep only control plots and make long format
    long_dat <-
      full_dat %>%
      filter(Trt == "con") %>%
      gather(spp, pct, BOGR:C4)
    
    # Summarize by site
    summary_dat <-
      long_dat %>%
      group_by(Site, spp) %>%
      summarise(mean = mean(pct),
                se = sd(pct) / sqrt(n()))
    
    return(summary_dat)
  }


diff_data_sev_sgs <-
  function(full_dat = collect_sev_sgs_data()) {
    # Join ambient and drought data to get the difference, Join tables
    compare_dat <-
      full_join(
        # Ambeint data
        full_dat %>%
          filter(Trt == "con"),
        
        # Aggregate among drt treatments by mean (e.g., including chr and int)
        full_dat %>%
          group_by(Site, Block, Trt) %>%
          summarise(
            total_biomass = mean(total_biomass),
            BOGR = mean(BOGR)
          ) %>%
          filter(Trt == "drt"),
        
        # Join
        by = c("Site", "Block")
      ) %>%
      dplyr::select(-C4,-C3) %>%
      filter(BOGR.x > 0)
    
    # Calculate the difference
    compare_dat$diff <-
      100 * (compare_dat$BOGR.y - compare_dat$BOGR.x) / compare_dat$BOGR.x
    
    # Perform T.tests
    BOGR_sgs <-
      compare_dat %>%
      filter(Site == "SGS") %>%
      pull(diff)
    BOGR_sev <-
      compare_dat %>%
      filter(Site == "SEV.blue") %>%
      pull(diff)
    
    # Run test and write results
    sink("output/statistical/tests.txt", append = TRUE)
    
    print("T test of true difference in B. gracilis ANPP change (SGS vs SEV.blue) is not equal to 0")
    ttest_with_var_check(BOGR_sgs, BOGR_sev)
    
    sink()
    
    # Summarize by site
    summary_dat <-
      compare_dat %>%
      group_by(Site) %>%
      summarise(mean = mean(diff),
                se = sd(diff) / sqrt(n()),
                type = "bogr")
    
    return(summary_dat)
  }



plot_spp_sev_sgs <-
  function(summary_dat, filename = NA) {
    gg <-
      ggplot(data = summary_dat) +
      # Draw bars
      geom_bar_custom(data = summary_dat, fill_name = "spp") +
      # Add standard error for BOGR
      geom_errorbar_custom(data = summary_dat %>%
                             filter(spp == "BOGR")) +
      # Add theme and adjust axes
      theme_sigmaplot(xticks = FALSE) +
      scale_y_continuous_percent() +
      scale_y_continuous_percent_ticks() +
      ylab(y_lab_7) +
      xlab(NULL) +
      # Adjust legend and colors
      legend_custom() +
      scale_fill_manual(values = c(gracilis_color,
                                   C3_color,
                                   C4_color),
                        labels = legend_names_7) +
      scale_x_discrete(labels = x_ticks_7) +
      # Add standard error for C3 - not a good workaround for this - 
      # considered bad practice to have stacked error bars. Have to add 58.4.
      geom_errorbar_custom(data = summary_dat %>%
                             filter(spp == "C3"),
                           add = 58.4)
    gg
    
    if (!(is.na(filename))) {
      ggsave(file = filename,
             height = 5,
             width = 3.5)
    }
    
    return(gg)
  }


plot_sev_sgs_diff <- 
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
    geom_point_custom(data = summary_dat, 
                      fill_name = "Site",
                      dodge = 0.3) +
    # Add theme and adjust axes
    theme_sigmaplot(xticks = FALSE) +
    scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = "")) +
    ylab(y_lab_8) +
    xlab(NULL) +
    # Adjust legend and colors
    legend_custom() +
    scale_fill_manual(values = c(sev_grac_color,
                                 sgs_grac_color),
                      labels = x_ticks_7) +
    scale_x_discrete(labels = x_ticks_7)
  
  gg
  
  if (!(is.na(filename))) {
    ggsave(file = filename,
           height = 5,
           width = 3.5)
  }
  
  return(gg)
}
