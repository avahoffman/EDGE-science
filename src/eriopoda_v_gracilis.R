# This script analyzes and plots differences between composition of Bouteloua species
# at the two Sevilleta sites, and relates it to sensitivity to precipitation limitation.
###########################################################################################
# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

###########################################################################################


collect_sev_data <-
  function(sum_across_years = TRUE) {
    # Sum across years option decides whether each year is a data point for site (FALSE), plot
    # OR whether to add up all the years cumulatively first (TRUE)
    dat <- filter_and_clean_raw_data(bio_dat,
                                     sites = eri_grac_sites,
                                     years = eri_grac_years)
    
    # Collect only C4 grasses
    c4_taxa <-
      taxa_dat %>%
      filter(Photo.path == "C4" &
               Funct.grp == "grass") %>%
      pull(Plant.code)
    
    # Filter data frame for C4 taxa
    # Exclude Bouteloua gracilis and eriopoda, and describe as C4
    c4_dat <-
      rbind(
        dat %>%
          filter(category %in% c4_taxa &
                   category != "BOGR" &
                   category != "BOER4") %>%
          mutate(category = replace(category, !(is.na(
            category
          )), "C4")),
        
        # Filter data frame for C4 taxa
        # Save B. gracilis and eriopoda seperately
        dat %>%
          filter(category %in% c4_taxa &
                   (category == "BOGR" | category == "BOER4"))
      )
    
    # IF cumulative, sum across all years to get a more stable number
    if (sum_across_years) {
      full_dat <-
        c4_dat %>%
        group_by(Site, Block, Plot, Trt, category) %>%
        summarise(biomass = sum(biomass) /
                    length(eri_grac_years))
    }
    
    full_dat <-
      full_join(
        # Calculate total biomass
        full_dat %>%
          group_by(Site,
                   Block,
                   Plot,
                   Trt) %>%
          summarise(total_biomass = sum(biomass)),
        
        # Convert species level to wide format
        dcast(full_dat, Site + Block + Plot + Trt ~ category,
              value.var = "biomass"),
        
        # Join together
        by = c("Site",
               "Block",
               "Plot",
               "Trt")
      )
    
    # Replace missing values with zeros (no biomass)
    for (i in c("BOER4", "BOGR", "C4")) {
      full_dat[[i]] <- full_dat[[i]] %>% replace_na(0)
    }
    
    return(full_dat)
  }

ambient_data_erio_grac <-
  function(full_dat = collect_sev_data()) {
    # Convert to percent of total
    for (i in c("BOER4", "BOGR", "C4")) {
      full_dat[[i]] <- 100 * full_dat[[i]] / full_dat$total_biomass
    }
    # Keep only control plots and make long format
    long_dat <-
      full_dat %>%
      filter(Trt == "con") %>%
      gather(spp, pct, BOER4:C4)
    
    # Perform T.test
    SEV_black_eriopoda <-
      long_dat %>%
      filter(Site == "SEV.black" &
               spp == "BOER4") %>%
      pull(pct)
    SEV_blue_eriopoda <-
      long_dat %>%
      filter(Site == "SEV.blue" &
               spp == "BOER4") %>%
      pull(pct)
    
    # Run test and write results
    sink(statsfile, append = TRUE)
    print(
      "T test of true difference in levels of Bouteloua eriopoda (percent at SEV Black vs SEV Blue) is not equal to 0"
    )
    print(t.test(SEV_blue_eriopoda,
                         SEV_black_eriopoda))
    sink()
    
    # Summarize by site
    summary_dat <-
      long_dat %>%
      group_by(Site, spp) %>%
      summarise(mean = mean(pct),
                se = sd(pct) / sqrt(n()))
    
    return(summary_dat)
  }

diff_data_erio_grac <-
  function(full_dat = collect_sev_data()) {
    # Join ambient and drought data to get the difference, Join tables
    compare_dat <-
      full_join(
        # Ambient data
        full_dat %>%
          filter(Trt == "con"),
        
        # Aggregate among drt treatments by mean (e.g., including chr and int)
        full_dat %>%
          group_by(Site, Block, Trt) %>%
          summarise(
            total_biomass = mean(total_biomass),
            BOGR = mean(BOGR),
            BOER4 = mean(BOER4)
          ) %>%
          filter(Trt == "drt"),
        
        # Join
        by = c("Site", "Block")
      ) %>%
      dplyr::select(-C4)
    
    # Subset based on species
    BOER4 <-
      compare_dat %>%
      filter(Site == "SEV.black") %>%
      filter(BOER4.x > 0) %>%
      dplyr::select(-BOGR.x, -BOGR.y)
    BOGR <-
      compare_dat %>%
      filter(Site == "SEV.blue") %>%
      filter(BOGR.x > 0) %>%
      dplyr::select(-BOER4.x, -BOER4.y)
    
    # Calculate the difference
    BOGR$diff <-
      100 * (BOGR$BOGR.y - BOGR$BOGR.x) / BOGR$BOGR.x
    BOER4$diff <-
      100 * (BOER4$BOER4.y - BOER4$BOER4.x) / BOER4$BOER4.x
    
    # Perform T.tests
    SEV_blue_gracilis <-
      BOGR %>%
      pull(diff)
    SEV_black_eriopoda <-
      BOER4 %>%
      pull(diff)
    # Run test and write results
    sink(statsfile, append = TRUE)
    print(
      "T test of true difference in percent change of ANPP (SEV Black B. eriopoda vs SEV Blue B. gracilis) is not equal to 0"
    )
    print(t.test(SEV_blue_gracilis,
                         SEV_black_eriopoda))
    sink()
    
    # Summarize by site
    summary_dat <-
      
      rbind(
        # Summarize by site for BOGR
        BOGR %>%
          group_by(Site) %>%
          summarise(
            mean = mean(diff),
            se = sd(diff) / sqrt(n()),
            type = "bogr"
          ),
        # Repeat for BOER
        BOER4 %>%
          group_by(Site) %>%
          summarise(
            mean = mean(diff),
            se = sd(diff) / sqrt(n()),
            type = "boer"
          )
      )
    
    return(summary_dat)
  }

plot_spp_sev <-
  function(summary_dat, filename = NA) {
    gg <-
      ggplot(data = summary_dat) +
      # Draw bars
      geom_bar_custom(data = summary_dat, fill_name = "spp") +
      # Add standard error for BOGR
      geom_errorbar_custom(data = summary_dat %>%
                             filter(spp == "BOER4")) +
      # Add theme and adjust axes
      theme_sigmaplot(xticks = FALSE) +
      scale_y_continuous_percent() +
      scale_y_continuous_percent_ticks() +
      ylab(y_lab_5) +
      xlab(NULL) +
      # Adjust legend and colors
      legend_custom() +
      scale_fill_manual(
        values = c(eriopoda_color,
                   gracilis_color,
                   C4_color),
        labels = c(legend_names_5)
      ) +
      scale_x_discrete(labels = x_ticks_5) +
      # Add standard error for BOER4
      geom_errorbar_custom(data = summary_dat %>%
                             filter(spp == "BOGR" &
                                      Site == "SEV.blue"),
                           add = summary_dat %>% 
                             filter(Site == "SEV.blue" & spp == "BOER4") %>% 
                             pull(mean))
    
    gg
    
    if (!(is.na(filename))) {
      ggsave(file = filename,
             height = 5,
             width = 3.5)
    }
    
    return(gg)
  }


plot_sev_diff <-
  function(summary_dat, filename = NA) {
    gg <-
      ggplot(data = summary_dat) +
      # Add zero line
      #geom_hline(yintercept = 0, lty = 3) +
      # Add standard error first
      geom_errorbar_custom(data = summary_dat,
                           group = "type",
                           dodge = 0.3) +
      # Draw points
      geom_point_custom(data = summary_dat, dodge = 0.3) +
      # Add theme and adjust axes
      theme_sigmaplot(xticks = FALSE) +
      scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = "")) +
      ylab(y_lab_6) +
      xlab(NULL) +
      # Adjust legend and colors
      legend_custom() +
      scale_fill_manual(
        values = c(eriopoda_color,
                   gracilis_color),
        labels = c(legend_names_6)
      ) +
      scale_x_discrete(labels = x_ticks_5)
    
    gg
    
    if (!(is.na(filename))) {
      ggsave(file = filename,
             height = 5,
             width = 3.5)
    }
    
    return(gg)
  }
