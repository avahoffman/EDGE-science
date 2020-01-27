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
  function(ambient_composition, sum_across_years = TRUE) {
    # Sum across years option decides whether each year is a data point for site (FALSE), plot
    # OR whether to add up all the years cumulatively first (TRUE)
    
    setwd(data_dir)
    raw_dat <- read.csv("EDGE_biomass_long_QAQC_final.csv")
    
    # Filter out old years
    dat <- raw_dat[(raw_dat$Year %in% eri_grac_years), ]
    
    # Lump all experimental droughts into drought (if want to exclude one or the other, see config)
    dat <- dat[(dat$Trt %in% c(include_in_drt_trt, "con")), ]
    dat <- dat %>%
      mutate(Trt = as.character(Trt)) %>% mutate(Trt = replace(Trt, Trt == "chr" |
                                                                 Trt == "int", "drt"))
    
    # Collect only C4 grasses
    c4_taxa <- read.csv("Taxa_info.csv") %>%
      filter(Photo.path == "C4" & Funct.grp == "grass") %>%
      pull(Plant.code)
    c4_dat <- as_tibble(dat[(dat$category %in% c4_taxa), ])
    # Exclude Bouteloua gracilis and eriopoda, and describe as C4
    c4_dat_other <-
      c4_dat %>% filter(category != "BOGR" &
                          category != "BOER4") %>% mutate(category = replace(category, !(is.na(category)), "C4"))
    # Save B. gracilis and eriopoda seperately
    c4_dat_main <-
      c4_dat %>% filter(category == "BOGR" | category == "BOER4")
    # Combine data segments
    c4_dat <- rbind(c4_dat_other, c4_dat_main)
    
    # IF cumulative, sum across all years to get a more stable number
    if (sum_across_years) {
      full_dat <-
        c4_dat %>% group_by(Site, Block, Plot, Trt, category) %>% summarise(biomass = sum(biomass) / length(eri_grac_years))
    }
    
    # Calculate total biomass
    totals <- full_dat %>% group_by(Site, Block, Plot, Trt) %>%
      summarise(total_biomass = sum(biomass))
    # Convert species level to wide format
    wide_dat <-
      dcast(full_dat, Site + Block + Plot + Trt ~ category, value.var = "biomass")
    # Join total to wide format and replace any NAs with zeros
    full_dat <-
      full_join(totals, wide_dat, by = c("Site", "Block", "Plot", "Trt")) %>%
      mutate(BOER4 = replace(BOER4, (is.na(BOER4)), 0)) %>%
      mutate(BOGR = replace(BOGR, (is.na(BOGR)), 0)) %>%
      mutate(C4 = replace(C4, (is.na(C4)), 0))
    
    if (ambient_composition) {
      # Convert to percent of total
      for (i in c("BOER4", "BOGR", "C4")) {
        full_dat[[i]] <- 100 * full_dat[[i]] / full_dat$total_biomass
      }
      # Keep only control plots
      full_dat <- full_dat %>% filter(Trt == "con")
      # Make long format
      long_dat <- full_dat %>% gather(spp, pct, BOER4:C4)
      
      # Perform T.test
      SEV_black_eriopoda <-
        long_dat %>% filter(Site == "SEV.black" &
                              spp == "BOER4") %>% pull(pct)
      SEV_blue_eriopoda <-
        long_dat %>% filter(Site == "SEV.blue" &
                              spp == "BOER4") %>% pull(pct)
      
      # Run test and write results
      sink("output/statistical/tests.txt", append = TRUE)
      
      print(
        "T test of true difference in Bouteloua eriopoda percent (SEV Black vs SEV Blue) is not equal to 0"
      )
      print(var.test(SEV_black_eriopoda, SEV_blue_eriopoda)) # Variance is similar
      print(
        t.test(
          SEV_black_eriopoda,
          SEV_blue_eriopoda,
          var.equal = TRUE,
          alternative = "two.sided"
        )
      )
      
      sink()
      
      # Summarize by site
      summary_dat <- long_dat %>% group_by(Site, spp) %>%
        summarise(mean = mean(pct),
                  se = sd(pct) / sqrt(n())) %>%
        filter(Site %in% eri_grac_sites)
      
      setwd(wd)
      return(summary_dat)
    }
    else{
      # Ambient
      full_dat_amb <- full_dat %>% filter(Trt == "con")
      # Take the mean of drt treatments (including chr and int)
      full_dat_drt <- full_dat %>% group_by(Site, Block, Trt) %>%
        summarise(
          total_biomass = mean(total_biomass),
          BOGR = mean(BOGR),
          BOER4 = mean(BOER4)
        ) %>% filter(Trt == "drt")
      # Join tables
      compare_dat <-
        full_join(full_dat_amb, full_dat_drt, by = c("Site", "Block")) %>% select(-C4)
      
      # Subset based on species
      BOER4 <-
        compare_dat %>% filter(Site == "SEV.black") %>% filter(BOER4.x > 0) %>% select(-BOGR.x,-BOGR.y)
      BOGR <-
        compare_dat %>% filter(Site == "SEV.blue") %>% filter(BOGR.x > 0) %>% select(-BOER4.x,-BOER4.y)
      
      # Calculate the difference
      BOGR$diff <-
        100 * (BOGR$BOGR.y - BOGR$BOGR.x) / BOGR$BOGR.x
      BOER4$diff <-
        100 * (BOER4$BOER4.y - BOER4$BOER4.x) / BOER4$BOER4.x
      
      # Summarize by site
      summary_dat <- BOGR %>% group_by(Site) %>%
        summarise(
          mean = mean(diff),
          se = sd(diff) / sqrt(n()),
          type = "bogr"
        )
      
      # Repeat data for C4
      summary_dat <- rbind(
        summary_dat,
        BOER4 %>% group_by(Site) %>%
          summarise(
            mean = mean(diff),
            se = sd(diff) / sqrt(n()),
            type = "boer"
          )
      )
      
      setwd(wd)
      return(summary_dat)
    }
  }


plot_spp_sev <- function(summary_dat, filename = NA) {
  gg <- ggplot(data = summary_dat) +
    
    # Draw bars
    geom_bar(
      aes(fill = spp, y = mean, x = Site),
      stat = "identity",
      position = position_stack(reverse = TRUE),
      color = "black",
      width = 0.5,
    ) +
    
    # Add standard error for BOGR
    geom_errorbar(
      data = summary_dat %>% filter(spp == "BOER4"),
      aes(
        x = Site,
        ymin = mean - se,
        ymax = mean + se
      ),
      size = 0.5,
      width = 0
    ) +
    
    # Add standard error for BOER4 - not a good workaround for this - considered bad practice to have
    # stacked error bars
    geom_errorbar(
      data = summary_dat %>% filter(spp == "BOGR" & Site == "SEV.blue"),
      aes(
        x = Site,
        ymin = mean + 31.5 - se,
        ymax = mean + 31.5 + se
      ),
      size = 0.5,
      width = 0
    ) +
    
    # Add theme and adjust axes
    theme_sigmaplot(xticks = FALSE) +
    scale_y_continuous(
      limits = c(0, 105),
      breaks = c(0, 20, 40, 60, 80, 100),
      expand = c(0, 0),
      labels = c(0, 20, 40, 60, 80, 100),
      sec.axis = dup_axis(labels = NULL, name = "")
    ) +
    theme(axis.ticks.y = element_line(
      color = c("transparent", "black", "black", "black", "black", "black")
    )) +
    
    ylab(y_lab_5) +
    xlab(NULL) +
    
    # Adjust legend and colors
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_blank(),
    ) +
    scale_fill_manual(
      values = c(eriopoda_color, gracilis_color, C4_color),
      labels = c(legend_names_5)
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


plot_sev_diff <- function(summary_dat, filename = NA) {
  gg <- ggplot(data = summary_dat) +
    
    # Add zero line
    #geom_hline(yintercept = 0, lty = 3) +
    
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
      aes(fill = type, y = mean, x = Site),
      color = "black",
      shape = 21,
      size = 4,
      stat = "identity",
      position = position_dodge(0.3)
    ) +
    
    # Add theme and adjust axes
    theme_sigmaplot(xticks = FALSE) +
    scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = "")) +
    ylab(y_lab_6) +
    xlab(NULL) +
    
    # Adjust legend and colors
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_blank()
    ) +
    scale_fill_manual(
      values = c(eriopoda_color, gracilis_color),
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
