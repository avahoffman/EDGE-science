###########################################################################################
## set working directory
setwd("/Users/avahoffman/Dropbox/Research/EDGE_Science/EDGE-science/src/")
source("config.R")
source("utils.R")
setwd(wd)

###########################################################################################
## load libraries
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
    raw_dat <- read.csv("SGS-CHY_TRT-ANPP_long.csv")
    
    # Filter out old years
    dat <- raw_dat[(raw_dat$Year %in% eri_grac_years), ]
    
    # Lump all experimental droughts into drought (if want to exclude one or the other, see config)
    dat <- dat[(dat$Trt %in% c(include_in_drt_trt, "con")), ]
    dat <- dat %>%
      mutate(Trt = as.character(Trt)) %>% mutate(Trt = replace(Trt, Trt == "chr" |
                                                                 Trt == "int", "drt"))
    
    # Collect only C4 grasses
    c4_dat <- as_tibble(dat[(dat$category %in% eri_grac_grasses), ])
    
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
    # Join total to wide format
    full_dat <-
      full_join(totals, wide_dat, by = c("Site", "Block", "Plot", "Trt"))
    
    if (ambient_composition) {
      # Convert to percent of total
      for (i in eri_grac_grasses) {
        full_dat[[i]] <- 100 * full_dat[[i]] / full_dat$total_biomass
      }
      # Keep only control plots
      full_dat <- full_dat %>% filter(Trt == "con")
      # Make long format
      long_dat <- full_dat %>% gather(spp, pct, BOGR:C4)
      
      # Summarize by site
      summary_dat <- long_dat %>% group_by(Site, spp) %>%
        summarise(mean = mean(pct),
                  se = sd(pct) / sqrt(n()))
      
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
          BUDA = mean(BUDA)
        ) %>% filter(Trt == "drt")
      # Join tables
      compare_dat <-
        full_join(full_dat_amb, full_dat_drt, by = c("Site", "Block")) %>% select(-C4)
      
      # Subset based on species
      BUDA <-
        compare_dat %>% filter(Site == "SGS") %>% filter(BUDA.x > 0) %>% select(-BOGR.x,-BOGR.y)
      BOGR <-
        compare_dat %>% filter(Site == "CHY") %>% filter(BOGR.x > 0) %>% select(-BUDA.x,-BUDA.y)
      
      # Calculate the difference
      BOGR$diff <-
        100 * (BOGR$BOGR.y - BOGR$BOGR.x) / BOGR$BOGR.x
      BUDA$diff <-
        100 * (BUDA$BUDA.y - BUDA$BUDA.x) / BUDA$BUDA.x
      
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
        BUDA %>% group_by(Site) %>%
          summarise(
            mean = mean(diff),
            se = sd(diff) / sqrt(n()),
            type = "buda"
          )
      )
      
      setwd(wd)
      return(summary_dat)
    }
  }


plot_spp_sev <- function(summary_dat, filename) {
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
      data = summary_dat %>% filter(spp == "BOGR"),
      aes(
        x = Site,
        ymin = mean - se,
        ymax = mean + se
      ),
      size = 1,
      width = 0
    ) +
    
    # Add standard error for BUDA - not a good workaround for this - considered bad practice to have
    # stacked error bars
    geom_errorbar(
      data = summary_dat %>% filter(spp == "BUDA"),
      aes(
        x = Site,
        ymin = mean + 95.4 - se,
        ymax = mean + 95.4 + se
      ),
      size = 1,
      width = 0
    ) +
    
    # Add theme and adjust axes
    theme_sigmaplot(xticks = FALSE) +
    scale_y_continuous(
      limits = c(0, 105),
      breaks = c(0.25, 20, 40, 60, 80, 100),
      expand = c(0, 0),
      labels = c(0, 20, 40, 60, 80, 100),
      sec.axis = dup_axis(labels = NULL, name = "")
    ) +
    
    ylab("Percent of Grass ANPP") +
    xlab(NULL) +
    
    # Adjust legend and colors
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_blank(),
    ) +
    scale_fill_manual(
      values = c(shortgrass_color_pale, northern_color_pale, "white"),
      labels = c("B. gracilis", "B. dactyloides", "Other C4")
    )
  
  gg
  ggsave(file = filename,
         height = 5,
         width = 3.5)
  return(gg)
}


plot_sev_diff <- function(summary_dat, filename) {
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
      size = 1,
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
    ylab("Percent Change in ANPP") +
    xlab(NULL) +
    
    # Adjust legend and colors
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_blank()
    ) +
    scale_fill_manual(
      values = c(shortgrass_color_pale, northern_color_pale),
      labels = c("B. gracilis", "B. dactyloides")
    )
  
  
  
  gg
  ggsave(file = filename,
         height = 5,
         width = 3.5)
  return(gg)
}

plot_spp_sev(collect_sev_data(ambient_composition = TRUE,
                              sum_across_years = TRUE),
             filename = "figures/eriopoda_v_gracilis_DRAFT.pdf")
plot_sev_diff(collect_sev_data(ambient_composition = FALSE,
                               sum_across_years = TRUE),
              filename = "figures/eriopoda_v_gracilis_diff_chr_DRAFT.pdf")
