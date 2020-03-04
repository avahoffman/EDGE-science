# Search for outliers among biomass subplots in preparation for the rest of the analysis
###########################################################################################
library(dplyr)
library(ggplot2)
library(cowplot)

# Useful information here: http://r-statistics.co/Outlier-Treatment-With-R.html
###########################################################################################

make_outlier_plot <-
  function(d) {
    # This function will test for chi-square scores that are outside the
    # percentile cutoff, and color them blue.
    # For best results, use only on a specific site-category-treatment subset
    ggplot() +
      geom_point(aes(
        x = as.numeric(rownames(d)),
        y = d$biomass,
        color = outliers::scores(d$biomass,
                                 type = "chisq",
                                 prob = percentile_cutoff)
      )) +
      xlab("Index") +
      ylab("Response") +
      guides(col = guide_legend(title = "Outlier"))
  }


remove_outliers <-
  function(df,
           percentile_cutoff = 0.99,
           outlier_prop_threshold = 0.5) {
    # Find outliers by Site, Trt, and species/category
    dat = data.frame()
    # First divide up by site and drop that column
    for (Site_ in distinct(df, Site)$Site) {
      site_level_dat <-
        df %>%
        filter(Site == Site_)
      # Divide up by treatment and drop that column
      for (Trt_ in distinct(site_level_dat, Trt)$Trt) {
        trt_level_dat <-
          site_level_dat %>%
          filter(Trt == Trt_)
        # Divide up by species and drop that column
        for (category_ in distinct(trt_level_dat, category)$category) {
          cat_level_dat <-
            trt_level_dat %>%
            filter(category == category_)
          # Perform chi-square test for outliers
          influential <-
            outliers::scores(cat_level_dat$biomass,
                             type = "chisq",
                             prob = percentile_cutoff)
          # Only drop influential points if the proportion of influential points is less than some threshold
          # (eg, allows you to only get rid of data if a very small proportion are outliers)
          if ((length(influential[influential]) / nrow(cat_level_dat)) <= outlier_prop_threshold) {
            d <- cat_level_dat[!(influential),] # drop any where they are not influential
            dat <- rbind(dat, d)
          } else {
            dat <- rbind(dat, cat_level_dat)
          }
        }
      }
    }
    
    # Report percent data retained
    print(paste(
      "Kept ",
      round(nrow(dat) / nrow(df) * 100, 2),
      "% of raw biomass data.",
      sep = ""
    ))
    
    # Take the average among subplots to get the plot (not summing, that happens only for total biomass/plot)
    aggregated_dat <-
      dat %>%
      group_by(Site,
               Block,
               Plot,
               Year,
               Trt,
               category) %>%
      summarise(biomass = mean(biomass))
    
    # Replace BOGR2 with BOGR for continuity
    aggregated_dat <- 
      aggregated_dat %>%
      mutate(category = replace(category, category == "BOGR2", "BOGR"))
    
    # Remove noted outliers
    if (exclude_SGS_2016_plot2) {
      # there's a note that states it was grazed by cattle (BOGR, Carex, and BUDA are 0 across all iterations)
      aggregated_dat <- 
        aggregated_dat %>% 
        filter(!(Site == "SGS" & Year == 2016 & Plot == 2))
    }
    
    # Write data
    write.csv(aggregated_dat, 
              "data/EDGE_biomass_outliers_removed.csv",
              row.names = FALSE)
    
    return(aggregated_dat)
  }
