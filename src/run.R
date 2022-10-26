# Execution script for EDGE Science project pipeline.
# Generates plots and records results in the output directory.
###########################################################################################
# Set working directory for the repository (should be the git repo w/ .Rproj):
wd <- here::here()

setwd(wd)

# General functions and configuration
source("src/config.R")
source("src/utils.R")

# Dataset specific functions
# MAKE SURE ALL PACKAGES ARE INSTALLED TO ENSURE SUCCESSFUL IMPORT
source("src/outlier_analysis.R")
source("src/site_map.R")
source("src/sensitivity.R")
source("src/precipitation.R")
source("src/C3_v_c4.R")
source("src/eriopoda_v_gracilis.R")
source("src/sev_v_sgs.R")
source("src/genetic_diversity.R")
source("src/gracilis_traits.R")


###########################################################################################
# Load libraries
library(ggplot2)
library(cowplot)

###########################################################################################

raw_bio_dat <- 
  read.csv("data/EDGE_Allsites_long.csv")

bio_dat <- 
  as.data.frame(remove_outliers(raw_bio_dat,
                                percentile_cutoff = 0.99,
                                outlier_prop_threshold = 0.5))


# ---------
plot_grid(
  # Plot ecoregion and site map
  plot_site_map_with_ecoregions(generate_shapefile_data()),
  
  plot_precip_data(get_precip_data()),
  
  # Plot showing EDGE site sensitivity (No longer want Huxman plot)
  make_inset_decline_plot(get_percent_decline()),

  # Specify different widths of plot grid
  rel_widths = c(1,0.8,0.8),
  align = "h",
  axis = "bt",
  nrow = 1
)
# Save plots
ggsave(file = "figures/sites_sensitivity_ol_rm.pdf",
       height = 4.5,
       width = 9)


# ---------
plot_grid(# Compare percent C3 grass at CHY and SGS
  plot_c3_v_c4(ambient_data_c3_c4()),
  
  # Compare change in C3 and C4 grasses at CHY and SGS
  plot_c3_v_c4_diff(diff_data_c3_c4()))
# Save plots
ggsave(file = "figures/c3_v_c4_ol_rm.pdf",
       height = 3,
       width = 5.5)

# Plot same figure as above but looking at Bouteloua gracilis instead
plot_grid(# Compare percent bouteloua at CHY and SGS
  plot_c3_v_c4(ambient_data_c3_c4(bouteloua = TRUE)),
  
  # Compare change in C3 and B. gracilis at CHY and SGS
  plot_c3_v_c4_diff(diff_data_c3_c4(bouteloua = TRUE)))
# Save plots
ggsave(file = "figures/c3_v_BOGR_ol_rm.pdf",
       height = 3,
       width = 6)


# ---------
plot_grid(
  # Sevilleta Blue and Sevilleta Black composition comparison of Bouteloua species
  plot_spp_sev(ambient_data_erio_grac()) +
    # Break legend in to two rows so that it's not cut off
    guides(fill = guide_legend(nrow = 2)) +
    theme(legend.text.align = 0),
  
  # Percent change in ANPP in SEV Blue - gracilis and SEV Black eriopoda
  plot_sev_diff(diff_data_erio_grac())
  
)
# Save plots
ggsave(file = "figures/eriopoda_v_gracilis_ol_rm.pdf",
       height = 3,
       width = 5.5)


# Genetic differences between sites (Sevilleta and SGS) of Bouteloua gracilis:
# First, perform cross validation of number of principal components to use to distinguish
# sites on a genetic basis
perform_cross_validation_DAPC(load_and_clean_genind_data(), XV_skip = XV_skip)

plot_grid(
  # Compare percent of Bouteloua gracilis with other functional groups at SEV Blue and SGS
  plot_spp_sev_sgs(ambient_data_sev_sgs()) +
    # Break legend in to two rows so that it's not cut off
    guides(fill = guide_legend(nrow = 2)) +
    theme(legend.text.align = 0),
  
  # Percent change in B. gracilis ANPP in SEV Blue and SGS
  plot_sev_sgs_diff(diff_data_sev_sgs()),
  
  # Plot change in A:B biomass ratio by greenhouse treatment, both SEV Blue and SGS populations
  plot_traits(load_and_clean_trait_data(trait = "a/b"), trait = "a/b"),
  
  # Plot SEV Blue and SGS individuals on principal components based on genetic markers (SNPs)
  plot_dapc(load_and_clean_genind_data()) +
    # Break legend in to two rows so that it's not cut off
    guides(fill = guide_legend(nrow = 2)) +
    theme(legend.text.align = 0),
  
  # Plot change in biomass by greenhouse treatment, both SEV Blue and SGS populations
  plot_traits(load_and_clean_trait_data()),
  
  # Arrange plots in 2x2 with relative size
  rel_widths = c(3, 2, 2),
  nrow = 2,
  ncol = 3
  
)
# Save plots
ggsave(file = "figures/gracilis_genetic_diversity_ol_rm_with_ab.pdf",
       height = 6,
       width = 7)
