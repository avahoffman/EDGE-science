# Execution script for EDGE Science project pipeline.
# Generates plots and prints results to the R console.
###########################################################################################
# Set working directory for the repository (should be the git repo):
wd <-
  "/Users/hoffman ava/EDGE-science/"
# wd <-
#   "/Users/avahoffman/Dropbox/Research/EDGE_Science/EDGE-science/"

setwd(wd)

# General functions and configuration
source("src/config.R")
source("src/utils.R")

# Dataset specific functions
source("src/site_map.R")
source("src/sensitivity.R")
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


plot_grid(
  # Plot ecoregion and site map
  plot_site_map_with_ecoregions(generate_shapefile_data()),
  # Plot showing EDGE site sensitivity underlaid with Huxman 2004 data
  ggdraw(
    make_sensitivity_plot(get_huxman_2004_data(),
                          get_edge_data())
  ) +
    # Specify inset portion
    draw_plot(
      make_inset_decline_plot(get_percent_decline()),
      # Specify where the inset should be positioned, and how large it should be
      x = 0.36,
      y = 0.41,
      height = 0.5,
      width = 0.6
    ),
  # Specify different widths of plot grid
  rel_widths = c(2, 3)
)
# Save plots
ggsave(file = "figures/sites_sensitivity.pdf",
       height = 3,
       width = 5.5)


plot_grid(
  # Compare percent C3 grass at CHY and SGS
  plot_c3_v_c4(summarize_ambient_data(collect_c3_c4_data())),
  # Compare change in C3 and C4 grasses at CHY and SGS
  plot_c3_v_c4_diff(summarize_difference_data(collect_c3_c4_data()))
)
# Save plots
ggsave(file = "figures/c3_v_c4.pdf",
       height = 3,
       width = 5.5)


plot_grid(
  # Sevilleta Blue and Sevilleta Black composition comparison of Bouteloua species
  plot_spp_sev(collect_sev_data(ambient_composition = TRUE)) +
    # Break legend in to two rows so that it's not cut off
    guides(fill = guide_legend(nrow = 2)) + 
    theme(legend.text.align = 0),
  # Percent change in ANPP in SEV Blue - gracilis and SEV Black eriopoda
  plot_sev_diff(collect_sev_data(ambient_composition = FALSE))
)
# Save plots
ggsave(file = "figures/eriopoda_v_gracilis.pdf",
       height = 3,
       width = 5.5)


# Genetic differences between sites (Sevilleta and SGS) of Bouteloua gracilis:
# First, perform cross validation of number of principal components to use to distinguish
# sites on a genetic basis
perform_cross_validation_DAPC(load_and_clean_genind_data(), XV_skip = XV_skip)
plot_grid(
  # Compare percent of Bouteloua gracilis with other functional groups at SEV Blue and SGS
  plot_spp_sev_sgs(collect_sev_sgs_data(ambient_composition = TRUE)) +
    # Break legend in to two rows so that it's not cut off
    guides(fill = guide_legend(nrow = 2)) +
    theme(legend.text.align = 0),
  # Percent change in B. gracilis ANPP in SEV Blue and SGS
  plot_sev_sgs_diff(collect_sev_sgs_data(ambient_composition = FALSE)),
  # Plot SEV Blue and SGS individuals on principal components based on genetic markers (SNPs)
  plot_dapc(load_and_clean_genind_data()) +
    # Break legend in to two rows so that it's not cut off
    guides(fill = guide_legend(nrow = 2)) +
    theme(legend.text.align = 0),
  # Plot change in biomass by greenhouse treatment, both SEV Blue and SGS populations
  plot_traits(load_and_clean_trait_data()),
  # Arrange plots in 2x2 with relative size
  rel_widths = c(3, 2),
  nrow = 2,
  ncol = 2
)
# Save plots
ggsave(file = "figures/gracilis_genetic_diversity.pdf",
       height = 6,
       width = 5.5)
