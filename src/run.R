# Execution script for EDGE Science project pipeline.
# Generates plots and prints results to the R console.
###########################################################################################
# Set working directory
#setwd("/Users/hoffman ava/EDGE-science/")
setwd("/Users/avahoffman/Dropbox/Research/EDGE_Science/EDGE-science/")

# General functions and configuration
source("src/config.R")
source("src/utils.R")

# Dataset specific functions
source("src/site_map.R")
source("src/sensitivity.R")
source("src/C3_v_c4.R")
source("src/eriopoda_v_gracilis.R")
source("src/genetic_diversity.R")
source("src/gracilis_traits.R")

setwd(wd)


###########################################################################################
# Load libraries
library(ggplot2)
library(cowplot)


###########################################################################################


# Plot ecoregion and site map
# Plot showing sensitivity underlaid with Huxman 2004 data
plot_grid(
  plot_site_map_with_ecoregions(generate_shapefile_data()),
  make_sensitivity_plot(get_huxman_2004_data(),
                        get_edge_data())
)
ggsave(file = "figures/sites_sensitivity.pdf",
       height = 3,
       width = 5.5)


# C3 and C4 comparison from SGS and CHY
plot_grid(
  plot_c3_v_c4(summarize_ambient_data(
    collect_c3_c4_data(sum_across_years = TRUE)
  )),
  plot_c3_v_c4_diff(summarize_difference_data(
    collect_c3_c4_data(sum_across_years = TRUE)
  ))
)
ggsave(file = "figures/c3_v_c4.pdf",
       height = 3,
       width = 5.5)


# Bouteloua gracilis versus Bouteloua eriopoda comparison at Sevilleta Blue and Sevilleta
# Black, respectively
plot_grid(
  plot_spp_sev(
    collect_sev_data(ambient_composition = TRUE,
                     sum_across_years = TRUE)
  ) +
    guides(fill = guide_legend(nrow = 2)),
  plot_sev_diff(
    collect_sev_data(ambient_composition = FALSE,
                     sum_across_years = TRUE)
  )
)
ggsave(file = "figures/eriopoda_v_gracilis.pdf",
       height = 3,
       width = 5.5)


# Genetic differences between sites (Sevilleta and SGS) of Bouteloua gracilis:
# Genetic diversity
# Trait differences between sites
perform_cross_validation_DAPC(load_and_clean_genind_data(), XV_skip = XV_skip)
plot_grid(
  plot_dapc(load_and_clean_genind_data()) +
    guides(fill = guide_legend(nrow = 2)),
  plot_traits(load_and_clean_trait_data()),
  rel_widths = c(3,2)
)
ggsave(file = "figures/gracilis_genetic_diversity.pdf",
       height = 3,
       width = 5.5)
