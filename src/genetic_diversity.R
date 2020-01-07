###########################################################################################
## set working directory
setwd("/Users/avahoffman/Dropbox/Research/EDGE_Science/EDGE-science/src/")
source("config.R")
source("utils.R")
setwd(wd)

###########################################################################################
## load libraries
library(adegenet) ## deal with genind objects
library(ade4)
library(ggplot2)
library(cowplot)
library(reshape2)
library(dplyr)
library(tidyr)

###########################################################################################


load_and_clean_genind_data <- function() {
  # Load processed SNP genind data
  load(genetic_data)
  # Remove all but one of each clone (clones were used to test efficacy of this method)
  indNames(genind.data1)
  genind_1clone_only <-
    genind.data1[c(5, 12, 18, 21, 27, 33, 41, 44, 48, 50, 55, 58, 65:335)]
  # Clone sites were either EDGE or HQ - replace with SGS since they came from that site generally
  indNames(genind_1clone_only) <-
    gsub("Bgedge", "SGS", indNames(genind_1clone_only))
  indNames(genind_1clone_only) <-
    gsub("BgHq", "SGS", indNames(genind_1clone_only))
  indNames(genind_1clone_only)
  # Impute mean for missing data
  genind_1clone_only$tab <-
    tab(genind_1clone_only, NA.method = "mean")
  # Filter out only desired populations
  genind_final <-
    genind_1clone_only[(pop(genind_1clone_only) %in% genetic_pops_to_use),]
  return(genind_final)
}


perform_cross_validation_DAPC <- function(genind_final, XV_skip) {
  # NOTE THAT THIS IS COMPUTATIONALLY EXPENSIVE
  if (!XV_skip) {
    # Group is population
    grp <- pop(genind_final)
    # Plot CV results
    setwd(figure_dir)
    pdf("XV_genetic.pdf")
    xval <-
      xvalDapc(
        genind_final$tab,
        grp,
        n.pca.max = 100,
        training.set = 0.8,
        result = "groupMean",
        center = TRUE,
        scale = FALSE,
        n.pca = NULL,
        n.rep = 50,
        xval.plot = TRUE,
        parallel = "multicore"
      )
    dev.off()
    setwd(wd)
    # How much variance retained?
    print(xval$DAPC$var)
    # How many PCAs to use?
    print(xval$DAPC$n.pca)
  }
}


plot_dapc <- function(genind_final, filename) {
  # Make the data frame
  # Group is population
  grp <- pop(genind_final)
  DAPC <- dapc(genind_final$tab,
               grp,
               n.pca = n_prin_comp,
               n.da = (length(genetic_pops_to_use) - 1))
  # Recall how much variance retained
  print(DAPC$var)
  ## Combine LDs and Pop names
  plot_dat <- as.data.frame(DAPC$ind.coord)
  plot_dat <- cbind(plot_dat, DAPC$grp)
  # Rename column
  names(plot_dat)[length(genetic_pops_to_use)] <- "pop"
  # Save plot data
  setwd(write_dir)
  write.csv(plot_dat, file = "genetic/DAPC_loadings.csv")
  setwd(wd)
  # Set colors by reordering alphabetically
  custom_colors_bold <- DAPC_colors_bold[order(genetic_pops_to_use)]
  custom_colors_pale <- DAPC_colors_pale[order(genetic_pops_to_use)]
  sorted_pops <- sort(genetic_pops_to_use)
  plot_dat$ordered_pop <- tolower(plot_dat$pop)
  
  gg <- ggplot(data = plot_dat,
               aes(x = LD1, y = LD2)) +
    theme_sigmaplot() +
    scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = "")) +
    scale_x_continuous(sec.axis = dup_axis(labels = NULL, name = ""))
  for (i in 1:4) {
    # Plot an ellipse for each pop
    sub_dat = subset(plot_dat, `pop` == sorted_pops[i])
    gg <- gg + stat_ellipse(data = sub_dat,
                            color = custom_colors_pale[i])
  }
  gg <- gg +
    geom_point(
      aes(fill = ordered_pop),
      shape = 21,
      size = 2.5,
      color = "grey"
    ) +
    scale_fill_manual(
      name = "",
      values = custom_colors_pale,
      labels = c("NB", "KNZ", "SEV", "SGS")
    ) +
    theme(legend.position = "top")
  
  gg
  ggsave(file = filename,
         height = 3,
         width = 4)
  return(gg)
}


plot_prob_assign <- function(genind_final, filename) {
  # Make the data frame
  # Group is population
  grp <- pop(genind_final)
  DAPC <- dapc(genind_final$tab,
               grp,
               n.pca = n_prin_comp,
               n.da = (length(genetic_pops_to_use) - 1))
  ## "Structure" plot
  # Add true population to predictions (posterior) and convert to long format
  posts <- cbind(DAPC$posterior, as.data.frame(DAPC$grp))
  posts <- posts %>%
    rename(true_pop = `DAPC$grp`) %>%
    filter(true_pop %in% c("SGS", "Sevilleta")) %>%
    gather(id_pop, prob, SGS:Sevilleta)
  # Summarize by site
  summary_dat <- posts %>% group_by(true_pop, id_pop) %>%
    summarise(mean = mean(prob),
              se = sd(prob) / sqrt(n()))
  
  # Set colors by reordering alphabetically
  custom_colors_pale <- DAPC_colors_pale[order(genetic_pops_to_use)]
  
  gg <- ggplot(data = summary_dat) +
    
    # Draw bars
    geom_bar(
      aes(fill = id_pop, y = mean, x = true_pop),
      stat = "identity",
      position = position_stack(),
      color = "black",
      width = 0.5,
    ) +
    
    # Add standard error for SGS + SGS
    geom_errorbar(
      data = summary_dat %>% filter(id_pop == true_pop),
      aes(
        x = true_pop,
        ymin = mean - se,
        ymax = mean + se
      ),
      size = 1,
      width = 0
    ) +
    
    # Add theme and adjust axes
    theme_sigmaplot(xticks = FALSE) +
    scale_y_continuous(
      limits = c(0, 1.05),
      breaks = c(0.0025, .20, .40, .60, .80, 1.00),
      expand = c(0, 0),
      labels = c(0, .20, .40, .60, .80, "1.0"),
      sec.axis = dup_axis(labels = NULL, name = "")
    ) +
    
    ylab("Probability of Assignment") +
    xlab(NULL) +
    
    # Adjust legend and colors
    theme(legend.position = "none") +
    scale_fill_manual(values = custom_colors_pale) +
    scale_x_discrete(labels = c("SGS", "SEV"))
  
  gg
  ggsave(file = filename,
         height = 2.5,
         width = 3.5)
  return(gg)
}


plot_genetic_structure <- function(genind_final, filename) {
  # TODO: CLEANUP, NEEDS WORK
  # Make the data frame
  # Group is population
  grp <- pop(genind_final)
  DAPC <- dapc(genind_final$tab,
               grp,
               n.pca = n_prin_comp,
               n.da = (length(genetic_pops_to_use) - 1))
  ## "structure" plot
  # Add true population to predictions (posterior)
  posts <- cbind(DAPC$posterior, as.data.frame(DAPC$grp))
  posts <- posts %>% rename(true_pop = `DAPC$grp`)
  posts$id <- row.names(posts)
  # Make long format
  long_dat <- posts %>% gather(id_pop, prob, SGS:Sevilleta)
  long_dat <- long_dat[order(tolower(long_dat$true_pop)),]
  
  # write.csv(long.dat, "genomics_output/Structure_plot_data_total.csv")
  # write.csv(posts, "genomics_output/Structure_plot_data_total_wide.csv")
  
  gg <- ggplot(data = long_dat,
               aes(x = long_dat$id, y = long_dat$prob)) +
    geom_bar(
      aes(fill = long_dat$id_pop),
      stat = "identity",
      position = position_stack(),
      width = 0.9
    ) +
    scale_fill_manual(values = c(
      "grey",
      "lightgrey",
      northern_color_pale,
      shortgrass_color_pale
    )) +
    labs(fill = "Population") + xlab("Individual") + ylab("Probability of Assignment") +
    theme_sigmaplot(xticks = FALSE) +
    scale_y_continuous(
      limits = c(0, 1.00001),
      breaks = c(0, .20, .40, .60, .80, 1.00),
      expand = c(0, 0),
      labels = c(0, .20, .40, .60, .80, "1.0"),
      sec.axis = dup_axis(labels = NULL, name = "")
    ) +
    scale_x_discrete(expand = c(-1.05, 0)) +
    theme(
      legend.position = "top",
      axis.text.x = element_blank(),
      panel.border = element_blank(),
      axis.line.y = element_line(colour = "black", size = 1)
    )
  
  gg
  ggsave(file = filename,
         height = 2,
         width = 3)
  return(gg)
}



combine_dapc_and_assign <-
  function(plot_dapc, plot_prob, filename) {
    # Make an inset for prob of assignment
    plot_with_inset <-
      ggdraw(plot_dapc) +
      draw_plot(
        plot_prob + ylab("Probability of\nAssignment"),
        x = 0.55,
        y = 0.115,
        height = 0.29,
        width = 0.4
      )
    
    plot_with_inset
    ggsave(file = filename,
           height = 4,
           width = 5)
    return(plot_with_inset)
  }

perform_cross_validation_DAPC(load_and_clean_genind_data(), XV_skip = TRUE)
combine_dapc_and_assign(
  plot_dapc(load_and_clean_genind_data(),
            filename = "figures/genetic_dapc.pdf"),
  plot_prob_assign(load_and_clean_genind_data(),
                   filename = "figures/genetic_prob_assign.pdf"),
  "figures/genetic_dapc_assign_inset.pdf"
)
