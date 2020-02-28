# Utility functions for EDGE project
###########################################################################################
library(cowplot)

###########################################################################################


# Statistical

ttest_with_var_check <- function(x, y) {
  # This function checks for equal variance before proceeding with the t test.
  if (var.test(x, y)$p.value > 0.05) {
    equalvar = TRUE
    print("Variance is similar between samples for the following:")
  }
  else{
    equalvar = FALSE
    print("Variance is not similar between samples for the following:")
  }
  print(t.test(x,
               y,
               var.equal = equalvar,
               alternative = "two.sided"))
}


# Preprocessing

filter_and_clean_raw_data <-
  function(df,
           sites,
           years) {
    # This function applies filters to biomass raw data for further processing
    
    df$Site <- as.character(df$Site)
    df$Trt <- as.character(df$Trt)
    
    dat <-
      df %>%
      filter(# Keep only desired years
        Year %in% years
        # Keep only desired sites
        & Site %in% sites
        # Keep only drought treatments specified (Should be global)
        & Trt %in% c(include_in_drt_trt, "con")) %>%
      # If both drought treatments are included, collapse into single "drt" treatment
      mutate(Trt = replace(Trt,
                           Trt == "chr" | Trt == "int",
                           "drt"))
    
    return(dat)
  }


# Plotting

theme_sigmaplot <-
  function(xticks = TRUE,
           ticklen = -0.25) {
    # This function adds Sigma-plot like theme elements to a ggplot object.
    # Use as an additional arg, eg:
    # ggplot() + theme_sigmaplot()
    
    sigmaplot <-
      theme(
        panel.background = element_blank(),
        panel.border = element_rect(size = 1, fill = NA),
        legend.key = element_rect(fill = NA),
        axis.ticks.length.y = unit(ticklen, "cm"),
        axis.ticks.length.y.right = unit(ticklen, "cm"),
        axis.ticks.length.x = unit(ticklen, "cm"),
        axis.text.x = element_text(
          color = "black",
          margin = margin(
            t = 10,
            r = 0,
            b = 0,
            l = 0,
            unit = "pt"
          )
        ),
        axis.text.y = element_text(
          hjust = 1,
          color = "black",
          margin = margin(
            t = 0,
            r = 10,
            b = 0,
            l = 0,
            unit = "pt"
          )
        ),
      )
    if (!xticks) {
      sigmaplot <- sigmaplot +
        theme(axis.ticks.x = element_blank())
    }
    return(sigmaplot)
  }


geom_bar_custom <-
  function(data, fill_name) {
    obj <- geom_bar(
      aes(fill = data[[fill_name]],
          y = mean,
          x = Site),
      stat = "identity",
      position = position_stack(reverse = TRUE),
      color = "black",
      width = 0.5,
    )
    
    return(obj)
  }


geom_point_custom <-
  function(data,
           fill_name = "type",
           dodge = 0) {
    obj <- geom_point(
      aes(fill = data[[fill_name]],
          y = mean,
          x = Site),
      color = "black",
      shape = 21,
      size = 4,
      stat = "identity",
      position = position_dodge(width = dodge)
    )
    
    return(obj)
  }

geom_errorbar_custom <-
  function(data,
           group = NULL,
           add = 0,
           dodge = 0) {
    if (is.null(group)) {
      obj <- geom_errorbar(
        position = position_dodge(width = dodge),
        data = data,
        aes(
          x = Site,
          ymin = mean + add - se,
          ymax = mean + add + se
        ),
        size = 0.5,
        width = 0
      )
    } else {
      obj <- geom_errorbar(
        position = position_dodge(width = dodge),
        data = data,
        aes(
          group = get(group),
          x = Site,
          ymin = mean + add - se,
          ymax = mean + add + se
        ),
        size = 0.5,
        width = 0
      )
    }
    return(obj)
  }


scale_y_continuous_percent <-
  function() {
    # This function makes a ggplot object with specific y axis for percentages
    obj <- scale_y_continuous(
      limits = c(0, 105),
      breaks = c(0,
                 20,
                 40,
                 60,
                 80,
                 100),
      expand = c(0, 0),
      labels = c(0,
                 20,
                 40,
                 60,
                 80,
                 100),
      sec.axis = dup_axis(labels = NULL, name = "")
    )
    
    return(obj)
  }


scale_y_continuous_percent_ticks <-
  function() {
    # Allows no tick mark at the zero line
    obj <-
      theme(axis.ticks.y = element_line(color = c("transparent",
                                                  rep("black", 5))))
    
    return(obj)
  }


legend_custom <-
  function() {
    obj <- theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_blank()
    )
    
    return(obj)
  }
