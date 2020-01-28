# Utility functions for EDGE project
###########################################################################################
library(cowplot)

###########################################################################################


theme_sigmaplot <- function(xticks = TRUE,
                            ticklen = -0.25) {
  # This function adds Sigma-plot like theme elements to a ggplot object.
  # Use as an additional arg, eg:
  # ggplot() + theme_sigmaplot()
  
  sigmaplot <- theme(
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