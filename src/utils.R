## Utility functions for EDGE project

theme_sigmaplot <- function(xticks = TRUE) {
  
  # This function adds Sigma-plot like theme elements to a ggplot object.
  # Use as an additional arg, eg:
  # ggplot() + theme_sigmaplot()
  
  sigmaplot <- theme(
    panel.background = element_blank(),
    panel.border = element_rect(size = 1, fill = NA),
    legend.key = element_rect(fill = NA),
    axis.ticks.length.y = unit(-0.25, "cm"),
    axis.ticks.length.y.right = unit(-0.25, "cm"),
    axis.ticks.length.x = unit(-0.25, "cm"),
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