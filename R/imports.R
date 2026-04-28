# -------------------------------
# Imports
# -------------------------------

# stats
# ---------------------------------------------------------------------------------
#' @importFrom stats sd aggregate shapiro.test ks.test t.test wilcox.test var.test
#' @importFrom stats TukeyHSD complete.cases setNames na.omit
#' @importFrom stats chisq.test confint formula kruskal.test lm nobs p.adjust qnorm quantile reformulate dist

# grDevices
# ---------------------------------------------------------------------------------
#' @importFrom grDevices gray.colors

# utils
# ---------------------------------------------------------------------------------
#' @importFrom utils globalVariables combn

# dplyr / purrr / tidyverse helpers
# ---------------------------------------------------------------------------------
#' @importFrom dplyr %>%
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom scales hue_pal

# Specific packages for analysis and visualization
# ---------------------------------------------------------------------------------
#' @importFrom car leveneTest
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_jitter geom_violin geom_point geom_segment annotate labs theme_minimal scale_fill_manual element_text
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggExtra ggMarginal

NULL
