#' autotestR
#'
#' designed to simplify the execution of the main statistical tests commonly used in the life sciences.
#' It provides user-friendly functions that automatically generate plots and clear explanations,
#' making statistical analysis more accessible for researchers and students.
#'
#' # Main features
#'
#' - t test (independent and paired)
#' - Mann–Whitney test (Wilcoxon rank-sum)
#' - Multiple group comparison (t test or Mann–Whitney)
#' - Chi-squared test and Fisher’s exact test
#' - One-way ANOVA with Tukey HSD post hoc test
#' - Kruskal–Wallis test with Dunn post hoc test
#' - Pearson, Spearman, and Kendall correlation tests with automatic plots
#' - Diagnostic function that suggests the most appropriate statistical test
#' - Intuitive plots fully integrated into the functions
#'
#' ### Basic usage
#'
#' library(autotestR)
#'
#' # Independent t test
#' group1 <- rnorm(30, 10, 2)
#' group2 <- rnorm(30, 12, 2)
#' test.t(group1, group2)
#'
#' # Chi-squared test
#' var1 <- sample(c("A", "B"), 100, replace = TRUE)
#' var2 <- sample(c("Yes", "No"), 100, replace = TRUE)
#' test.chi(var1, var2)
#'
#' # Multiple test (t test or Mann–Whitney)
#' df <- data.frame(
#'  control   = rnorm(30, 10),
#'  treatment = rnorm(30, 12),
#'  test1     = rnorm(30, 11),
#'  test2     = rnorm(30, 15)
#' )
#' test.tmulti(df)
#'
#' # ANOVA with post hoc test
#' g1 <- rnorm(20, 5)
#' g2 <- rnorm(20, 7)
#' g3 <- rnorm(20, 6)
#' test.anova(g1, g2, g3)
#'
#' # Correlation test
#' x <- rnorm(30)
#' y <- x + rnorm(30, 0, 1)
#' test.correlation(x, y)
#'
#' @name autotestR
#'
"_PACKAGE"
