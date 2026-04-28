#' pre.test() — Automatic statistical test suggestion
#'
#' Automatically identifies whether the input data are numeric or categorical
#' and suggests the most appropriate statistical test.
#'
#' @param ... Two or more vectors (numeric or categorical), or a data.frame with >= 2 columns
#' @param alpha Significance level. Default = 0.05
#' @param help Logical. If TRUE, shows detailed help
#' @param verbose Logical. If TRUE, prints informative messages
#' @importFrom tidyselect everything
#' @return Invisible list with normality results, homogeneity or contingency table,
#' and test recommendation
#' @export

pre.test <- function(..., alpha = 0.05, help = FALSE, verbose = TRUE) {

  # ============================
  # Capture input
  # ============================
  args <- list(...)

  # Data frame input (>= 2 columns)
  if (length(args) == 1 && is.data.frame(args[[1]]) && ncol(args[[1]]) >= 2) {
    groups <- lapply(args[[1]], identity)
    group_names <- colnames(args[[1]])
  } else {
    groups <- args
    raw_names <- as.character(match.call(expand.dots = FALSE)$...)
    group_names <- sub("^.*\\$", "", raw_names)
  }

  # ============================
  # Help message
  # ============================
  if (help || length(groups) < 2) {

    if (verbose) {
      message("
pre.test()

Description:
  Automatically identifies whether the data are numeric or categorical
  and suggests the most appropriate statistical test.

Usage:
  - Accepts vectors or a data frame with 2 or more columns.

Example (data frame):
  df <- data.frame(
    control   = c(1, 2, 3, 4, 5),
    treatment = c(2, 3, 4, 5, 6)
  )
  pre.test(df)
")
    }

    return(invisible(NULL))
  }

  # ============================
  # Detect data type
  # ============================
  are_numeric <- all(vapply(groups, is.numeric, logical(1)))
  are_categorical <- all(
    vapply(groups, function(x) is.factor(x) || is.character(x), logical(1))
  )

  if (!are_numeric && !are_categorical) {
    stop("All inputs must be either numeric or categorical.")
  }

  # =====================================================================
  # NUMERIC DATA
  # =====================================================================
  if (are_numeric) {

    values <- unlist(groups)
    group_factor <- factor(
      rep(group_names, times = vapply(groups, length, integer(1)))
    )

    data_long <- data.frame(
      value = values,
      group = group_factor
    )

    # ----------------------------
    # Normality test selection
    # ----------------------------
    normality_test <- function(x) {

      n <- length(x)

      if (n <= 50) {
        p_value <- stats::shapiro.test(x)$p.value
        method <- "Shapiro-Wilk"
      } else if (n <= 300) {
        p_value <- nortest::ad.test(x)$p.value
        method <- "Anderson-Darling"
      } else {
        ks_res <- stats::ks.test(
          x,
          "pnorm",
          mean = mean(x),
          sd = stats::sd(x)
        )
        p_value <- ks_res$p.value
        method <- "Kolmogorov-Smirnov"
      }

      list(p_value = p_value, method = method)
    }

    normality_results <- lapply(groups, normality_test)

    p_values <- vapply(normality_results, function(x) x$p_value, numeric(1))
    methods <- vapply(normality_results, function(x) x$method, character(1))

    normality_df <- data.frame(
      group = group_names,
      n = vapply(groups, length, integer(1)),
      method = methods,
      p_value = signif(p_values, 3),
      normal = p_values > alpha
    )

    # ----------------------------
    # Homogeneity of variance
    # ----------------------------
    if (length(groups) > 2) {
      p_homogeneity <- car::leveneTest(value ~ group, data = data_long)$`Pr(>F)`[1]
    } else {
      p_homogeneity <- stats::var.test(groups[[1]], groups[[2]])$p.value
    }

    homogeneous <- p_homogeneity > alpha

    # ----------------------------
    # Test recommendation
    # ----------------------------
    if (length(groups) == 2) {

      if (all(normality_df$normal)) {
        recommendation <- if (homogeneous) {
          "t-test"
        } else {
          "Welch's t-test"
        }
      } else {
        recommendation <- "Mann-Whitney test"
      }

    } else {

      if (all(normality_df$normal) && homogeneous) {
        recommendation <- "ANOVA with Tukey post hoc test"
      } else {
        recommendation <- "Kruskal-Wallis with Dunn post hoc test"
      }
    }

    # ----------------------------
    # Verbose output
    # ----------------------------
    if (verbose) {

      separator <- paste(rep("=", 50), collapse = "")

      message("Normality test results:")
      message(separator)
      print(normality_df)
      message(separator)

      message(
        sprintf(
          "Homogeneity of variances: %s (p = %.3f)",
          ifelse(homogeneous, "homogeneous", "heterogeneous"),
          p_homogeneity
        )
      )

      message(separator)
      message(sprintf("Test recommendation: %s", recommendation))
      message(separator)
    }

    # ----------------------------
    # Return
    # ----------------------------
    return(invisible(list(
      normality = normality_df,
      p_homogeneity = p_homogeneity,
      homogeneous = homogeneous,
      recommendation = recommendation
    )))
  }

  # =====================================================================
  # CATEGORICAL DATA
  # =====================================================================
  if (are_categorical) {

    if (length(args) == 1 && is.data.frame(args[[1]]) && ncol(args[[1]]) == 2) {

      df <- args[[1]]

      df_long <- tidyr::pivot_longer(
        df,
        cols = tidyselect::everything(),
        names_to = "group",
        values_to = "category"
      )

      contingency_table <- table(df_long$group, df_long$category)

    } else {

      categories <- lapply(groups, as.factor)
      contingency_table <- table(categories[[1]], categories[[2]])
    }

    recommendation <- if (any(contingency_table < 5)) {
      "Fisher's exact test"
    } else {
      "Chi-squared test"
    }

    if (verbose) {
      message("Contingency table:")
      print(contingency_table)
      message("Association test recommendation: ", recommendation)
    }

    # ----------------------------
    # Return
    # ----------------------------
    return(invisible(list(
      contingency_table = contingency_table,
      recommendation = recommendation
    )))
  }
}
