#' Multiple pairwise comparisons with diagnostics
#'
#' Performs multiple pairwise comparisons using Student's t-test
#' or Mann-Whitney test, with automatic diagnostics, effect sizes,
#' confidence intervals, multiple testing correction and visualization.
#'
#' @param ... Numeric vectors or a data.frame with groups in columns.
#' @param comparisons List of character vectors specifying pairwise
#'   comparisons (e.g. list(c("A","B"), c("B","C"))).
#'   If NULL, all pairwise combinations are used.
#' @param title Plot title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param style Plot style. One of "boxplot", "violin",
#'   "mono", or "halfeye".
#' @param p_adjust Method for multiple testing correction.
#'   One of "none", "holm", "BH", "bonferroni".
#' @param help Logical. If TRUE, prints usage examples.
#' @param verbose Logical. If TRUE, prints results and plots.
#'
#' @return A list with:
#' \describe{
#'   \item{results}{A tibble with test results.}
#'   \item{plot}{A ggplot object.}
#'   \item{data_long}{Long-format data used for plotting.}
#' }
#'
#' @details
#' Normality is assessed using Shapiro-Wilk tests and homogeneity
#' of variances using Levene's test. If assumptions are met, a
#' pooled-variance t-test is used. Otherwise, the Mann-Whitney test
#' is applied with bootstrap confidence intervals.
#'
#' Effect sizes:
#' \itemize{
#'   \item Cohen's d for t-tests
#'   \item Rank-biserial correlation for Mann-Whitney
#' }
#'
#' @examples
#' df <- data.frame(
#'   control   = rnorm(30, 10),
#'   treatment = rnorm(30, 12),
#'   test1     = rnorm(30, 11),
#'   test2     = rnorm(30, 15)
#' )
#'
#' test.tmulti(
#'   df,
#'   comparisons = list(
#'     c("control", "treatment"),
#'     c("treatment", "test1")
#'   )
#' )
#'
#' @export

test.tmulti <- function(...,
                        comparisons = NULL,
                        title = "Multiple comparisons (t / MW)",
                        xlab = "",
                        ylab = "Value",
                        style = c("boxplot", "violin", "mono", "halfeye"),
                        p_adjust = c("none", "holm", "BH", "bonferroni"),
                        help = FALSE,
                        verbose = TRUE) {

  # ==============================
  # Argument handling
  # ==============================

  style    <- match.arg(style)
  p_adjust <- match.arg(p_adjust)


  # ==============================
  # Quick help
  # ==============================

  if (help || length(list(...)) == 0) {

    message("
Function test.tmulti()

Performs multiple pairwise comparisons using
Student t-test or Mann-Whitney, with diagnostics.

Example:
df <- data.frame(
  control   = rnorm(30, 10),
  treatment = rnorm(30, 12),
  test1     = rnorm(30, 11),
  test2     = rnorm(30, 15)
)

test.tmulti(
  df,
  comparisons = list(c('control','treatment'),
                     c('treatment','test1'))
)
")

return(invisible(NULL))
  }

  # ==============================
  # Required packages
  # ==============================

  required_packages <- c(
    "ggplot2", "purrr", "tibble",
    "tidyr", "dplyr", "scales", "car"
  )

  for (pkg in required_packages) {

    if (!requireNamespace(pkg, quietly = TRUE)) {

      stop(
        sprintf("Package '%s' is required.", pkg),
        call. = FALSE
      )
    }
  }

  has_ggdist <- requireNamespace("ggdist", quietly = TRUE)


  # ==============================
  # Capture input
  # ==============================

  dots <- list(...)

  if (length(dots) == 1 && is.data.frame(dots[[1]])) {

    df <- as.data.frame(dots[[1]])
    group_order <- names(df)

  } else {

    group_values <- dots
    group_names  <- sapply(substitute(list(...))[-1], deparse)

    df <- as.data.frame(group_values)
    names(df) <- group_names

    group_order <- group_names
  }

  if (!all(sapply(df, is.numeric))) {
    stop("All columns must be numeric.")
  }


  # ==============================
  # Normalize comparisons
  # ==============================

  to_pairs <- function(x) {

    if (is.list(x) &&
        all(sapply(x, function(z)
          is.character(z) && length(z) == 2))) {

      return(x)
    }

    if (is.character(x) && length(x) == 2) {
      return(list(x))
    }

    if (is.character(x) && length(x) == 1 &&
        grepl("[-,;]", x)) {

      parts <- trimws(unlist(strsplit(x, "[-,;]+")))

      if (length(parts) == 2) {
        return(list(parts))
      }
    }

    if (is.character(x) && length(x) > 2) {

      if (length(x) %% 2 != 0) {
        stop("Character vector must have even length.")
      }

      return(split(x, rep(seq_along(x)/2, each = 2)))
    }

    stop("Invalid 'comparisons' format.")
  }


  if (is.null(comparisons)) {

    cmb <- combn(group_order, 2)
    comparisons <- split(t(cmb), seq_len(ncol(cmb)))

  } else {

    comparisons <- to_pairs(comparisons)
  }


  # ==============================
  # Long format
  # ==============================

  data_long <- tibble::as_tibble(df) |>
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to  = "group",
      values_to = "value"
    )

  data_long$group <- factor(
    data_long$group,
    levels = group_order
  )


  # ==============================
  # Pair analysis
  # ==============================

  analyze_pair <- function(g1, g2) {

    v1 <- na.omit(df[[g1]])
    v2 <- na.omit(df[[g2]])


    # ------------------
    # Diagnostics
    # ------------------

    p_norm1 <- if (length(v1) >= 3)
      shapiro.test(v1)$p.value else NA

    p_norm2 <- if (length(v2) >= 3)
      shapiro.test(v2)$p.value else NA

    is_normal <- all(c(p_norm1, p_norm2) > 0.05,
                     na.rm = TRUE)


    lev <- tryCatch(
      car::leveneTest(
        value ~ group,
        data = data_long[
          data_long$group %in% c(g1, g2), ]
      ),
      error = function(e) NULL
    )

    is_homogeneous <-
      !is.null(lev) && lev$`Pr(>F)`[1] > 0.05

    # Decision
    use_t <- is_normal && is_homogeneous

    # ------------------
    # t-test
    # ------------------

    if (use_t) {

      res <- t.test(v1, v2, var.equal = TRUE)

      nx <- length(v1)
      ny <- length(v2)

      mean_diff <- mean(v1) - mean(v2)

      sd_pooled <- sqrt(
        ((nx - 1)*sd(v1)^2 +
           (ny - 1)*sd(v2)^2)/(nx + ny - 2)
      )

      d <- if (sd_pooled > 0)
        mean_diff / sd_pooled else NA


      return(
        tibble::tibble(

          group1 = g1,
          group2 = g2,

          test_type = "t",
          test_name = "Student t-test",

          estimate = mean_diff,

          ci_low  = res$conf.int[1],
          ci_high = res$conf.int[2],

          effect = d,

          p_value = res$p.value
        )
      )
    }

    # ------------------
    # Mann-Whitney
    # ------------------

    res <- wilcox.test(v1, v2, exact = FALSE)

    U <- as.numeric(res$statistic)

    nx <- length(v1)
    ny <- length(v2)

    r_rb <- 1 - (2*U)/(nx*ny)

    # ----------------------------
    # Bootstrap CI (median diff)
    # ----------------------------

    res_boot <- .boot_two_sample(
      v1,
      v2,
      stat_fun = function(a, b)
        median(a, na.rm = TRUE) - median(b, na.rm = TRUE)
    )

    ci_low  <- res_boot$ci_low
    ci_high <- res_boot$ci_high

    med_diff <- median(v1) - median(v2)


    tibble::tibble(

      group1 = g1,
      group2 = g2,

      test_type = "mw",
      test_name = "Mann-Whitney",

      estimate = med_diff,

      ci_low  = ci_low,
      ci_high = ci_high,

      effect = r_rb,

      p_value = res$p.value
    )
  }

  # ==============================
  # Run tests
  # ==============================

  results <- purrr::map_dfr(
    comparisons,
    ~ analyze_pair(.x[1], .x[2])
  )

  # ==============================
  # p-value adjustment
  # ==============================

  if (p_adjust != "none") {

    results$p_adj <- p.adjust(
      results$p_value,
      method = p_adjust
    )

  } else {

    results$p_adj <- results$p_value
  }

  # ==============================
  # Labels position
  # ==============================
  results$signif <- dplyr::case_when(
    results$p_adj < 0.001 ~ "***",
    results$p_adj < 0.01  ~ "**",
    results$p_adj < 0.05  ~ "*",
    TRUE ~ ""
  )

  results$x1 <- match(results$group1, group_order)
  results$x2 <- match(results$group2, group_order)

  y_max <- max(data_long$value, na.rm = TRUE)
  y_range <- diff(range(data_long$value, na.rm = TRUE))

  step <- 0.08 * y_range

  results$y <- y_max + seq_len(nrow(results)) * step

  results <- results |>
    dplyr::mutate(dist = abs(x1 - x2)) |>
    dplyr::arrange(dist)

  signif_pairs <- results |>
    dplyr::filter(signif != "")

  signif_pairs$y <- y_max + seq_len(nrow(signif_pairs)) * step

  # ==============================
  # Colors
  # ==============================
  # Vivid colors
  vivid_colors <- scales::hue_pal()(length(unique(data_long$group)))

  # mono
  n <- length(group_order)

  mono_colors <- gray.colors(
    n,
    start = 0.9,
    end = 0.1
  )

  # ==============================
  # Plot
  # ==============================
  base_theme <-
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        size = 12
      )
    )

  # ----------------------------
  # Style 1 (Boxplot)
  # ----------------------------
  if (style == "boxplot") {

    g <- ggplot2::ggplot(
      data_long,
      ggplot2::aes(group, value, fill = group)
    ) +
      ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA) +
      ggplot2::geom_jitter(width = 0.1, alpha = 0.4) +
      ggplot2::scale_fill_manual(values = vivid_colors) +
      base_theme +
      ggplot2::labs(title = title, x = "", y = ylab)
    }

  # ----------------------------
  # Style 2 (Violin)
  # ----------------------------
  if (style == "violin") {

  g <- ggplot2::ggplot(
    data_long,
    ggplot2::aes(group, value, fill = group)
  ) +
    ggplot2::geom_violin(
      trim = FALSE,
      alpha = 0.55,
      color = NA,
      adjust = .6
    ) +
    ggplot2::geom_boxplot(
      width = 0.18,
      outlier.shape = NA
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_jitter(width = .1),
      alpha = .4,
      size = 1.8,
      color = "gray25"
    ) +
    ggplot2::scale_fill_manual(values = vivid_colors) +
    base_theme +
    ggplot2::labs(title = title, x = "", y = ylab)
  }

  # ----------------------------
  # Style 3 (monochrome)
  # ----------------------------
  if (style == "mono") {

  g <- ggplot2::ggplot(
    data_long,
    ggplot2::aes(group, value, fill = group)
  ) +
    ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.1, alpha = 0.4) +
    ggplot2::scale_fill_manual(values = mono_colors) +
    base_theme +
    ggplot2::labs(title = title, x = "", y = ylab)
  }

  # ----------------------------
  # Style 4 (Half eye)
  # ----------------------------
  if (style == "halfeye" && has_ggdist) {

  g <- ggplot2::ggplot(
    data_long,
    ggplot2::aes(group, value, fill = group)
  ) +
    ggdist::stat_halfeye(
      alpha = 0.6,
      trim = FALSE,
      adjust = 0.6,
      width = 0.6,
      .width = c(0.5, 0.8, 0.95),
      justification = -0.2,
      slab_color = "gray20",
      interval_color = "gray20"
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_nudge(x = 0.15),
      size = 1.1,
      alpha = 0.4,
      color = "black"
    ) +
    ggdist::stat_pointinterval(
      position = ggplot2::position_nudge(x = 0.2),
      point_color = "black",
      interval_color = "black",
      .width = 0.95
    ) +
    ggplot2::scale_fill_manual(values = vivid_colors) +
    base_theme +
    ggplot2::labs(title = title, x = "", y = ylab)
  }

  # --------------------------
  # Annotation and print
  # --------------------------
  g <- g + .add_significance(signif_pairs, y_range)

  print(g)

  # ==============================
  # Output
  # ==============================
  obj <- (list(
    results   = results,
    plot      = g,
    data_long = data_long
  ))

  # ==============================
  # Return
  # ==============================
  if (verbose) {

    .print_header("Pairwise comparisons")

    .print_block("Results", function() {

      for (i in seq_len(nrow(results))) {

        r <- results[i, ]


        cat(
          r$group1, " vs ", r$group2,
          " (", r$test_name, ")\n",
          sep = ""
        )


        cat(
          "Estimate = ",
          round(r$estimate, 2),
          " [",
          round(r$ci_low, 2), ", ",
          round(r$ci_high, 2),
          "]\n",
          sep = ""
        )


        es_label <- if (r$test_type == "t") {
          "Cohen's d"
        } else {
          "Rank-biserial r"
        }

        cat(
          es_label,
          " = ",
          round(r$effect, 3),
          "\n",
          sep = ""
        )

        p_raw  <- r$p_value
        p_adjf <- r$p_adj

        p_fmt  <- .format_pval(p_raw)
        p_adjf <- .format_pval(p_adjf)

        if (p_adjust == "none") {

          cat(
            "p = ",
            .format_pval(r$p_value),
            "\n\n",
            sep = ""
          )

        } else {

          cat(
            "p = ",
            .format_pval(r$p_value),
            " (adj = ",
            .format_pval(r$p_adj),
            ", ",
            p_adjust,
            ")\n\n",
            sep = ""
          )
        }
      }
    })
  }

  return(invisible(list(result = obj)))
}


