#' Kruskal-Wallis Test with Dunn Post-hoc
#'
#' Performs the Kruskal-Wallis rank-sum test for comparing three or more
#' independent groups, followed by Dunn's post-hoc test with multiple
#' comparison adjustment.
#'
#' This function is a non-parametric alternative to one-way ANOVA and is
#' recommended when normality or homoscedasticity assumptions are violated.
#'
#' @param ... Numeric vectors representing groups, or a data frame with
#'   two or more columns (each column is treated as a group).
#' @param title Character. Plot title.
#' @param xlab Character. X-axis label.
#' @param ylab Character. Y-axis label.
#' @param style Character. Plot style. One of:
#'   \code{"boxplot"}, \code{"violin"}, \code{"mono"}, or \code{"halfeye"}.
#' @param adjust Character. Method for p-value adjustment in Dunn's test.
#'   One of \code{"bonferroni"}, \code{"holm"}, or \code{"BH"}.
#' @param help Logical. If \code{TRUE}, displays a short help message and exits.
#' @param verbose Logical. If \code{TRUE}, prints formatted statistical results
#'   to the console.
#'
#' @return Invisibly returns a list with the following components:
#'   \describe{
#'     \item{type}{Test type.}
#'     \item{H}{Kruskal-Wallis H statistic.}
#'     \item{df}{Degrees of freedom.}
#'     \item{p}{Global test p-value.}
#'     \item{epsilon_sq}{Epsilon-squared effect size.}
#'     \item{epsilon_ci}{Bootstrap confidence interval for effect size.}
#'     \item{means_sd}{Group means and standard deviations.}
#'     \item{dunn}{Dunn post-hoc results.}
#'     \item{significant_pairs}{Significant pairwise comparisons.}
#'     \item{data}{Long-format data used in the analysis.}
#'   }
#'
#' @examples
#' set.seed(123)
#'
#' n <- 25
#'
#' df <- data.frame(
#'   control    = rexp(n, rate = 1),
#'   treatment1 = rexp(n, rate = 0.6),
#'   treatment2 = rgamma(n, shape = 2, scale = 1)
#' )
#'
#' test.kruskal(df)
#'
#' @export

test.kruskal <- function(...,
                         title = "Kruskal-Wallis + Dunn",
                         xlab = "Group",
                         ylab = "Value",
                         style = c("boxplot", "violin", "mono", "halfeye"),
                         adjust = c("bonferroni", "holm", "BH"),
                         help = FALSE,
                         verbose = TRUE) {

  # Capture arguments
  args <- list(...)
  style <- match.arg(style)
  adjust <- match.arg(adjust)

  # Allow data frame input
  if (length(args) == 1 && is.data.frame(args[[1]]) && ncol(args[[1]]) >= 2) {
    groups <- lapply(args[[1]], function(col) col)
    group_names <- colnames(args[[1]])
  } else {
    groups <- args
    raw_names <- as.character(match.call(expand.dots = FALSE)$...)
    group_names <- sub("^.*\\$", "", raw_names)
  }

  # Help message
  if (help || length(groups) < 2) {
    message("
Function test.kruskal()

Description:
  Kruskal-Wallis test (non-parametric ANOVA) followed by Dunn's post-hoc test.
  Ideal for comparing three or more independent groups with non-normal data.

Example:
  set.seed(123)

n <- 25

df <- data.frame(
    control    = rexp(n, rate = 1.5),
    treatment1 = rexp(n, rate = 1),
    treatment2 = rgamma(n, shape = 2, scale = 1)
)

test.kruskal(df, style = 'violin')
")
return(invisible(NULL))
  }

  # Required packages
  required_packages <- c("ggplot2", "FSA", "dplyr", "RColorBrewer")
  lapply(required_packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is not installed.", pkg), call. = FALSE)
    }
  })

  # Data preparation
  data <- data.frame(
    value = unlist(groups),
    group = factor(
      rep(group_names, times = sapply(groups, length)),
      levels = group_names
    )
  )

  # Kruskal-Wallis test
  kruskal_res <- stats::kruskal.test(value ~ group, data = data)
  p_kruskal <- kruskal_res$p.value

  # -----------------------------
  # Effect size: Epsilon squared
  # -----------------------------

  k <- length(unique(data$group))
  n <- nrow(data)
  H <- as.numeric(kruskal_res$statistic)

  epsilon_sq <- (H - k + 1) / (n - k)

  # Bootstrap CI
  set.seed(123)

  boot_eps <- replicate(2000, {

    idx <- sample(seq_len(n), replace = TRUE)
    d_boot <- data[idx, ]

    H_boot <- suppressWarnings(
      kruskal.test(value ~ group, data = d_boot)$statistic
    )

    (H_boot - k + 1) / (n - k)
  })

  eps_ci <- quantile(boot_eps, c(0.025, 0.975), na.rm = TRUE)

  # Means and standard deviations (no automatic printing)
  mean_sd <- aggregate(
    value ~ group,
    data,
    function(x) c(mean = mean(x), sd = sd(x))
  )
  mean_sd <- do.call(data.frame, mean_sd)
  colnames(mean_sd)[2:3] <- c("mean", "sd")

  # -----------------------------
  # Dunn post-hoc test
  # -----------------------------
  suppressMessages({
    suppressWarnings({
      dunn_res <- FSA::dunnTest(
        value ~ group,
        data = data,
        method = adjust
      )
    })
  })

  dunn_df <- dunn_res$res
  significant_pairs <- subset(dunn_df, P.adj < 0.05)

  # --------------------------
  # Labels position
  # --------------------------
  sig_pairs <- significant_pairs

  if (nrow(sig_pairs) > 0) {

    comps <- strsplit(sig_pairs$Comparison, " - ")

    sig_pairs$group1 <- sapply(comps, function(x) trimws(x[1]))
    sig_pairs$group2 <- sapply(comps, function(x) trimws(x[2]))
  }

  group_levels <- levels(data$group)

  sig_pairs$x1 <- match(sig_pairs$group1, group_levels)
  sig_pairs$x2 <- match(sig_pairs$group2, group_levels)

  sig_pairs$signif <- ifelse(sig_pairs$P.adj < 0.001, "***",
                             ifelse(sig_pairs$P.adj < 0.01, "**",
                                    ifelse(sig_pairs$P.adj < 0.05, "*", "")))

  y_max <- max(data$value, na.rm = TRUE)
  y_range <- diff(range(data$value, na.rm = TRUE))

  step <- 0.08 * y_range

  sig_pairs$y <- y_max + seq_len(nrow(sig_pairs)) * step


  # --------------------------
  # Colors
  # --------------------------
  # Vivid colors
  vivid_colors <- scales::hue_pal()(length(unique(data$group)))

  # mono
  n <- length(groups)

  mono_colors <- gray.colors(
    n,
    start = 0.9,
    end = 0.1
  )

  # --------------------------
  # STYLE 1: Boxplot + jitter
  # --------------------------
  if (style == "boxplot") {
    g <- ggplot2::ggplot(data, ggplot2::aes(x = group, y = value, fill = group)) +
      ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.7, linewidth = 0.7) +
      ggplot2::geom_jitter(width = 0.1, alpha = 0.5, color = "grey25") +
      ggplot2::labs(
        title = title,
        subtitle = .build_subtitle_kw(p_kruskal, epsilon_sq),
        x = "",
        y = ylab
      ) +
      ggplot2::scale_fill_manual(values = vivid_colors) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12)
      )
  }

  # --------------------------
  # STYLE 2: Violin + minimalist boxplot
  # --------------------------
  if (style == "violin") {
    g <- ggplot2::ggplot(data, ggplot2::aes(x = group, y = value, fill = group)) +
      ggplot2::geom_violin(
        trim = FALSE,
        alpha = 0.6,
        color = NA,
        adjust = 0.6
      ) +
      ggplot2::geom_boxplot(
        width = 0.18,
        outlier.shape = NA,
        color = "gray20",
        linewidth = 0.4
      ) +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = 0.1),
        alpha = 0.2,
        size = 1.8,
        color = "gray25"
      ) +
      ggplot2::labs(
        title = title,
        subtitle = .build_subtitle_kw(p_kruskal, epsilon_sq),
        x = "",
        y = ylab
      ) +
      ggplot2::scale_fill_manual(values = vivid_colors) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45,
                                            hjust = 1,
                                            size = 12)
      )
  }

  # --------------------------
  # STYLE 3: monochrome premium
  # --------------------------
  if (style == "mono") {
    g <- ggplot2::ggplot(data, ggplot2::aes(x = group, y = value, fill = group)) +
      ggplot2::geom_boxplot(alpha = 0.7,outlier.shape = NA, width = 0.7, linewidth = 0.7, color = "black") +
      ggplot2::geom_jitter(width = 0.1, alpha = 0.2, color = "grey25") +
      ggplot2::labs(
        title = title,
        subtitle = .build_subtitle_kw(p_kruskal, epsilon_sq),
        x = "",
        y = ylab
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::scale_fill_manual(values = mono_colors) +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12))
   }

  # --------------------------
  # STYLE 4: Half-eye (ggdist)
  # --------------------------
  if (style == "halfeye") {
    if (!requireNamespace("ggdist", quietly = TRUE)) {
      stop("Style = 'halfeye' requires the 'ggdist' package.")
    }

    g <- ggplot2::ggplot(data, ggplot2::aes(x = group, y = value, fill = group)) +
      ggdist::stat_halfeye(
        alpha = .6,
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
      ggplot2::labs(
        title = title,
        subtitle = .build_subtitle_kw(p_kruskal, epsilon_sq),
        x = "",
        y = ylab
      ) +
      ggplot2::scale_fill_manual(values = vivid_colors) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12)
      )
  }

  # --------------------------
  # Annotation and print
  # --------------------------
  g <- g + .add_significance(sig_pairs, y_range)

  print(g)

  # --------------------------
  # Return
  # --------------------------
  invisible(list(
    type = "Kruskal-Wallis",
    H = H,
    df = k - 1,
    p = p_kruskal,
    epsilon_sq = epsilon_sq,
    epsilon_ci = eps_ci,
    means_sd = mean_sd,
    dunn = dunn_df,
    significant_pairs = significant_pairs,
    data = data
  ))

  # -----------------------------
  # Output
  # -----------------------------
  if (verbose) {

    .print_header("Kruskal-Wallis")

    .print_block("Statistics", function() {

      cat(
        "H statistic = ",
        round(H, 3),
        " | df = ",
        k - 1,
        " | p = ",
        .format_pval(p_kruskal),
        "\n",
        sep = ""
      )

      cat(
        "Epsilon squared = ",
        round(epsilon_sq, 3),
        " [",
        round(eps_ci[1], 3), ", ",
        round(eps_ci[2], 3),
        "]\n",
        sep = ""
      )
    })

    # --- Post-hoc output ---

    .print_header(paste0("Post-hoc: Dunn (", adjust, ")"))

    .print_block("Significant comparisons", function() {

      if (nrow(significant_pairs) == 0) {

        cat("No significant comparisons (p < 0.05)\n")

      } else {

        for (i in seq_len(nrow(significant_pairs))) {

          r <- significant_pairs[i, ]

          comps <- unlist(strsplit(r$Comparison, " - "))

          g1 <- trimws(comps[1])
          g2 <- trimws(comps[2])

          cat(g1, " vs ", g2, "\n", sep = "")

          cat(
            "Z = ",
            round(r$Z, 3),
            "\n",
            sep = ""
          )

          cat(
            "p (adj) = ",
            .format_pval(r$P.adj),
            "\n\n",
            sep = ""
          )
        }
      }
    })
  }
}
