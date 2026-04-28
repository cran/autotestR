#' ANOVA test with automated assumption checking
#'
#' Performs ANOVA (and Tukey HSD) if data meet normality and homogeneity assumptions.
#' Otherwise, automatically recommends Kruskal-Wallis/Dunn.
#'
#' @param ... Vectors or a data.frame with >= 2 columns.
#' @param title Plot title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param style Aesthetic style of the generated plot.
#' @param help If TRUE, shows help.
#' @param verbose If TRUE, shows detailed messages.
#' @importFrom stats aov sd aggregate shapiro.test var.test
#' @return An `aov` object or a recommendation message.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   control = rnorm(30, 10),
#'   treatment = rnorm(30, 12),
#'   test = rnorm(30, 11)
#' )
#' test.anova(df)

test.anova <- function(...,
                       title = "ANOVA/Tukey HSD",
                       xlab = "Group",
                       ylab = "Value",
                       style = c("boxplot", "violin", "mono", "halfeye"),
                       help = FALSE,
                       verbose = TRUE) {

  style <- match.arg(style)

  # --- Quick help block ---
  if (help || length(list(...)) == 0) {
    message("
test.anova() function

Description:
  Performs ANOVA between numeric groups, followed by Tukey HSD,
  if normality and homogeneity assumptions are met.
  Otherwise, recommends Kruskal-Wallis/Dunn.

Example:
  df <- data.frame(
    control = rnorm(30, 10, sd = 0.5),
    treatment = rnorm(30, 11, sd = 0.5),
    test = rnorm(30, 11, sd = 0.5)
  )

test.anova(df)
")
    return(invisible(NULL))
  }

  required_packages <- c("ggplot2", "car")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Please install package: ", pkg))
    }
  }

  dots <- list(...)
  if (length(dots) == 1 && is.data.frame(dots[[1]])) {
    df <- dots[[1]]
    groups <- as.list(df)
    group_names <- names(df)
  } else {
    groups <- dots
    group_names <- sapply(substitute(list(...))[-1], deparse)
  }

  values <- unlist(groups)
  group_factor <- factor(rep(group_names, times = sapply(groups, length)))
  data_long <- data.frame(value = values, group = group_factor)

  # -------------------------
  # Normality test
  # -------------------------
  apply_normality_test <- function(x) {

    out <- tryCatch(
      shapiro.test(x)$p.value,
      error = function(e) NA
    )

    out
  }

  p_normal <- sapply(groups, apply_normality_test)

  normal <- all(!is.na(p_normal) & p_normal > 0.05)

  p_levene <- tryCatch({
    if (length(groups) > 2) {
      car::leveneTest(value ~ group, data = data_long)$`Pr(>F)`[1]
    } else {
      var.test(groups[[1]], groups[[2]])$p.value
    }
  }, error = function(e) NA)
  homogeneous <- !is.na(p_levene) && p_levene > 0.05

  # -------------------------
  # If assumptions fail
  # -------------------------
  if (!normal || !homogeneous) {
    if (verbose) message("\nAssumptions failed. Recommendation: Kruskal-Wallis/Dunn")

    means_sd <- aggregate(value ~ group, data = data_long,
                          function(x) c(mean = mean(x), sd = sd(x)))
    means_sd <- do.call(data.frame, means_sd)
    colnames(means_sd)[2:3] <- c("mean", "sd")

    return(invisible(list(
      type = "ANOVA - assumptions not met",
      normal = normal,
      p_normal = p_normal,
      homogeneous = homogeneous,
      p_levene = p_levene,
      recommendation = "Use Kruskal-Wallis/Dunn",
      means_sd = means_sd
    )))
  }

  # -------------------------
  # ANOVA
  # -------------------------
  model <- aov(value ~ group, data = data_long)
  p_anova <- summary(model)[[1]][["Pr(>F)"]][1]

  # --- Effect size: Omega squared (ω²) ---
  anova_tab <- summary(model)[[1]]

  ss_between <- anova_tab[1, "Sum Sq"]
  ss_within  <- anova_tab[nrow(anova_tab), "Sum Sq"]

  df_between <- anova_tab[1, "Df"]
  ms_within  <- anova_tab[nrow(anova_tab), "Mean Sq"]

  ss_total <- ss_between + ss_within

  omega_sq <- (ss_between - df_between * ms_within) /
    (ss_total + ms_within)

  F_value <- anova_tab[1, "F value"]
  df1 <- anova_tab[1, "Df"]
  df2 <- anova_tab[nrow(anova_tab), "Df"]
  n_total <- nrow(data_long)

  # --- Bootstrap CI for omega squared ---
  boot_omega <- .boot_anova_omega(
    data  = data_long,
    group = "group",
    value = "value",
    B = 2000
  )

  omega_ci_low  <- boot_omega$ci_low
  omega_ci_high <- boot_omega$ci_high

  # Tukey table
  tukey_res <- TukeyHSD(model)$group
  tukey_df <- as.data.frame(tukey_res)

  # Significant Tukey pairs (p < 0.05)
  tukey_pairs <- data.frame(
    Comparison = rownames(tukey_res),
    diff = tukey_res[, "diff"],
    lwr  = tukey_res[, "lwr"],
    upr  = tukey_res[, "upr"],
    p_adj = tukey_res[, "p adj"],
    stringsAsFactors = FALSE
  )

  significant_pairs <- subset(tukey_pairs, p_adj < 0.05)

  tukey_df$Comparison <- rownames(tukey_res)
  rownames(tukey_df) <- NULL

  # Means and SD
  means_sd <- aggregate(value ~ group, data = data_long,
                        function(x) c(mean = mean(x), sd = sd(x)))
  means_sd <- do.call(data.frame, means_sd)
  colnames(means_sd)[2:3] <- c("mean", "sd")

  # --------------------------
  # Labels position
  # --------------------------
  sig_pairs <- significant_pairs

  if (nrow(sig_pairs) > 0) {

    comps <- strsplit(sig_pairs$Comparison, "-")

    sig_pairs$group1 <- sapply(comps, function(x) trimws(x[1]))
    sig_pairs$group2 <- sapply(comps, function(x) trimws(x[2]))
  }

  group_levels <- levels(data_long$group)

  sig_pairs$x1 <- match(sig_pairs$group1, group_levels)
  sig_pairs$x2 <- match(sig_pairs$group2, group_levels)

  sig_pairs$signif <- ifelse(sig_pairs$p_adj < 0.001, "***",
                             ifelse(sig_pairs$p_adj < 0.01, "**",
                                    ifelse(sig_pairs$p_adj < 0.05, "*", "")))

  y_max <- max(data_long$value, na.rm = TRUE)
  y_range <- diff(range(data_long$value, na.rm = TRUE))

  step <- 0.08 * y_range

  sig_pairs$y <- y_max + seq_len(nrow(sig_pairs)) * step

  # --------------------------
  # Subtitle
  # --------------------------
  subtitle_text <- .make_subtitle_anova(
    omega_sq = omega_sq,
    p_value = p_anova
  )

  # --------------------------
  # Colors
  # --------------------------
  # Vivid colors
  vivid_colors <- scales::hue_pal()(length(unique(data_long$group)))

  # mono
  n <- length(groups)

  mono_colors <- gray.colors(
    n,
    start = 0.9,
    end = 0.1
  )

  # --------------------------
  # Boxplot
  # --------------------------
  if (style == "boxplot") {
    g <- ggplot2::ggplot(data_long, ggplot2::aes(x = group, y = value, fill = group)) +
      ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.7, linewidth = 0.7) +
      ggplot2::geom_jitter(width = 0.1, alpha = 0.2, color = "grey25") +
      ggplot2::labs(title = title, subtitle = subtitle_text, x = "", y = ylab) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::scale_fill_manual(values = vivid_colors) +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12))

}

  # --------------------------
  # Violin plot
  # --------------------------
  if (style == "violin") {
    g <- ggplot2::ggplot(data_long, ggplot2::aes(x = group, y = value, fill = group)) +
      ggplot2::geom_violin(trim = FALSE, alpha = 0.6, color = NA, adjust = 0.6) +
      ggplot2::geom_boxplot(width = 0.18, outlier.shape = NA,
                            color = "gray20", linewidth = 0.4) +
      ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.1),
                          alpha = 0.2, size = 1.8, color = "gray25") +
      ggplot2::labs(title = title, subtitle = subtitle_text, x = "", y = ylab) +
      ggplot2::scale_fill_manual(values = vivid_colors) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12))
}

  # --------------------------
  # monochrome premium
  # --------------------------
  if (style == "mono") {
    g <- ggplot2::ggplot(data_long, ggplot2::aes(x = group, y = value, fill = group)) +
      ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.7, linewidth = 0.7, color = "black") +
      ggplot2::geom_jitter(width = 0.1, alpha = 0.2, color = "grey25") +
      ggplot2::labs(title = title, subtitle = subtitle_text, x = "", y = ylab) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::scale_fill_manual(values = mono_colors) +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12))
}

  # --------------------------
  # Half eye
  # --------------------------
  if (style == "halfeye") {
    if (!requireNamespace("ggdist", quietly = TRUE)) {
      stop("For 'halfeye' style, please install the 'ggdist' package")
    }
    g <- ggplot2::ggplot(data_long, ggplot2::aes(x = group, y = value, fill = group)) +
      ggdist::stat_halfeye(alpha = .6, trim = FALSE, adjust = 0.6, width = 0.6,
                           .width = c(0.5, 0.8, 0.95),
                           justification = -0.2,
                           slab_color = "gray20",
                           interval_color = "gray20") +
      ggplot2::geom_point(position = ggplot2::position_nudge(x = 0.15),
                          size = 1.1, alpha = 0.2, color = "grey25") +
      ggdist::stat_pointinterval(position = ggplot2::position_nudge(x = 0.2),
                                 point_color = "black",
                                 interval_color = "black",
                                 .width = 0.95) +
      ggplot2::labs(title = title, subtitle = subtitle_text, x = "", y = ylab) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12))
}

  # --------------------------
  # Annotation and print
  # --------------------------
  g <- g + .add_significance(sig_pairs, y_range)

  print(g)

  # --------------------------
  # Output
  # --------------------------
  obj <- (list(
    type = "ANOVA",
    p_anova = p_anova,
    omega_sq = omega_sq,
    omega_ci = c(omega_ci_low, omega_ci_high),
    normal = normal,
    p_normal = p_normal,
    homogeneous = homogeneous,
    p_levene = p_levene,
    means_sd = means_sd,
    tukey = tukey_df,
    significant_pairs = significant_pairs,
    model = model
    )
  )

  # --------------------------
  # Return
  # --------------------------
  if (verbose) {
    .print_header("One-way ANOVA")

    .print_block("Statistics", function() {

      cat("F statistic = ",
          round(anova_tab[1, "F value"], 3),
          " | df = ",
          anova_tab[1, "Df"], ", ",
          anova_tab[nrow(anova_tab), "Df"],
          " | p = ",
          .format_pval(p_anova),
          "\n", sep = "")

      cat("Omega^2 = ",
          round(omega_sq, 3),
          " [",
          round(omega_ci_low, 3), ", ",
          round(omega_ci_high, 3),
          "]\n", sep = "")

    })

    # --- Post-hoc output ---

    .print_header("Post-hoc: Tukey HSD")

    .print_block("Significant comparisons", function() {

      if (nrow(significant_pairs) == 0) {

        cat("No significant comparisons (p < 0.05)\n")

      } else {

        for (i in seq_len(nrow(significant_pairs))) {

          r <- significant_pairs[i, ]

          comps <- unlist(strsplit(r$Comparison, "-"))

          g1 <- trimws(comps[1])
          g2 <- trimws(comps[2])

          cat(g1, " vs ", g2, "\n", sep = "")

          cat(
            "Mean difference = ",
            round(r$diff, 3),
            " [",
            round(r$lwr, 3), ", ",
            round(r$upr, 3),
            "]\n",
            sep = ""
          )

          cat(
            "p (adj) = ",
            .format_pval(r$p_adj),
            "\n\n",
            sep = ""
          )
        }
      }
    })
  }

  return(invisible(list(result = obj)))
}

