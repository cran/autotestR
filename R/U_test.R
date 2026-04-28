#' Mann-Whitney U Test
#'
#' Performs the Mann-Whitney (Wilcoxon rank-sum) test for comparing two independent groups,
#' with statistical summary and graphical visualization.
#'
#' @param ... Two numeric vectors or a data.frame with two numeric columns.
#' @param title Plot title. Default: "Mann-Whitney Test".
#' @param xlab Label for x-axis. Default: "Group".
#' @param ylab Label for y-axis. Default: "Value".
#' @param style Plot aesthetic style.
#' @param help Logical. If TRUE, prints a detailed explanation. Default: FALSE.
#' @param verbose Logical. If TRUE, prints detailed messages. Default: TRUE.
#' @importFrom stats median
#'
#' @return Invisible list with:
#' \describe{
#'   \item{summary}{Group-wise statistical summary}
#'   \item{test}{Test result (htest object)}
#'   \item{plot}{ggplot2 visualization object}
#' }
#' @export
#'
#' @examples
#' x <- c(1, 3, 5, 6)
#' y <- c(7, 8, 9, 12)
#' data <- data.frame(groupA = x, groupB = y)
#' test.u(data)

test.u <- function(...,
                   title = "Mann-Whitney Test",
                   xlab = "Group",
                   ylab = "Value",
                   style = c("boxplot", "violin", "mono", "halfeye"),
                   help = FALSE,
                   verbose = TRUE) {

  input_groups <- list(...)
  style <- match.arg(style)

  # ============================
  # Input via data.frame
  # ============================
  if (length(input_groups) == 1 && is.data.frame(input_groups[[1]])) {

    df <- input_groups[[1]]

    if (ncol(df) != 2)
      stop("The data.frame must contain exactly two numeric columns.")

    if (!all(vapply(df, is.numeric, logical(1))))
      stop("Both columns must be numeric.")

    group_names <- colnames(df)
    groups <- as.list(df)

  } else {

    if (length(input_groups) != 2)
      stop("Provide two numeric vectors or one data.frame with two columns.")

    if (!all(vapply(input_groups, is.numeric, logical(1))))
      stop("All groups must be numeric vectors.")

    call_names <- as.character(match.call(expand.dots = FALSE)$...)
    group_names <- sub("^.*\\$", "", call_names)
    groups <- input_groups
  }

  # ============================
  # Help message
  # ============================
  if (help) {

    if (verbose) {
      message("
Function: test.u()

Description:
  Performs the Mann-Whitney (Wilcoxon rank-sum) test to compare two independent groups.

When to use:
  - Non-normal or ordinal data
  - Comparison of two independent groups
  - Non-parametric alternative to the t-test

Examples:
  x <- c(1, 3, 5, 6)
  y <- c(7, 8, 9, 12)
  data <- data.frame(groupA = x, groupB = y)
  test.u(data)
")
    }

    return(invisible(NULL))
  }

  # ============================
  # Package checking
  # ============================
  required_packages <- c("ggplot2", "dplyr", "scales", "ggdist")

  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        paste0(
          "Package ", pkg,
          " is not installed. Install it with install.packages('", pkg, "')"
        )
      )
    }
  }

  # ============================
  # Long-format data
  # ============================
  values <- unlist(groups)

  group_factor <- factor(
    rep(group_names, times = vapply(groups, length, integer(1))),
    levels = group_names
  )

  data_long <- data.frame(
    value = values,
    group = group_factor
  )

  # ============================
  # Mann-Whitney test
  # ============================
  test_result <- stats::wilcox.test(
    groups[[1]],
    groups[[2]],
    exact = FALSE
  )

  p_value <- test_result$p.value
  p_label <- .format_pval(p_value)

  x_data <- groups[[1]]
  y_data <- groups[[2]]

  nx <- sum(!is.na(x_data))
  ny <- sum(!is.na(y_data))

  # ============================
  # Median difference
  # ============================
  median_diff <- median(x_data, na.rm = TRUE) - median(y_data, na.rm = TRUE)

  # ============================
  # Rank-biserial correlation
  # ============================
  U <- test_result$statistic

  r_rb <- as.numeric((2 * U) / (nx * ny) - 1)

  # ============================
  # Bootstrap CI (median diff)
  # ============================

  res_boot <- .boot_two_sample(
    x_data,
    y_data,
    stat_fun = function(a, b)
      median(a, na.rm = TRUE) - median(b, na.rm = TRUE)
  )

  ci_low  <- res_boot$ci_low
  ci_high <- res_boot$ci_high

  # ============================
  # Labels and colors
  # ============================
  p_label <- signif(p_value, 3)

  signif_label <- if (p_value < 0.001) {
    "***"
  } else if (p_value < 0.01) {
    "**"
  } else if (p_value < 0.05) {
    "*"
  } else {
    ""
  }

  y_pos <- max(values, na.rm = TRUE) +
    0.1 * diff(range(values, na.rm = TRUE))

  vivid_colors <- scales::hue_pal()(length(unique(data_long$group)))
  mono_colors <- c("grey75", "grey25")

  # ============================
  # STYLE 1 — Boxplot + jitter
  # ============================
  if (style == "boxplot") {
    g <- ggplot2::ggplot(data_long, ggplot2::aes(x = group, y = value, fill = group)) +
      ggplot2::geom_boxplot(alpha = 0.75, outlier.shape = NA, width = 0.60) +
      ggplot2::geom_jitter(width = 0.1, alpha = 0.2, color = "grey25", size = 1.8) +
      ggplot2::annotate(
        "text", x = mean(1:2), y = y_pos,
        label = signif_label, size = 6,
        col = "grey25"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::scale_fill_manual(values = vivid_colors) +
      ggplot2::labs(
        title = title,
        subtitle = .build_subtitle_u(median_diff, p_value),
        x = "",
        y = ylab
      ) +
      ggplot2::theme(
        legend.position = "none",
        plot.margin = ggplot2::margin(5.5, 5.5, 10, 5.5),
        axis.text.x = ggplot2::element_text(
          angle = 45, hjust = 1, size = 12
        )
      )
  }

  # ============================
  # STYLE 2 — Violin
  # ============================
  if (style == "violin") {
    g <- ggplot2::ggplot(data_long, ggplot2::aes(x = group, y = value, fill = group)) +
      ggplot2::geom_violin(
        trim = FALSE, alpha = .6,
        color = NA, adjust = .6
      ) +
      ggplot2::geom_boxplot(
        width = .18, outlier.shape = NA,
        color = "gray20", linewidth = .4
      ) +
      ggplot2::annotate(
        "text", x = mean(1:2), y = y_pos,
        label = signif_label, size = 6,
        col = "grey25"
      ) +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = .1),
        alpha = .2, size = 1.8, color = "gray25"
      ) +
      ggplot2::scale_fill_manual(values = vivid_colors) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = title,
        subtitle = .build_subtitle_u(median_diff, p_value),
        x = "",
        y = ylab
      ) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(
          angle = 45, hjust = 1, size = 12
        )
      )
  }

  # ============================
  # STYLE 3 — (Premium monochorme)
  # ============================
  if (style == "mono") {
    g <- ggplot2::ggplot(data_long, ggplot2::aes(x = group, y = value, fill = group)) +
      ggplot2::geom_boxplot(alpha = 0.75, outlier.shape = NA, width = 0.7, linewidth = 0.7) +
      ggplot2::geom_jitter(width = 0.1, alpha = 0.2, color = "grey25", size = 1.8) +
      ggplot2::annotate(
        "text", x = mean(1:2), y = y_pos,
        label = signif_label, size = 6,
        col = "grey25"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::scale_fill_manual(values = mono_colors) +
      ggplot2::labs(
        title = title,
        subtitle = .build_subtitle_u(median_diff, p_value),
        x = "",
        y = ylab
      ) +
      ggplot2::theme(
        legend.position = "none",
        plot.margin = ggplot2::margin(5.5, 5.5, 10, 5.5),
        axis.text.x = ggplot2::element_text(
          angle = 45, hjust = 1, size = 12
        )
      )
  }

  # ============================
  # STYLE 4 — ggdist half-eye
  # ============================
  if (style == "halfeye") {

    if (!requireNamespace("ggdist", quietly = TRUE))
      stop("Package 'ggdist' is required for style = halfeye'.")

    g <- ggplot2::ggplot(
      data_long,
      ggplot2::aes(x = group, y = value, fill = group)
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
      ggplot2::annotate(
        "text", x = mean(1:2), y = y_pos,
        label = signif_label, size = 6,
        col = "grey25"
      ) +
      ggplot2::geom_point(
        position = ggplot2::position_nudge(x = 0.15),
        size = 1.1, alpha = 0.4, color = "black"
      ) +
      ggdist::stat_pointinterval(
        position = ggplot2::position_nudge(x = 0.2),
        point_color = "black",
        interval_color = "black",
        .width = 0.95
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::scale_fill_manual(values = vivid_colors) +
      ggplot2::labs(
        title = title,
        subtitle = .build_subtitle_u(median_diff, p_value),
        x = "",
        y = ylab
      ) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(
          angle = 45, hjust = 1, size = 12
        )
      )
  }

  print(g)

  # ============================
  # Output
  # ============================
  summary_table <- data_long |>
    dplyr::group_by(group) |>
    dplyr::summarise(
      Median = round(stats::median(value, na.rm = TRUE), 2),
      Mean   = round(mean(value, na.rm = TRUE), 2),
      SD     = round(stats::sd(value, na.rm = TRUE), 2),
      .groups = "drop"
    )

  obj <- list(
    summary = summary_table,
    test    = test_result,
    plot    = g
  )

  # ============================
  # Return
  # ============================
  if (verbose) {

    .print_header("Mann-Whitney U Test")

    .print_block("Summary", function() {
      print(summary_table, row.names = FALSE)
    })

    .print_block("Statistics", function() {

      cat("W statistic:          ", round(test_result$statistic, 3), "\n", sep = "")
      cat("p-value:              ", p_label, "\n", sep = "")
      cat("Median difference:    ", round(median_diff, 2),
          " [", round(ci_low, 2), ", ", round(ci_high, 2), "]\n", sep = "")
      cat("Rank-biserial r:      ", round(r_rb, 3), "\n", sep = "")

    })
  }

  return(invisible(list(result = obj)))
}
