#' Paired t-test with advanced visualizations
#'
#' Performs a paired t-test between two numeric vectors (e.g., before vs after)
#' or between two numeric columns of a data frame.
#' Includes four visualization styles (boxplot, violin, mono, and half-eye).
#'
#' @param ... Two numeric vectors of equal length, or
#'   a data frame with exactly two numeric columns.
#' @param title Plot title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param style Plot style:
#'   \itemize{
#'     \item \code{1} Premium boxplot
#'     \item \code{2} Violin + minimal boxplot
#'     \item \code{3} mono
#'     \item \code{4} Half-eye (ggdist)
#'   }
#' @param connect Logical. If TRUE, connects paired observations.
#' @param help If TRUE, displays detailed help.
#' @param verbose If TRUE, prints progress messages.
#'
#' @return An invisible list containing:
#' \describe{
#'   \item{summary_table}{Group means and standard deviations}
#'   \item{test_result}{t-test result object (stats::t.test)}
#'   \item{data}{Data frame used for plotting}
#'   \item{plot}{ggplot2 object}
#' }
#'
#' @export
#'
#' @examples
#' before <- c(13, 12, 15, 14)
#' after  <- c(9, 11, 10, 10)
#' test.tpaired(before, after)
#'

test.tpaired <- function(
    ...,
    title = "Paired t-test",
    xlab = "",
    ylab = "Value",
    style = c("boxplot", "violin", "mono", "halfeye"),
    connect = TRUE,
    help = FALSE,
    verbose = TRUE
) {

  args <- list(...)
  style <- match.arg(style)

  # ------------------------------
  # Help
  # ------------------------------
  if (help) {
    message(
      "Function test.tpaired()

Accepted input:
 - Two numeric vectors of equal length
 - Or a data frame with exactly two numeric columns

# Example
before <- c(13, 12, 15, 14)
after  <- c(9, 11, 10, 10)
test.tpaired(before, after)

Returns:
 A list with summary_table, t.test result, plotting data and plot"
    )
    return(invisible(NULL))
  }

  # ------------------------------
  # Flexible input
  # ------------------------------
  if (length(args) == 1 && is.data.frame(args[[1]]) && ncol(args[[1]]) == 2) {
    df <- args[[1]]
    if (!all(sapply(df, is.numeric)))
      stop("The data frame must contain exactly two numeric columns.")
    x <- df[[1]]
    y <- df[[2]]
    names <- colnames(df)
  } else {
    if (length(args) != 2)
      stop("Provide two numeric vectors or a data frame with two columns.")

    x <- args[[1]]
    y <- args[[2]]

    if (!is.numeric(x) || !is.numeric(y))
      stop("Both vectors must be numeric.")

    if (length(x) != length(y))
      stop("Vectors must have the same length (paired test).")

    names <- as.character(match.call(expand.dots = FALSE)$...)[1:2]
  }

  # Remove missing pairs
  ok <- complete.cases(x, y)
  x <- x[ok]
  y <- y[ok]

  if (length(x) < 3) stop("At least 3 valid paired observations are required.")

  # ------------------------------
  # Statistical test_result
  # ------------------------------
  test_result <- stats::t.test(x, y, paired = TRUE)

  # Differences
  d <- x - y

  # Effect size: Cohen's dz
  dz <- mean(d) / sd(d)

  # Bootstrap CI
  res_boot <- .boot_one_sample(
    d,
    stat_fun = function(z)
      mean(z) / sd(z)
  )

  ci_low  <- res_boot$ci_low
  ci_high <- res_boot$ci_high

  # Labels
  p_value <- test_result$p.value
  p_numeric <- signif(p_value, 3)
  p_label   <- .format_pval(p_value)

  summary_table <- data.frame(
    Group = names,
    Mean  = round(c(mean(x), mean(y)), 2),
    SD    = round(c(sd(x), sd(y)), 2)
  )

  attr(summary_table, "effect_size") <- list(
    dz = dz,
    CI = c(
      low = ci_low,
      high = ci_high
    )
  )

  # ------------------------------
  # Final data frame
  # ------------------------------
  data <- data.frame(
    id = seq_along(x),
    group = factor(
      rep(names, each = length(x)),
      levels = names
    ),
    value = c(x, y)
  )

  # ============================
  # Labels and colors
  # ============================
  signif_label <- if (p_value < 0.001) {
    "***"
  } else if (p_value < 0.01) {
    "**"
  } else if (p_value < 0.05) {
    "*"
  } else {
    ""
  }

  mean_diff <- mean(d)

  y_pos <- max(data$value, na.rm = TRUE) +
    0.1 * diff(range(data$value, na.rm = TRUE))

  # --- Optional paired-line layer ---
  lines <- function() {
    if (!connect) return(NULL)
    ggplot2::geom_line(
      data = data,
      ggplot2::aes(x = group, y = value, group = id),
      color = "gray40",
      linewidth = 0.5,
      alpha = 0.4
    )
  }

  # --- Colors ---
  vivid_colours <- scales::hue_pal()(length(unique(data$group)))
  mono_colors <- c("grey75", "grey25")

  # ============================
  # STYLE 1 (Boxplot + jitter)
  # ============================
  if (style == "boxplot") {
    g <- ggplot2::ggplot(data, ggplot2::aes(group, value, fill = group)) +
      ggplot2::geom_boxplot(alpha = .7, outlier.shape = NA, width = 0.7, linewidth = 0.7) +
      lines() +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = .1),
        alpha = .2,
        size = 1.8,
        color = "grey25"
      ) +
      ggplot2::annotate(
        "text", x = mean(1:2), y = y_pos,
        label = signif_label, size = 6,
        col = "grey25"
      ) +
      ggplot2::scale_fill_manual(values = vivid_colours) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0.05, 0.15))
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = title,
        subtitle = .build_subtitle(mean_diff, p_value, "mean diff"),
        x = xlab,
        y = ylab
      ) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12)
      )
  }

  # ============================
  # STYLE 2 (Violin)
  # ============================
  if (style == "violin") {
    g <- ggplot2::ggplot(data, ggplot2::aes(group, value, fill = group)) +
      ggplot2::geom_violin(trim = FALSE, alpha = .6, color = NA, adjust = .6) +
      ggplot2::geom_boxplot(width = .18, outlier.shape = NA, color = "gray20") +
      lines() +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = .1),
        alpha = .2, size = 1.8, color = "gray25"
      ) +
      ggplot2::annotate(
        "text", x = mean(1:2), y = y_pos,
        label = signif_label, size = 6,
        col = "grey25"
      ) +
      ggplot2::scale_fill_manual(values = vivid_colours) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0.05, 0.15))
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = title,
        subtitle = .build_subtitle(mean_diff, p_value, "mean diff"),
        x = xlab,
        y = ylab
      ) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12)
      )
  }

  # ============================
  # STYLE 3 (monochrome premium)
  # ============================
  if (style == "mono") {
    g <- ggplot2::ggplot(data, ggplot2::aes(x = group, y = value, fill = group)) +
      ggplot2::geom_boxplot(alpha = 0.75, outlier.shape = NA, width = 0.7, linewidth = 0.7) +
      lines() +
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
        subtitle = .build_subtitle(mean_diff, p_value, "mean diff"),
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
  # STYLE 4 (ggdist half-eye + median)
  # ============================
  if (style == "halfeye") {
    if (!requireNamespace("ggdist", quietly = TRUE))
      stop("Package 'ggdist' is required for style = halfeye'.")

    g <- ggplot2::ggplot(data, ggplot2::aes(group, value, fill = group)) +
      ggdist::stat_halfeye(alpha = .6, adjust = 0.6, trim = FALSE, .width = 0.95) +
      lines() +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = .1),
        alpha = .4
      ) +
      ggplot2::annotate(
        "text", x = mean(1:2), y = y_pos,
        label = signif_label, size = 6,
        col = "grey25"
      ) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0.05, 0.15))
      ) +
      ggplot2::scale_fill_manual(values = vivid_colours) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = title,
        subtitle = .build_subtitle(mean_diff, p_value, "mean diff"),
        x = xlab,
        y = ylab
      ) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12)
      )
  }

  print(g)

  # ------------------------------
  # Output
  # ------------------------------
  obj <- list(
    summary_table = summary_table,
    test = test_result,
    data = data,
    plot = g
  )

  # ------------------------------
  # Return
  # ------------------------------
  if (verbose) {

    .print_header("Paired t-test")

    .print_block("Summary", function() {
      print(summary_table, row.names = FALSE)
    })

    .print_block("Statistics", function() {

      cat(
        "t statistic = ", round(test_result$statistic, 3),
        " | df = ", round(test_result$parameter, 1),
        " | p = ", p_label,
        "\n",
        sep = ""
      )

      cat(
        "Effect size (Cohen's dz) = ",
        round(dz, 2),
        " [",
        round(ci_low, 2), ", ",
        round(ci_high, 2),
        "]\n",
        sep = ""
      )
    })
  }

  return(invisible(list(result = obj)))
}
