#' Correlation Analysis with Automatic Method Selection
#'
#' Performs correlation analysis between two numeric variables using Pearson,
#' Spearman, or Kendall methods. When \code{method = "auto"}, the function
#' automatically selects the most appropriate method based on normality,
#' presence of ties, and proportion of outliers.
#'
#' In addition to hypothesis testing, the function provides diagnostic
#' information, effect size interpretation, and publication-ready
#' visualizations.
#'
#' @details
#' Method selection in automatic mode follows these rules:
#' \itemize{
#'   \item If more than 5\% of observations are classified as outliers,
#'         Kendall's tau is used.
#'   \item If both variables are normally distributed and no ties are present,
#'         Pearson's correlation is used.
#'   \item Otherwise, Spearman's rank correlation is applied.
#' }
#'
#' Normality is assessed using Shapiro-Wilk (n <= 50), Anderson-Darling
#' (50 < n <= 300), or Kolmogorov-Smirnov (n > 300) tests.
#'
#' @param x Numeric vector or data frame with exactly two numeric columns.
#' @param y Numeric vector (optional if \code{x} is a data frame).
#' @param method Correlation method: \code{"auto"} (default), \code{"pearson"},
#'   \code{"spearman"}, or \code{"kendall"}.
#' @param main Plot title. If \code{NULL}, an automatic title is generated.
#'   If \code{FALSE}, no title is displayed.
#' @param xlab Label for the x-axis. If \code{NULL}, a label is inferred
#'   from the input. If \code{FALSE}, no label is shown.
#' @param ylab Label for the y-axis. If \code{NULL}, a label is inferred
#'   from the input. If \code{FALSE}, no label is shown.
#' @param style Plot style: \code{"simple"} (default), \code{"inference"},
#'   \code{"structure"}, \code{"density"}, or \code{"distribution"}.
#' @param plot_normality Logical. If \code{TRUE}, displays QQ-plots for
#'   normality assessment.
#' @param help Logical. If \code{TRUE}, displays a detailed usage guide.
#' @param verbose Logical. If \code{TRUE}, prints information about method
#'   selection and diagnostics.
#'
#' @return
#' An object of class \code{"test.correlation"} containing:
#' \itemize{
#'   \item \code{call}: Matched function call.
#'   \item \code{data}: Input data (if \code{n <= 5000}).
#'   \item \code{n}: Sample size.
#'   \item \code{method}: Selected correlation method.
#'   \item \code{estimate}: Correlation coefficient.
#'   \item \code{effect}: Effect size information (r, rho, or tau, and r² when applicable).
#'   \item \code{conf.int}: Confidence interval.
#'   \item \code{p.value}: P-value of the test.
#'   \item \code{interpretation}: Qualitative interpretation of effect size.
#'   \item \code{diagnostics}: Normality, ties, and outlier diagnostics.
#'   \item \code{decision}: Automatic selection rationale.
#'   \item \code{htest}: Underlying \code{stats::cor.test} object.
#'   \item \code{plot}: \code{ggplot2} object.
#' }
#'
#' @seealso
#' \code{\link[stats]{cor.test}},
#' \code{\link[ggplot2]{ggplot}}
#'
#' @examples
#' # Pearson (approximately normal data)
#' set.seed(123)
#' x <- rnorm(50, sd = 0.1)
#' y <- x + rnorm(50, sd = 0.1)
#' test.correlation(x, y)
#'
#' # Spearman (non-normal data)
#' set.seed(123)
#' x <- runif(300)
#' y <- log(x + 0.1) + rnorm(300, sd = 0.5)
#' test.correlation(x, y)
#'
#' # Kendall (ties and outliers)
#' set.seed(123)
#' x <- runif(1000, 1, 100)
#' y <- sin(x) * 30 + rnorm(1000, 0, 10)
#' x[sample(1:500, 50)] <- 50
#' y[sample(1:500, 50)] <- 0
#' x_out <- runif(100, 10, 20)
#' y_out <- runif(100, 80, 120)
#' x <- c(x, x_out)
#' y <- c(y, y_out)
#' test.correlation(x, y)
#'
#' @export

test.correlation <- function(x,
                             y = NULL,
                             method = "auto",
                             main = NULL,
                             xlab = NULL,
                             ylab = NULL,
                             style = c("simple", "inference", "structure", "density", "distribution"),
                             plot_normality = FALSE,
                             help = FALSE,
                             verbose = TRUE
) {

  style <- match.arg(style)
  method <- match.arg(
    method,
    c("auto","pearson","spearman","kendall")
  )

  if (help || missing(x)) {
    if (verbose) message("
test.correlation() function

Description:
  Tests correlation between two numeric variables, automatically choosing
  between Pearson, Spearman and Kendall.
  Can display graphical normality diagnostics (QQ-plots).

Usage:
  test.correlation(x, y, method = 'auto', plot_normality = TRUE)

Arguments:
  x, y              Numeric vectors of equal length or a data frame with two numeric columns
  method            'auto', 'pearson', 'spearman' or 'kendall'
  verbose           If TRUE, shows which method was used and why
  help              If TRUE, displays this help message with examples
  plot_normality    If TRUE, displays QQ-plots

Examples:

# Pearson
  set.seed(123)
  x <- rnorm(50, sd = 0.1)
  y <- x + rnorm(50, sd = 0.1)
  test.correlation(x, y)

# Spearman
  set.seed(123)
  x <- runif(300)
  y <- log(x + 0.1) + rnorm(300, sd = 0.5)
  test.correlation(x, y)

# Kendall
  set.seed(123)
  x <- runif(1000, 1, 100)
  y <- sin(x) * 30 + rnorm(1000, 0, 10)
  x[sample(1:500, 50)] <- 50
  y[sample(1:500, 50)] <- 0
  x_out <- runif(100, 10, 20)
  y_out <- runif(100, 80, 120)
  x <- c(x, x_out)
  y <- c(y, y_out)
  test.correlation(x, y)
")
    return(invisible(NULL))
  }

  # ---------------------------
  # required packages
  # ---------------------------
  required_packages <- c("ggplot2", "ggExtra")
  lapply(required_packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is not installed.", pkg), call. = FALSE)
    }
  })

  # ---------------------------
  # Prepare data
  # ---------------------------
  if (is.data.frame(x)) {
    if (ncol(x) != 2) stop("The data frame must contain exactly two numeric columns.")
    if (!all(sapply(x, is.numeric))) stop("Both data frame columns must be numeric.")
    name_x <- colnames(x)[1]
    name_y <- colnames(x)[2]
    data_df <- data.frame(x = x[[1]], y = x[[2]])
  } else {
    if (is.null(y)) stop("Provide two numeric vectors or a data frame with two columns.")
    if (!is.numeric(x) || !is.numeric(y)) stop("Both vectors must be numeric.")
    if (length(x) != length(y)) stop("Vectors must have the same length.")
    name_x <- deparse(substitute(x))
    name_y <- deparse(substitute(y))
    data_df <- data.frame(x = x, y = y)
  }

  clean_label <- function(label) {
    if (is.null(label) || identical(label, FALSE)) return(label)
    label <- gsub(".*\\$", "", label)
    label <- gsub("_", " ", label)
    tools::toTitleCase(label)
  }

  name_x <- clean_label(name_x)
  name_y <- clean_label(name_y)

  # ---------------------------
  # Check and diagnostics
  # ---------------------------
  has_ties <- anyDuplicated(data_df$x) > 0 || anyDuplicated(data_df$y) > 0

  apply_normality_test <- function(v) {
    n <- length(v)

    if (n <= 50) {
      p <- stats::shapiro.test(v)$p.value
      method <- "Shapiro-Wilk"

    } else if (n <= 300) {
      if (!requireNamespace("nortest", quietly = TRUE)) {
        stop("Please install the 'nortest' package for Anderson-Darling.")
      }
      p <- nortest::ad.test(v)$p.value
      method <- "Anderson-Darling"

    } else {
      ks_res <- stats::ks.test(v, "pnorm", mean = mean(v), sd = sd(v))
      p <- ks_res$p.value
      method <- "Kolmogorov-Smirnov"
    }

    list(p = p, method = method)
  }

  norm_x <- apply_normality_test(data_df$x)
  norm_y <- apply_normality_test(data_df$y)

  normal_x <- norm_x$p > 0.05
  normal_y <- norm_y$p > 0.05

  if (plot_normality) {
    oldpar <- graphics::par(mfrow = c(1, 2))
    on.exit(graphics::par(oldpar), add = TRUE)
    stats::qqnorm(data_df$x, main = paste("QQ-Plot of", name_x))
    stats::qqline(data_df$x, col = "red")
    stats::qqnorm(data_df$y, main = paste("QQ-Plot of", name_y))
    stats::qqline(data_df$y, col = "red")
  }

  detect_outliers <- function(v) {
    q1 <- stats::quantile(v, 0.25)
    q3 <- stats::quantile(v, 0.75)
    iqr <- q3 - q1
    which(v < (q1 - 1.5 * iqr) | v > (q3 + 1.5 * iqr))
  }

  outlier_fraction <- max(
    length(detect_outliers(data_df$x)) / nrow(data_df),
    length(detect_outliers(data_df$y)) / nrow(data_df)
  )

  method_used <- if (method == "auto") {
    if (outlier_fraction > 0.05) {
      "kendall"
    } else if (normal_x && normal_y && !has_ties) {
      "pearson"
    } else {
      "spearman"
    }
  } else {
    method
  }

  if (verbose && method == "auto") {
    message(sprintf("Selected method: %s", method_used))

    if (!normal_x || !normal_y) message("- Data are not normally distributed")
    if (has_ties) message("- Ties detected in the data")
    if (outlier_fraction > 0.05)
      message(sprintf("- Presence of outliers detected: %.1f%%",
                      outlier_fraction * 100))
  }

  # ---------------------------
  # Statistical test
  # ---------------------------
  test <- stats::cor.test(data_df$x, data_df$y, method = method_used)
  coef_value <- round(unname(test$estimate), 3)
  p_val <- test$p.value

  interpretation <- cut(
    abs(coef_value),
    breaks = c(-Inf, 0.3, 0.5, 0.7, 0.9, Inf),
    labels = c("very weak or none", "weak", "moderate", "strong", "very strong"),
    right = FALSE
  )

  # Choose smoothing method for trend line
  smooth_method <- if (tolower(method_used) == "pearson") "lm" else "loess"

  auto_title <- paste(tools::toTitleCase(method_used), "Correlation")

  # Naming title and axis
  plot_title <- if (is.null(main)) {
    auto_title
  } else if (identical(main, FALSE)) {
    NULL
  } else {
    main
  }

  plot_xlab <- if (is.null(xlab)) {
    name_x
  } else if (identical(xlab, FALSE)) {
    NULL
  } else {
    xlab
  }

  plot_ylab <- if (is.null(ylab)) {
    name_y
  } else if (identical(ylab, FALSE)) {
    NULL
  } else {
    ylab
  }

  # --- Subtitle text ---
  subtitle_text <- .make_subtitle_correlation(
    method_used,
    coef_value,
    nrow(data_df),
    p_val
  )

  # ---------------------------
  # Plot styles
  # ---------------------------
  if (style == "simple") {
    g <- ggplot2::ggplot(data_df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.40, size = 2.5, col = "grey40") +
      ggplot2::geom_smooth(method = smooth_method, se = FALSE, linetype = "dashed") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = plot_title,
        subtitle = subtitle_text,
        x = plot_xlab,
        y = plot_ylab
      )

  } else if (style == "inference") {
    line_col <- "#2F5D8A"
    ci_fill  <- "#2F5D8A"
    point_col <- "grey40"

    g <- ggplot2::ggplot(data_df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.40, size = 2.3, color = point_col) +
      ggplot2::geom_smooth(
        method = "lm",
        se = TRUE,
        level = 0.95,
        color = line_col,
        fill = ci_fill,
        alpha = 0.25,
        linewidth = 1.1
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = plot_title,
        subtitle = subtitle_text,
        x = plot_xlab,
        y = plot_ylab
      )

  } else if (style == "structure") {
    g <- ggplot2::ggplot(data_df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.25, size = 2) +
      ggplot2::geom_density_2d(color = "grey40", alpha = 0.7) +
      ggplot2::geom_smooth(method = smooth_method, se = FALSE) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = plot_title,
        subtitle = subtitle_text,
        x = plot_xlab,
        y = plot_ylab
      )

  } else if (style == "density") {
    g <- ggplot2::ggplot(data_df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_hex() +
      ggplot2::scale_fill_gradientn(
        colours = c("#0000FF", "#00FF00", "#FFFF00", "#FF0000"),
        trans = "sqrt",
        name = "Density"
      ) +
      ggplot2::geom_smooth(method = smooth_method, se = FALSE, color = "white") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = plot_title,
        subtitle = subtitle_text,
        x = plot_xlab,
        y = plot_ylab
      )

  } else if (style == "distribution") {

    if (!requireNamespace("ggExtra", quietly = TRUE)) {
      warning("For style = 'distribution' install the 'ggExtra' package. Falling back to style 'simple'.")
      g <- ggplot2::ggplot(data_df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(alpha = 0.25, size = 2.5) +
        ggplot2::geom_smooth(method = smooth_method, se = FALSE, linetype = "dashed") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(
          title = plot_title,
          subtitle = subtitle_text,
          x = plot_xlab,
          y = plot_ylab
        )

    } else {
      base_plot <- ggplot2::ggplot(data_df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(alpha = 0.40, size = 2, col = "grey40") +
        ggplot2::geom_smooth(method = smooth_method, se = FALSE) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(
          title = plot_title,
          subtitle = subtitle_text,
          x = plot_xlab,
          y = plot_ylab
        )
      g <- ggExtra::ggMarginal(base_plot, type = "density", size = 5, fill = "#c8d3e0")
    }

  } else {
    stop("Invalid style: use 'simple', 'inference', 'structure', 'density' or 'distribution'")
  }

  # ---------------------------
  # Return
  # ---------------------------
  out <- list(
    call = match.call(),
    data = if (nrow(data_df) <= 5000) data_df else NULL,
    n = nrow(data_df),
    method = method_used,
    estimate = coef_value,
    conf.int = test$conf.int,
    p.value = test$p.value,
    interpretation = interpretation,
    effect = list(
      statistic = switch(
        method_used,
        pearson  = "r",
        spearman = "rho",
        kendall  = "tau"
      ),
      value = coef_value,
      r2 = if (method_used == "pearson") coef_value^2 else NULL
    ),
    diagnostics = list(
      normal_x = normal_x,
      normal_y = normal_y,
      ties = has_ties,
      outlier_fraction = outlier_fraction
    ),
    decision = list(
      auto = (method == "auto"),
      chosen = method_used,
      reasons = Filter(
        Negate(is.null),
        list(
          if (!normal_x || !normal_y) "non-normality",
          if (has_ties) "ties",
          if (outlier_fraction > 0.05) "outliers"
        )
      )
    ),
    htest = test,
    plot = g,
    verbose = verbose
  )

  class(out) <- "test.correlation"
  out
}

# ---------------------------
# S3 Output
# ---------------------------
#' @method print test.correlation
#' @param ... Additional arguments passed to other print methods (currently ignored)
#' @export
#' @rdname test.correlation

print.test.correlation <- function(x, ...) {

  if (!isTRUE(x$verbose)) return(invisible(x))

  .print_header("Correlation analysis")

  .print_block("Method", function() {
    cat("Method:", x$method, "\n")
    cat(
      "Correlation (", x$effect$statistic, "): ",
      round(x$effect$value, 3),
      "\n",
      sep = ""
    )
    cat("p-value:", .format_pval(x$p.value), "\n")
  })

  .print_block("Diagnostics", function() {
    cat("Normal X:", x$diagnostics$normal_x, "\n")
    cat("Normal Y:", x$diagnostics$normal_y, "\n")
    cat("Ties:", x$diagnostics$ties, "\n")
    cat("N:", x$n, "\n")
    cat("Outliers:", round(x$diagnostics$outlier_fraction * 100, 1), "%\n")
  })

  if (!is.null(x$plot)) print(x$plot)

  invisible(x)
}
