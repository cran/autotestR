#' Test for interaction between a continuous predictor and a grouping variable
#'
#' Fits a linear model of the form \code{y ~ x * by} to evaluate whether the
#' association between a continuous predictor and an outcome differs across
#' groups. Optionally produces a publication-ready visualization of
#' group-specific regression lines.
#'
#' @param y Numeric vector representing the continuous predictor.
#' @param x Numeric vector representing the continuous outcome.
#' @param by Grouping variable defining the interaction. Must be coercible
#'   to a factor with at least two levels.
#' @param title Optional title title for the plot.
#' @param xlab Optional x-axis label.
#' @param ylab Optional y-axis label.
#' @param plot Logical. Should a plot be generated?
#' @param style Plot style. One of \code{"clean"}, \code{"CI"} or \code{"facet"}.
#' @param conf.level Confidence level for the interaction interval
#'   (default: 0.95).
#' @param help Logical. If TRUE, shows a detailed explanation of the function.
#'        Default: FALSE.
#' @param colors Character. Optional point and line colors.
#' @param verbose Logical. If TRUE, prints detailed messages. Default: TRUE.
#'
#' @return An object of class \code{"test_interaction"} containing:
#' \itemize{
#'   \item \code{model}: the fitted linear model,
#'   \item \code{interaction}: estimated interaction effects with confidence intervals,
#'   \item \code{plot}: a \code{ggplot} object (if \code{plot = TRUE}).
#' }
#'
#' @details
#' The interaction coefficient (\eqn{\beta}) represents the difference in
#' regression slopes between groups, conditional on the reference level
#' of \code{by}. The sign and magnitude of this coefficient depend on the
#' chosen reference group.
#'
#' Confidence intervals are emphasized as the primary inferential quantity.
#'
#' @examples
#'
#' # Simple example: different trends between groups
#'
#' set.seed(125)
#'
#' n <- 60
#' df_bio <- data.frame(
#'   response = rnorm(n, mean = 10, sd = 2),
#'   group    = rep(c('Control', 'Treatment'), each = n / 2)
#' )
#'
#' df_bio$marker <- with(
#'   df_bio,
#'   ifelse(
#'     group == 'Control',
#'     2 + 1.3 * response + rnorm(n, 0, 2),
#'     2 + 1.2 * response + rnorm(n, 0, 1)
#'   )
#' )
#'
#' test_interaction(
#'   x  = df_bio$marker,
#'   y  = df_bio$response,
#'   by = df_bio$group)
#'
#' @export

test_interaction <- function(y,
                             x,
                             by,
                             title = NULL,
                             xlab = NULL,
                             ylab = NULL,
                             plot = TRUE,
                             style = c("clean", "CI", "facet"),
                             conf.level = 0.95,
                             colors = NULL,
                             verbose = TRUE,
                             help = FALSE
) {

style <- match.arg(style)

# =============================================================================
# 1. Quick help block
# =============================================================================

if (help) {
  if (verbose) {
    message("

Function test_interaction()

Description:
  Tests whether the association between a continuous predictor and an outcome
  differs across groups using a linear model (y ~ x * by).

Interpretation:
  The interaction term represents the difference in slopes between groups.

Example:

set.seed(125)

n <- 60
df_bio <- data.frame(
response = rnorm(n, mean = 10, sd = 2),
group    = rep(c('Control', 'Treatment'), each = n / 2)
)

df_bio$marker <- with(
df_bio,
ifelse(
group == 'Control',
2 + 1.3 * response + rnorm(n, 0, 2),
2 + 1.2 * response + rnorm(n, 0, 1)
)
)

test_interaction(
x  = df_bio$marker,
y  = df_bio$response,
by = df_bio$group)

")
  }
  return(invisible(NULL))
}

# =============================================================================
# 2. Input validation
# =============================================================================

if (!is.numeric(x) || !is.numeric(y)) {
  stop("x and y must be numeric vectors.", call. = FALSE)
}

if (length(x) != length(y) || length(y) != length(by)) {
  stop("x, y and by must have the same length.", call. = FALSE)
}

df <- data.frame(
  x  = x,
  y  = y,
  by = factor(by)
)

df <- df[complete.cases(df), ]

if (nlevels(df$by) < 2) {
  stop("'by' must have at least two levels.", call. = FALSE)
}

# ===============================================
# 2.1. Variable labels (for plots)
# ===============================================

x_var  <- deparse(substitute(x))
y_var  <- deparse(substitute(y))
by_var <- deparse(substitute(by))

clean_label <- function(label) {
  label <- gsub(".*\\$", "", label)
  label <- gsub("_", " ", label)
  tools::toTitleCase(label)
}

x_var  <- clean_label(x_var)
y_var  <- clean_label(y_var)
by_var <- clean_label(by_var)

# =============================================================================
# 3. Model
# =============================================================================

fit <- lm(y ~ x * by, data = df)

coef_tbl <- summary(fit)$coefficients
ci_tbl   <- confint(fit, level = conf.level)

interaction_terms <- grep("^x:|:x$", rownames(coef_tbl), value = TRUE)

interaction_info <- lapply(interaction_terms, function(term) {

  term_pretty <- .pretty_interaction_term(term, by_var)

  list(
    term = term_pretty,
    raw_term = term,
    beta = coef_tbl[term, "Estimate"],
    p    = coef_tbl[term, "Pr(>|t|)"],
    ci   = ci_tbl[term, ]
  )
})

summary_table <- aggregate(
  y ~ by,
  data = df,
  function(z) c(
    Mean = mean(z, na.rm = TRUE),
    SD   = sd(z, na.rm = TRUE),
    N    = sum(!is.na(z))
  )
)

summary_table <- do.call(data.frame, summary_table)

main_interaction <- interaction_info[[1]]

# =============================================================================
# 4. Plot
# =============================================================================

g <- NULL

if (plot) {

  base_plot <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = x, y = y, color = by, fill = by)
  ) +
    ggplot2::theme_minimal(base_size = 12)

  # ===============================================
  # 4.1. Clean
  # ===============================================

  if (style == "clean") {
    g <- base_plot +
      ggplot2::geom_point(alpha = 0.6, size = 2) +
      ggplot2::geom_smooth(method = "lm", se = FALSE)

  # ===============================================
  # 4.2. CI
  # ===============================================

  } else if (style == "CI") {
    g <- base_plot +
      ggplot2::geom_point(alpha = 0.5, size = 1.8) +
      ggplot2::geom_smooth(
        method = "lm",
        se = TRUE,
        level = conf.level,
        alpha = 0.25
      )

  # ===============================================
  # 4.3. Facet
  # ===============================================

  } else if (style == "facet") {
    g <- base_plot +
      ggplot2::geom_point(alpha = 0.6, size = 2) +
      ggplot2::geom_smooth(method = "lm", se = FALSE) +
      ggplot2::facet_wrap(~ by)
  }

  # ===============================================
  # 4.4. Custom colors
  # ===============================================

  if (!is.null(colors)) {
    g <- g +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::scale_fill_manual(values = colors)
  }

  # ===============================================
  # 4.4. Additional labs
  # ===============================================

  if (style %in% c("clean", "CI")) {
    g <- g +
      ggplot2::labs(
        title = if (is.null(title)) paste(y_var, "vs", x_var) else title,
        subtitle = .build_subtitle_interaction(
          main_interaction$beta,
          main_interaction$p
        ),
        x = if (is.null(xlab)) x_var else xlab,
        y = if (is.null(ylab)) y_var else ylab,
        color = by_var,
        fill  = by_var
      )
  } else {
    g <- g +
      ggplot2::labs(
        title = if (is.null(title)) paste(y_var, "vs", x_var) else title,
        subtitle = .build_subtitle_interaction(
          main_interaction$beta,
          main_interaction$p
        ),
        x = if (is.null(xlab)) x_var else xlab,
        y = if (is.null(ylab)) y_var else ylab
      )
  }
}

# =============================================================================
# 5. Return
# =============================================================================

obj <- list(
  call = match.call(),
  model = fit,
  interaction = interaction_info,
  summary = summary_table,
  conf.level = conf.level,
  plot = g,
  method = "Linear model interaction",
  verbose = verbose
)

class(obj) <- "test_interaction"

obj

}

# =============================================================================
# 6. S3 Output
# =============================================================================
#' @method print test_interaction
#' @param ... Additional arguments passed to other print methods (currently ignored)
#' @export
#' @rdname test_interaction

print.test_interaction <- function(x, ...) {

if (!isTRUE(x$verbose)) {
  return(invisible(x))
}

.print_header(x$method)

.print_block("Model", function() {

  cat("Formula:", deparse(formula(x$model)), "\n")
  cat("Observations:", nobs(x$model), "\n")

})

.print_block("Summary by group", function() {
  print(x$summary, row.names = FALSE)
})

.print_block("Statistics", function() {

  for (info in x$interaction) {

    cat(
      info$term, "\n", "\n")

    cat(
      "Beta = ", round(info$beta, 3), "\n")

    cat(
      "CI = [", round(info$ci[1], 3), ",", round(info$ci[2], 3),"]", "\n")

    cat(
      "p = ", .format_pval(info$p), "\n", sep = "")
    }
})

if (!is.null(x$plot)) {
  print(x$plot)
}

return(invisible(x))

}
