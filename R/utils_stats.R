# =============================================================================
# Auxiliary stats functions
# =============================================================================

# =====================================
# Format p_val
# =====================================
#' @keywords internal
.format_pval <- function(p, digits = 3, sci_cut = 1e-3) {

  if (is.na(p)) return(NA_character_)

  if (p < .Machine$double.eps) {
    return("< 2.2e-16")
  }

  if (p < sci_cut) {
    return(formatC(p, format = "e", digits = 2))
  }

  round_p <- round(p, digits)

  formatC(round_p, format = "f", digits = digits)
}

# =====================================
# Compute cramers_v
# =====================================
#' @keywords internal
.cramers_v <- function(tab) {

  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))

  n <- sum(tab)

  r <- nrow(tab)
  c <- ncol(tab)

  v <- sqrt(
    as.numeric(chi$statistic) /
      (n * min(r - 1, c - 1))
  )

  v
}

# =====================================
# Compute odds_ratio_ci
# =====================================
#' @keywords internal
.odds_ratio_ci <- function(tab, conf = 0.95) {

  if (!all(dim(tab) == c(2,2)))
    stop("Odds ratio only for 2x2 tables.")

  a <- tab[1,1]
  b <- tab[1,2]
  c <- tab[2,1]
  d <- tab[2,2]

  # Haldane correction
  if (any(tab == 0)) {
    a <- a + 0.5
    b <- b + 0.5
    c <- c + 0.5
    d <- d + 0.5
  }

  or <- (a * d) / (b * c)

  se <- sqrt(1/a + 1/b + 1/c + 1/d)

  z <- qnorm(1 - (1 - conf)/2)

  low <- exp(log(or) - z * se)
  high <- exp(log(or) + z * se)

  list(
    or = or,
    ci_low = low,
    ci_high = high
  )
}

# =====================================
# Add significance
# =====================================
#' @keywords internal
.add_significance <- function(sig_pairs, y_range, text_size = 5) {

  if (is.null(sig_pairs) || nrow(sig_pairs) == 0) {
    return(NULL)
  }

  list(

    ggplot2::geom_segment(
      data = sig_pairs,
      ggplot2::aes(
        x = x1,
        xend = x2,
        y = y,
        yend = y
      ),
      inherit.aes = FALSE
    ),

    ggplot2::geom_segment(
      data = sig_pairs,
      ggplot2::aes(
        x = x1,
        xend = x1,
        y = y,
        yend = y - 0.02 * y_range
      ),
      inherit.aes = FALSE
    ),

    ggplot2::geom_segment(
      data = sig_pairs,
      ggplot2::aes(
        x = x2,
        xend = x2,
        y = y,
        yend = y - 0.02 * y_range
      ),
      inherit.aes = FALSE
    ),

    ggplot2::geom_text(
      data = sig_pairs,
      ggplot2::aes(
        x = (x1 + x2) / 2,
        y = y + 0.02 * y_range,
        label = signif
      ),
      inherit.aes = FALSE,
      size = text_size
    )
  )
}

# =====================================
# P value format
# =====================================
#' @keywords internal
.format_p <- function(p) {

  if (is.na(p)) return("= NA")

  if (p < 0.001) {
    "< 0.001"
  } else {
    paste0("= ", formatC(p, format = "f", digits = 3))
  }
}

# =====================================
# U test subtitle
# =====================================
#' @keywords internal
.build_subtitle_u <- function(median_diff, p_value) {

  p_txt <- .format_p(p_value)

  if (p_value < 0.001) {
    paste0("med diff = ", round(median_diff, 2), " | p < 0.001")
  } else {
    paste0("med diff = ", round(median_diff, 2), " | p = ", p_txt)
  }
}

# =====================================
# Paired T test subtitle
# =====================================
#' @keywords internal
.build_subtitle <- function(effect, p_value, label = "diff") {

  p_txt <- .format_p(p_value)

  if (p_value < 0.001) {
    paste0(label, " = ", round(effect, 2), " | p < 0.001")
  } else {
    paste0(label, " = ", round(effect, 2), " | p = ", p_txt)
  }
}

# =====================================
# Kruskal Wallis subtitle
# =====================================
#' @keywords internal
.build_subtitle_kw <- function(p_value, eps) {

  p_txt <- .format_p(p_value)

  if (p_value < 0.001) {
    paste0(
      "eps^2 = ", round(eps, 2),
      " | p < 0.001"
    )
  } else {
    paste0(
      "eps^2 = ", round(eps, 2),
      " | p = ", p_txt
    )
  }
}

# =====================================
# ANOVA subtitle
# =====================================
#' @keywords internal
.make_subtitle_anova <- function(omega_sq, p_value) {

  p_label <- .format_p(p_value)

  paste0(
    "omega^2 = ", round(omega_sq, 2),
    " | p ", p_label
  )
}

# =====================================
# Interaction subtitle
# =====================================
#' @keywords internal
.build_subtitle_interaction <- function(beta, p_value) {

  p_txt <- .format_p(p_value)

  if (p_value < 0.001) {
    paste0(
      "slope diff = ", round(beta, 2),
      " | p < 0.001"
    )
  } else {
    paste0(
      "slope diff = ", round(beta, 2),
      " | p = ", p_txt
    )
  }
}

# =====================================
# Correlation subtitle
# =====================================
#' @keywords internal
.make_subtitle_correlation <- function(method, estimate, n, p_value) {

  stat_name <- switch(
    method,
    pearson  = "r",
    spearman = "rho",
    kendall  = "tau"
  )

  p_label <- .format_p(p_value)

  paste0(
    stat_name, " = ", round(estimate, 2),
    " | p ", p_label,
    " (n = ", n, ")"
  )
}

# =====================================
# Chi-square subtitle
# =====================================
#' @keywords internal
.make_subtitle_chi <- function(cramers_v, p_value, small_expected = FALSE) {

  p_label <- .format_p(p_value)

  base <- paste0(
    "V = ", round(cramers_v, 2),
    " | p ", p_label
  )

  if (small_expected) {
    base <- paste0(base, " | low expected counts")
  }

  base
}

# =====================================
# Fisher subtitle
# =====================================
#' @keywords internal
.make_subtitle_fisher <- function(or, p_value) {

  p_label <- .format_p(p_value)

  or_label <- if (is.infinite(or)) {
    "Inf"
  } else {
    round(or, 2)
  }

  paste0(
    "OR = ", or_label,
    " | p ", p_label
  )
}
