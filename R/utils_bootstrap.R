# ============================
# Auxiliary bootstrap functions
# ============================

#' @keywords internal
.boot_one_sample <- function(
    x,
    stat_fun,
    B = 2000
) {

  boot_stat <- replicate(B, {

    x_b <- sample(x, length(x), replace = TRUE)
    stat_fun(x_b)

  })

  boot_stat <- boot_stat[is.finite(boot_stat)]

  ci <- quantile(boot_stat, c(0.025, 0.975), na.rm = TRUE)

  list(
    stat = stat_fun(x),
    ci_low = ci[1],
    ci_high = ci[2],
    boot = boot_stat
  )
}


#' @keywords internal
.boot_two_sample <- function(x, y, stat_fun, B = 2000, conf = 0.95) {

  x <- x[!is.na(x)]
  y <- y[!is.na(y)]

  boot_vals <- replicate(B, {

    xb <- sample(x, length(x), replace = TRUE)
    yb <- sample(y, length(y), replace = TRUE)

    stat_fun(xb, yb)

  })

  alpha <- (1 - conf) / 2

  ci <- quantile(
    boot_vals,
    probs = c(alpha, 1 - alpha),
    na.rm = TRUE
  )

  list(
    boot = boot_vals,
    ci_low = ci[1],
    ci_high = ci[2]
  )
}


#' @keywords internal
.boot_anova_omega <- function(data, group, value, B = 2000, conf = 0.95) {

  data <- data[!is.na(data[[value]]), ]

  groups <- unique(data[[group]])

  boot_vals <- replicate(B, {

    boot_data <- do.call(
      rbind,
      lapply(groups, function(g) {

        sub <- data[data[[group]] == g, ]

        sub[sample(nrow(sub), replace = TRUE), ]

      })
    )

    fit <- stats::aov(
      reformulate(group, value),
      data = boot_data
    )

    tab <- summary(fit)[[1]]

    ssb <- tab[1, "Sum Sq"]
    ssw <- tab[nrow(tab), "Sum Sq"]

    dfb <- tab[1, "Df"]
    msw <- tab[nrow(tab), "Mean Sq"]

    sst <- ssb + ssw

    (ssb - dfb * msw) / (sst + msw)

  })

  alpha <- (1 - conf) / 2

  ci <- quantile(
    boot_vals,
    c(alpha, 1 - alpha),
    na.rm = TRUE
  )

  list(
    omega = mean(boot_vals, na.rm = TRUE),
    ci_low = ci[1],
    ci_high = ci[2],
    boot = boot_vals
  )
}


#' @keywords internal
.boot_cramers_v <- function(tab, B = 2000, conf = 0.95) {

  df <- as.data.frame(as.table(tab))
  colnames(df) <- c("x", "y", "n")

  expanded <- df[rep(seq_len(nrow(df)), df$n), 1:2]

  boot_vals <- replicate(B, {

    idx <- sample(nrow(expanded), replace = TRUE)

    tb <- table(
      expanded$x[idx],
      expanded$y[idx]
    )

    .cramers_v(tb)

  })

  alpha <- (1 - conf) / 2

  ci <- quantile(
    boot_vals,
    c(alpha, 1 - alpha),
    na.rm = TRUE
  )

  list(
    v = mean(boot_vals, na.rm = TRUE),
    ci_low = ci[1],
    ci_high = ci[2],
    boot = boot_vals
  )
}
