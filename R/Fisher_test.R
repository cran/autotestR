#' Fisher's Exact Test
#'
#' Performs Fisher's Exact Test using two categorical vectors or a data frame with two columns,
#' constructing a contingency table and optionally generating graphical visualizations.
#'
#' @param x Categorical vector or data frame with two columns.
#' @param y Categorical vector (required if x_var is a vector).
#' @param title Plot title (string). Default: "Fisher's Exact Test"
#' @param xlab Name of the x-axis in the plot (string). Default: NULL (uses variable name)
#' @param ylab Name of the y-axis in the plot (string). Default: "Proportion"
#' @param style Plot style. Controls the visualization type
#' @param show_table Logical. If TRUE, prints the contingency table to the console. Default: TRUE
#' @param help Logical. If TRUE, shows detailed function explanation. Default: FALSE
#' @param verbose Logical. If TRUE, prints detailed test messages. Default: TRUE
#'
#' @return Invisible object containing the Fisher test result.
#' @export
#'
#' @examples
#' data <- data.frame(control = c("healthy","healthy","sick","sick","sick"),
#'                    treatment = c("healthy","healthy","healthy","healthy","sick"))
#' test.fisher(data)

test.fisher <- function(x, y = NULL,
                        title = "Fisher's Exact Test",
                        xlab = NULL,
                        ylab = "Proportion",
                        style = c("stacked", "barplot", "mosaic", "pie"),
                        show_table = TRUE,
                        help = FALSE,
                        verbose = TRUE) {

  style <- match.arg(style)

  # Help
  if (help || missing(x)) {
    if (verbose) {
      message(
        "Function test.fisher()

Description:
  Performs Fisher's Exact Test to evaluate association between
  two categorical variables.

When to use:
  - Small samples
  - Expected frequencies < 5
  - Especially for 2x2 tables

Effect size:
  - Odds Ratio (OR)
  - log(OR) with confidence interval

Example:
data <- data.frame(control = c('healthy','healthy','sick','sick','sick'),
                   treatment = c('healthy','healthy','healthy','healthy','sick'))

test.fisher(data)

"
      )
    }
    return(invisible(NULL))
  }

  # Required packages
  required_packages <- c("ggplot2", "dplyr", "tidyr", "vcd")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        paste0(
          "Package '", pkg,
          "' is not installed. Install it first."
        ),
        call. = FALSE
      )
    }
  }

  # -----------------------------
  # Input handling
  # -----------------------------

  if (is.data.frame(x)) {

    if (ncol(x) != 2) {
      stop("The data frame must contain exactly two columns.", call. = FALSE)
    }

    column_names <- colnames(x)

    data_long <- tidyr::pivot_longer(
      x,
      cols = tidyselect::everything(),
      names_to = "group",
      values_to = "category"
    )

    data_long$group <- factor(
      data_long$group,
      levels = column_names
    )

    group <- data_long$group
    category <- data_long$category

    name_x <- column_names[1]
    name_y <- column_names[2]

  } else {

    if (is.null(y)) {
      stop("Argument 'y' must be provided if 'x' is a vector.", call. = FALSE)
    }

    if (length(x) != length(y)) {
      stop("Both variables must have the same length.", call. = FALSE)
    }

    group <- x
    category <- y

    name_x <- deparse(substitute(x))
    name_y <- deparse(substitute(y))

    name_x <- sub(".*\\$", "", name_x)
    name_y <- sub(".*\\$", "", name_y)
  }

  if (is.null(xlab)) xlab <- name_x
  if (is.null(ylab)) ylab <- "Proportion"


  # -----------------------------
  # Contingency table
  # -----------------------------

  contingency_table <- table(group, category)

  if (verbose && show_table) {
    message("Observed contingency table:")
    print(contingency_table)
  }

  # -----------------------------
  # Fisher test
  # -----------------------------

  test <- stats::fisher.test(contingency_table)

  # -----------------------------
  # Effect size (Odds Ratio)
  # -----------------------------

  or <- as.numeric(test$estimate)
  ci <- test$conf.int

  log_or <- log(or)
  log_ci <- log(ci)

  # -----------------------------
  # Prepare plot data
  # -----------------------------

  df_plot <- data.frame(group = group, category = category)

  df_prop <- df_plot |>
    dplyr::group_by(group, category) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(group) |>
    dplyr::mutate(prop = n / sum(n))

  # --- Subtitle ---
  subtitle_text <- .make_subtitle_fisher(
    or = or,
    p_value = test$p.value
  )

  # --- Colors ---
  vivid_colors <- scales::hue_pal()(length(unique(df_prop$group)))

  # -----------------------------
  # Plots
  # -----------------------------

  # --- Stacked ---
  if (style == "stacked") {

    g <- ggplot2::ggplot(
      df_prop,
      ggplot2::aes(x = group, y = prop, fill = category)
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = vivid_colors) +
      ggplot2::labs(
        title = title,
        subtitle = subtitle_text,
        y = ylab,
        fill = name_y
      ) +
      ggplot2::theme_minimal()

  }

  # --- Barplot ---
  if (style == "barplot") {

    g <- ggplot2::ggplot(
      df_prop,
      ggplot2::aes(x = group, y = prop, fill = category)
    ) +
      ggplot2::geom_bar(
        stat = "identity",
        position = ggplot2::position_dodge()
      ) +
      ggplot2::scale_fill_manual(values = vivid_colors) +
      ggplot2::labs(
        title = title,
        subtitle = subtitle_text,
        y = ylab,
        fill = name_y
      ) +
      ggplot2::theme_minimal()
  }

  # --- Mosaic ---
  if (style == "mosaic") {

    vcd::mosaic(
      contingency_table,
      shade = TRUE,
      legend = TRUE,
      main = paste0(title, "\n", subtitle_text)
    )
  }

  # --- Pie ---
  # --- Pie ---
  if (style == "pie") {

    g <- ggplot2::ggplot(
      df_prop,
      ggplot2::aes(x = "", y = prop, fill = category)
    ) +
      ggplot2::geom_bar(stat = "identity", width = 1) +
      ggplot2::coord_polar("y") +
      ggplot2::facet_wrap(~ group) +
      ggplot2::scale_fill_manual(values = vivid_colors) +
      ggplot2::theme_void(base_size = 12) +
      ggplot2::labs(
        title = title,
        subtitle = subtitle_text,
        fill = name_y
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          size = 14
        ),
        plot.subtitle = ggplot2::element_text(
          hjust = 0.5,
          size = 11,
          margin = ggplot2::margin(b = 10)
        ),
        strip.text = ggplot2::element_text(
          size = 12
        )
      )
  }

  if (style != "mosaic") print(g)

  # -----------------------------
  # Output
  # -----------------------------
  obj <- list(
    type = "Fisher",
    p = test$p.value,
    odds_ratio = or,
    or_ci = ci,
    log_or = log_or,
    log_or_ci = log_ci,
    table = contingency_table,
    data = df_plot
  )

  # -----------------------------
  # Return
  # -----------------------------

  if (verbose) {

    .print_header("Fisher's Exact Test")

    .print_block("Statistics", function() {

      cat(
        "Odds Ratio = ",
        round(or, 3),
        " [",
        round(ci[1], 3), ", ",
        round(ci[2], 3),
        "]\n",
        sep = ""
      )

      cat(
        "log(OR) = ",
        round(log_or, 3),
        " [",
        round(log_ci[1], 3), ", ",
        round(log_ci[2], 3),
        "]\n",
        sep = ""
      )

      cat(
        "p = ",
        .format_pval(test$p.value),
        "\n",
        sep = ""
      )
    })
  }

  return(invisible(list(result = obj)))
}
