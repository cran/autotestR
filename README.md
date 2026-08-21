
# autotestR

<!-- badges: start -->
<!-- badges: end -->

## Overview

**autotestR** is an R package designed to simplify and improve statistical analysis
in the life sciences.

It combines automatic test selection, diagnostic evaluation,
effect size reporting, and intuitive visualization to support
transparent and responsible data interpretation.


## Installation

You can install the development version of autotestR directly from GitHub:

```r
# Install autotestR from GitHub

install.packages("remotes")
remotes::install_github("Luiz-Garcia-R/autotestR")

# Install autotestR from CRAN
install.packages("autotestR")
```


## Philosophy

autotestR is designed to go beyond "p-value driven" analysis.

The package emphasizes:

- Effect sizes and confidence intervals
- Automatic diagnostic checks
- Warnings when test assumptions are violated
- Transparent reporting of uncertainty
- Encouragement of biological interpretation over mechanical decision-making

The goal is to support reproducible, interpretable, and responsible data analysis in the life sciences.


## Main features

- Automatic selection and execution of classical statistical tests
- Integrated assumption diagnostics and warnings
- Effect size estimation with confidence intervals
- Robust alternatives for non-normal data
- Built-in graphical summaries
- Consistent and interpretable outputs
- Support for categorical, continuous, and correlation analyses


## What makes autotestR different?

Unlike many statistical wrappers that focus mainly on hypothesis testing,
autotestR prioritizes interpretation.

Instead of providing only p-values, the package:

- Reports effect sizes whenever possible
- Highlights uncertainty
- Flags potential violations of assumptions
- Encourages users to look beyond statistical significance

This makes autotestR especially suitable for exploratory and applied research in biology, medicine, and veterinary sciences.


## Automatic diagnostics and warnings

Many functions in autotestR automatically evaluate key assumptions
(e.g., normality, homoscedasticity, expected frequencies).

When potential issues are detected, the user is informed through
clear warnings and messages, helping prevent inappropriate test usage.

### Basic usage

```r
library(autotestR)

# Independent t test
group1 <- rnorm(30, 10, 2)
group2 <- rnorm(30, 12, 2)
test.t(group1, group2)

# Chi-squared test
var1 <- sample(c("A", "B"), 100, replace = TRUE)
var2 <- sample(c("Yes", "No"), 100, replace = TRUE)
test.chi(var1, var2)

# Multiple test (t test or Mann–Whitney)
df <- data.frame(
  control   = rnorm(30, 10),
  treatment = rnorm(30, 12),
  test1     = rnorm(30, 11),
  test2     = rnorm(30, 15)
)
test.tmulti(df)

# ANOVA with post hoc test
g1 <- rnorm(20, 5)
g2 <- rnorm(20, 7)
g3 <- rnorm(20, 6)
test.anova(g1, g2, g3)

# Correlation test
x <- rnorm(30)
y <- x + rnorm(30, 0, 1)
test.correlation(x, y)
```


# Contact

If you have questions, suggestions, or would like to contribute, feel free to open an issue or submit a pull request on the GitHub repository.

Thank you for using autotestR!
