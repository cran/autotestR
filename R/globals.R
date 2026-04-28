# Declare global variables to avoid NOTE: no visible binding
# ---------------------------------------------------------------------------------
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    # Internal variables
    "group", "id", "ind", "n", "p adj", "p.adj", "P.adj", "p_adj",
    "prop", "value", "values", "x1", "x2", "y", "signif",

    # Standard variables
    "group", "value"
  ))
}

