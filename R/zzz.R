.onAttach <- function(libname, pkgname) {

  version_text <- paste0(
    "autotestR v.",
    utils::packageVersion("autotestR")
  )

  if (requireNamespace("crayon", quietly = TRUE)) {
    version_text <- crayon::green(version_text)
  }

  packageStartupMessage(
    "\n",
    version_text, " loaded successfully!\n",
    "--------------------------------------------------\n",
    "Simplified statistical analysis with automatic diagnostics and clear graphics.\n",
    "GitHub: https://github.com/Luiz-Garcia-R/autotestR\n"
  )
}
