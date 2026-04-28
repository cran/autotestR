.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n",
    crayon::green("autotestR "), "loaded successfully\n",
    "----------------------------------------\n",
    "Simplified statistical analysis with automatic diagnostics and clear graphics.\n",
    "GitHub: https://github.com/Luiz-Garcia-R/autotestR\n"
  )
}
