try(
  local({
    foghorn_results <- function() {
      if (require(foghorn, quietly = TRUE)) {
        foghorn::summary_cran_results(email = "francois.michonneau@gmail.com",
          pkg = c("arrow"))
      }
    }
    foghorn_results()
    unloadNamespace("foghorn")
}))
