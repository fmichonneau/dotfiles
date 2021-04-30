print.data.frame <- function(x, ...) {
  msg <- crayon::cyan("`data.frame` converted to `tibble` for printing.")
  cli::cli_alert_info("{.emph {msg}}")
  print(tibble::as_tibble(x), ...)
}
