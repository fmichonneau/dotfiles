local({
  r_v <- paste(R.Version()$version.string, "--", R.Version()$nickname)
  cli::cli_h1(r_v)
  cat("\n")
})
