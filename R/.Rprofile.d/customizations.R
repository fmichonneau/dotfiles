Sys.setenv(MAKEFLAGS = "-j15")
options(
  editor = "emacsclient",
  browserNLdisabled = TRUE,
  Ncpus = 15L,
  max.print = 100L,
  rlang_backtrace_on_error = "collapse",
  foghorn_columns = c("error", "fail", "warn", "note", "ok", "deadline"),
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE,
  warnPartialMatchArgs = TRUE
)
if (!startup::sysinfo()$rstudio) options(error = quote(rlang::entrace()))
