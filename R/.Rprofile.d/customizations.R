local({
  Sys.setenv(MAKEFLAGS = "-j7")
  options(
    editor = "emacsclient",
    browserNLdisabled = TRUE,
    Ncpus = 7L,
    max.print = 100L,
    error =  quote(rlang::entrace()),
    rlang__backtrace_on_error = "collapse"
  )
})
