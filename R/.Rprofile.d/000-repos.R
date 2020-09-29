local({
  repos <- c(
    RSPM = "https://packagemanager.rstudio.com/all/__linux__/bionic/latest/",
    CRAN = "https://cran.rstudio.com"
  )
  options(repos = c(repos, getOption("repos")))
  options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))

})


## for pak set library in my path
Sys.setenv("R_PKG_CACHE_DIR" = "~/.R/cache")
