local({
  repos <- c(
    CRAN = "https://cloud.r-project.org",
    bioconductor =  "https://www.bioconductor.org/packages/release/bioc/"
  )
  options(repos = c(repos, getOption("repos")))
})

## for pak set library in my path
Sys.setenv("R_PKG_CACHE_DIR" = "~/.R/cache")
