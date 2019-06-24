local({
  repos <- c(
    CRAN = "https://cloud.r-project.org",
    bioconductor =  "https://www.bioconductor.org/packages/release/bioc/"
  )
  options(repos = c(repos, getOption("repos")))
})
