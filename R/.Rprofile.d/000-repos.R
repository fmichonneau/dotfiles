local({
  get_ubuntu_codename <- function() {
    os <- readLines("/etc/os-release")
    cn <- os[grepl("UBUNTU_CODENAME", os)]
    unlist(strsplit(cn, "="))[2]
  }
  codename <- "focal" #get_ubuntu_codename()

  repos <- c(
    RSPM = paste0(
      "https://packagemanager.rstudio.com/all/__linux__/", codename, "/latest/"
    ),
    carpentries = "https://carpentries.github.io/drat/"
  )
  options(repos = c(repos, getOption("repos")))
  options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))

})
