local({
  get_ubuntu_codename <- function() {
    os <- readLines("/etc/os-release")
    cn <- os[grepl("UBUNTU_CODENAME", os)]
    unlist(strsplit(cn, "="))[2]
  }
  codename <- "noble" #get_ubuntu_codename()
  if (get_ubuntu_codename() != codename) {
    cli::cli_warn(
      "{codename} and {get_ubuntu_codename()} don't match. Update the config file."
    )
  }

  repos <- c(
    RSPM = paste0(
      "https://packagemanager.rstudio.com/all/__linux__/",
      codename,
      "/latest/"
    )
  )
  options(repos = c(repos, getOption("repos")))

  options(
    HTTPUserAgent = sprintf(
      "R/%s R (%s)",
      getRversion(),
      paste(
        getRversion(),
        R.version["platform"],
        R.version["arch"],
        R.version["os"]
      )
    )
  )
})
