autoUpdate <- function(freqUpdate = 7,
                       fileLastUpdate = "~/.R/.date-last-update.rds",
                       lib.loc = "~/.R/library/") {

  createLastUpdate <- function(fileLastUpdate) {
    lastUpdate <- Sys.Date()
    saveRDS(lastUpdate, file=fileLastUpdate)
  }

  exeUpdate <- FALSE

  if(!file.exists(fileLastUpdate)) {
    exeUpdate <- TRUE
  } else {
    lastUpdate <- readRDS(file = fileLastUpdate)
    today <- Sys.Date()
    nbDays <- today - lastUpdate
    if(nbDays >= freqUpdate)
      exeUpdate <- TRUE
    else
      message("Next automatic update in ", freqUpdate-nbDays, " days.")
  }

  if(exeUpdate) {
    message("It's time to update your packages... it can take some time...")
    if(require(utils)) {
      ow <- options("warn")
      options(warn = 2) # warnings are turned into err msg
      z <- try(devtools::update_packages(packages = TRUE, upgrade = "always",
                                         checkBuilt = TRUE),
               silent=TRUE)
      options(ow)
    }

    ## TODO -- store R version, and do a check build

    if(inherits(z, "try-error")) {
      message("Auto update failed, unable to access repositories or update for a package for which you don't have appropriate privileges.")
    }
    else {
      createLastUpdate(fileLastUpdate)
      message("Auto update succeed, date of update saved.")
    }
  }
  invisible()
}
