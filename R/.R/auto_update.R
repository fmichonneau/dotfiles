create_last_updated <- function(path) {
  saveRDS(Sys.time(), file=path)
}

is_outdated <- function(path_last_updated, max_days = 7) {
  if (!file.exists(path_last_updated)) {
    return(TRUE)
  }

  last_updated <- readRDS(path_last_updated)

  difftime(Sys.time() - last_updated, unit = "days") > 7
}

auto_update <- function(update_freq = 7,
                        last_updated = "~/.R/.date-last-update.rds",
                        lib.loc = "~/.R/library/") {

  if (!is_outdated(last_updated)) {
    last_updated <- readRDS(path_last_updated)
    next_update <- floor(
      update_freq - difftime(Sys.time(), last_updated, unit = "days")
    )
    message("Next update in ", next_update,  "days.")
    return(invisible())
  }

  message("It's time to update your packages... it can take some time...")

  ow <- options("warn")
  options(warn = 2) # warnings are turned into err msg
  z <- try(
    callr::r(function() {
      readRenviron("~/.Renviron")
      remotes::update_packages(
        packages = TRUE, upgrade = "always", checkBuilt = TRUE,
        lib.loc = lib.loc
      )
    },
    cmdargs = "--vanilla"
    ),
    silent = FALSE
  )
  options(ow)

  if(inherits(z, "try-error")) {
    cli::cli_alert_danger(paste(
      "Auto update failed. ",
      "Unable to access repositories or update for a package ",
      "for which you don't have appropriate privileges."
    ))
  } else {
    create_last_updated(last_updated)
    cli::cli_alert_success("Auto update succeed, date of update saved.")
  }

  invisible()
}
