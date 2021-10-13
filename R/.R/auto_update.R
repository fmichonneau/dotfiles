create_last_updated <- function(path) {
  saveRDS(Sys.time(), file=path)
}

is_outdated <- function(path_last_updated, max_days = 7) {
  if (!file.exists(path_last_updated)) {
    return(TRUE)
  }
  last_updated <- readRDS(path_last_updated)

  time_since_update <- difftime(Sys.time(), last_updated, units = "days")

  res <- time_since_update >=  max_days
  attr(res, "last_update_in_days") <- floor(
    as.numeric(time_since_update)
  )
  res
}

auto_update <- function(update_freq = 7,
                        last_updated = "~/.R/.date-last-update.rds",
                        last_output = "~/.R/.output-last-update.log",
                        lib.loc = "~/.R/library/") {

  if (!is_outdated(last_updated, update_freq)) {
    cli::cli_alert_success("Library is up to date.")
    return(invisible())
  }

  message("It's time to update your packages... it can take some time...")

  ow <- options("warn")
  options(warn = 2) # warnings are turned into err msg
  on.exit(options(ow))

  to_upgrade <- old.packages(lib.loc = lib.loc, checkBuilt = TRUE)[, "Package"]
  z <- try(
    capture.output({
      pak::pkg_install(to_upgrade, ask = FALSE)
    },
    file = last_output),
    silent = FALSE
  )

  ## z <- try({
  ##   callr::r_vanilla(function(lib.loc) {
  ##     readRenviron("~/.Renviron")
  ##     .libPaths(lib.loc)
  ##     message("Update starting at ", Sys.time(), " ---------------")
  ##     remotes::update_packages(
  ##       packages = TRUE, upgrade = "always", checkBuilt = TRUE,
  ##       lib.loc = lib.loc
  ##     )
  ##   },
  ##   args = list(lib.loc),
  ##   show = TRUE,
  ##   stdout = last_output,
  ##   stderr = last_output
  ##   )
  ## },
  ## silent = FALSE
  ## )

  if(inherits(z, "try-error")) {
    cli::cli_alert_danger(paste(
      "Auto update failed. "
    ))
  } else {
    create_last_updated(last_updated)
    cli::cli_alert_success("Auto update succeed, date of update saved.")
  }

  invisible()
}
