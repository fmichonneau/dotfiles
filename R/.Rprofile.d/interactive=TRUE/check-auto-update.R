local({
  source("~/.R/auto_update.R")
  lib_is_outdated <- is_outdated("~/.R/.date-last-update.rds")
  last_update_days <- attr(lib_is_outdated, "last_update_in_days")
  if (lib_is_outdated) {
    cli::cli_alert_danger("Library is out of date!")
  }
  if (file.exists("~/.R/.date-last-update.rds")) {
    last_update <- readRDS("~/.R/.date-last-update.rds")
    cli::cli_alert(
      "Last update on {last_update} ({last_update_days} days ago)."
    )
  } else {
    cli::cli_alert_danger("Last updated file does not exist.")
  }
  cat("\n")
})
