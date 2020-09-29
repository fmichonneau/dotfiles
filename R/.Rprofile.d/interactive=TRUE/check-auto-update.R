local({
  source("~/.R/auto_update.R")
  if (is_outdated("~/.R/.date-last-update.rds")) {
    cli::cli_alert_danger("Library is out of date!")
  }
  if (file.exists("~/.R/.date-last-update.rds")) {
    last_update <- readRDS("~/.R/.date-last-update.rds")
    cli::cli_alert("Last update on {last_update}.")
  } else {
    cli::cli_alert_danger("Last updated file does not exist.")
  }
  cat("\n")
})
