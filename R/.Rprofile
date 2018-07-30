try(source("http://bioconductor.org/biocLite.R"))

Sys.setenv(MAKEFLAGS = "-j7")

options(repos = c(
            bioconductor = "http://www.bioconductor.org/packages/release/bioc/",
            CRAN = "https://cran.rstudio.com/"
        ),
        editor = "emacsclient",
        browserNLdisabled = TRUE,
        Ncpus = 7L,
        max.print = 100,
        ## warn on partial matches
        ## warnPartialMatchAttr = TRUE,
        ## warnPartialMatchDollar = TRUE,
        ## warnPartialMatchArgs = TRUE,
        ## devtools
        devtools.name = "Francois Michonneau",
        devtools.desc.author =
          "person(\"Francois\", \"Michonneau\", role = c(\"aut\", \"cre\"),
               email = \"francois.michonneau@gmail.com\",
               comment = c(\"ORCID\" = \"0000-0002-9092-966X\"))",
        devtools.desc.license = "MIT + file LICENSE",
        devtools.revdep.libpath = "~/.R/revdep-library/", # library for checking reverse dependencies
        devtools.path = "~/.R-dev-lib"			  # library for devtools' dev_mode
        )

load_rtweet <- function() {
    if (interactive() && require(rtweet)) {
        rtweet_key <- "~/.R/rtweet-key.rds"
        if (!file.exists(rtweet_key)) {
            message("Getting rtweet key")
            twitter_token <- rtweet::create_token(
                                         app = "rtweet-apir",
                                         consumer_key = Sys.getenv("TWITTER_CONSUMER_KEY"),
                                         consumer_secret = Sys.getenv("TWITTER_CONSUMER_SECRET"))
            saveRDS(twitter_token, rtweet_key)
        }
        Sys.setenv(TWITTER_PAT = rtweet_key)
    }
}

auto_update <- function() {
  ## Autoupdates packages and cleans everything
  source("~/.R/autoUpdate.R")
  autoUpdate()

  ## From: http://stackoverflow.com/questions/27129622/define-a-callback-for-command-start/40963728#40963728
    notify_long_running <- function(second_cutoff = 30) {
        last <- proc.time()[1]
        function(expr, value, ok, visible) {
            duration <- proc.time()[1] - last
            if (duration > second_cutoff) {
                notifier::notify(msg = paste0(collapse = " ", deparse(expr)),
                                 title = sprintf("Completed in %.02f (s)", duration))
            }
            last <<- proc.time()[1]
            TRUE
        }
    }
    addTaskCallback(notify_long_running())
}

show_fortune <- function()  {
  if (require(fortunes, quietly = TRUE)) {
    frt <- fortune()
    if (require(crayon, quietly = TRUE)) {
      frt[] <- lapply(frt[], crayon::cyan)
    }
    print(frt)
  }
}

foghorn_results <- function() {
  if (require(foghorn, quietly = TRUE)) {
    foghorn::summary_cran_results(email = "francois.michonneau@gmail.com",
                                  pkg = c("ridigbio", "mregions"))
  }
}

if (interactive()) {
  zz <- try(auto_update(), silent=FALSE)
  if (inherits(zz, "try-error"))
    message("Something went wrong in the initialization process.")
  suppressMessages(library(devtools))
  suppressPackageStartupMessages(library(lookup))
  show_fortune()
  foghorn_results()
  rm(list=ls())
}
