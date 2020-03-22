try(startup::startup())


local({
  message("It's time to update your packages... it can take some time...")

  if(! (require(sys, quietly = TRUE) && require(remotes, quietly = TRUE) &&
          require(pak, quietly = TRUE) && require(utils, quietly = TRUE))) {
    remotes::install_cran(c("sys", "remotes"))
  }

  ow <- options("warn")
  on.exit(options(ow))
  options(warn = 2) # warnings are turned into err msg
  z <- try(
    remotes::update_packages(
      packages = TRUE, upgrade = "always", checkBuilt = TRUE
    ),
    silent = TRUE
  )

  to_update <- utils::old.packages(lib.loc = "~/.R/library", checkBuilt = TRUE)
  z <- try(
    pak::pkg_install(
      to_update[, "Package"], lib = "~/.R/library", upgrade = TRUE, ask = FALSE
    ),
    silent = TRUE
  )

  if(inherits(z, "try-error")) {
    stop("Auto update failed, unable to access repositories or update for a package for which you don't have appropriate privileges. Original error: ",
      z)
  }

})
