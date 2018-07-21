### created 2008-07-29
### by François Michonneau - francois.michonneau@gmail.com

### last modified 2009-04-22

### Works only for linux (and probably MacOSX)

### Checks the date of the last update in fileLastUpdate (or creates it,
### if file doesn't exist) and depending of the frequency of the update,
### update silently all packages

autoUpdate <- function(freqUpdate = 7,
                       fileLastUpdate = "~/.R/.date-last-update.rds",
                       lib.loc = "~/.R/library/") {

    createLastUpdate <- function(fileLastUpdate) {
        lastUpdate <- Sys.Date()
        save(lastUpdate, file=fileLastUpdate)
    }
    exeUpdate <- FALSE
    if(!file.exists(fileLastUpdate)) {
        exeUpdate <- TRUE
    }
    else {
        load(file = fileLastUpdate)
        today <- Sys.Date()
        nbDays <- today - lastUpdate
        if(nbDays >= freqUpdate)
            exeUpdate <- TRUE
        else
            message("Next automatic update in ", freqUpdate-nbDays, " days")
    }

    if(exeUpdate) {
        message("It's time to update your packages... it can take some time...")
        if(require(utils)) {
            ow <- options("warn")
            options(warn = 2) # warnings are turned into err msg
            z <- try(update.packages(ask = FALSE, lib.loc = lib.loc,
                                     checkBuilt = TRUE, dependencies = FALSE),
                     silent=TRUE)
            options(ow)
        }

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
