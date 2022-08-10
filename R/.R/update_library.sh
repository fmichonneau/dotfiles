#!/usr/bin/env bash

echo "$(date) starting to update R library" &&
    /usr/bin//Rscript -e 'readRenviron(file.path(Sys.getenv("HOME"), ".Renviron")); .libPaths(file.path(Sys.getenv("HOME"), ".R/library")); source(file.path(Sys.getenv("HOME"), ".R/auto_update.R")); auto_update()' &&
    echo "   ... DONE!"
