#!/usr/bin/env bash

echo "$(date) starting to update R library" &&
    /usr/bin//Rscript -e 'readRenviron("/home/francois/.Renviron"); .libPaths("/home/francois/.R/library"); source("/home/francois/.R/auto_update.R"); auto_update()' &&
    echo "   ... DONE!"
