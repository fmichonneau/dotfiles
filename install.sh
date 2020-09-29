#!/usr/bin/env bash

stow emacs
stow git
stow R
stow config
stow bash
stow ccache

printf "0 9 * *  1 /usr/bin/Rscript --vanilla -e 'source(\"~/.R/auto_update.R\"); .libPaths(\"~/.R/library\"); readRenviron(\"~/.Renviron\"); auto_update()' 2>&1\n" > ~/etc/crontab

crontab ~/etc/crontab
