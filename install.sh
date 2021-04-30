#!/usr/bin/env bash

stow emacs
stow git
stow R
stow config
stow bash
stow ccache

printf "0 9 * *  * /usr/bin/Rscript -e 'source(\"~/.R/auto_update.R\"); auto_update()' 2>&1\n" > ~/etc/crontab

crontab ~/etc/crontab
