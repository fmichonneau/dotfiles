#!/usr/bin/env bash

stow emacs
stow git
stow R
stow config
stow bash
stow ccache

printf "## don't edit this file by hand, edit the install script instead\n38 6,9,12 * * * bash /home/francois/.R/update_library.sh >> /tmp/update_r_library.log 2>&1\n" > ~/etc/crontab

crontab ~/etc/crontab
