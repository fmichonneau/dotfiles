#!/usr/bin/env bash

mkdir -p ~/etc
mkdir -p ~/.R-library

stow emacs
stow git
stow R
stow local
stow config
stow bash
stow ccache

printf "## don't edit this file by hand, edit the install script instead\n38 6,9,12 * * * bash %s/.R/update_library.sh >> /tmp/update_r_library.log 2>&1\n" "$HOME" > ~/etc/crontab

crontab ~/etc/crontab
