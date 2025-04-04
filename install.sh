#!/usr/bin/env bash

mkdir -p ~/etc
mkdir -p ~/.R-library

stow emacs -t $HOME -v
stow git  -t $HOME -v
stow R  -t $HOME -v
#stow local  -t $HOME -v
stow config  -t $HOME -v
# stow bash  -t $HOME -v
stow ccache  -t $HOME -v


# printf "## don't edit this file by hand, edit the install script instead\n38 6,9,12 * * * bash %s/.R/update_library.sh >> /tmp/update_r_library.log 2>&1\n" "$HOME" > ~/etc/crontab

# crontab ~/etc/crontab
