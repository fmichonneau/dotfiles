#!/bin/sh

alias gr="git remote -v"

alias bs="bundle config set path ~/.vendor/bundle && bundle install && bundle update && bundle exec jekyll serve --livereload"

alias create_gemfile="echo \"source 'https://rubygems.org'\" > Gemfile && echo \"gem 'github-pages', group: :jekyll_plugins\" >> Gemfile"

alias R="R --quiet --no-save --no-restore"
