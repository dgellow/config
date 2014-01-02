# File: .profile
# Creation: 2013
# Author: Samuel El-Borai aka dgellow <samuel.elborai@gmail.com>
# Website: https://github.com/dgellow/home-bootstrapping
# Description: My sh environment

##————————————————————————————————————————————————————————————————————
##                   0. Meta

# Aliases
alias q='exit'
alias :q='exit'
alias visudo='sudo -E visudo'

# Main editor
export EDITOR="emacs -nw"

# 256 colors terminals
export TERM=xterm-256color

# Disable xterm <C-S> freezed mode
stty stop ''


##————————————————————————————————————————————————————————————————————
##                   1. Utilities

# Heroku
export PATH=/usr/local/heroku/bin:$PATH


##————————————————————————————————————————————————————————————————————
##                   2. Programming


##————————————————————————————————————————————————————————————————————
##                   2.1 Ruby

export PATH=$PATH:$HOME/.gem/ruby/2.0.0/bin
 
# RVM
export PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# RVM bash completion
[[ -r "$HOME/.rvm/scripts/completion" ]] && source "$HOME/.rvm/scripts/completion"


##————————————————————————————————————————————————————————————————————
##                   2.2 Python

# Virtualenv (Python)
export WORKON_HOME=$HOME/.virtualenvs
export PIP_VIRTUALENV_BASE=$WORKON_HOME
export PIP_RESPECT_VIRTUALENV=true
