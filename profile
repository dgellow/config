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
alias cdgit='cd $(git rev-parse --show-cdup)'

# Main editor
export EDITOR="emacs -nw"

# 256 colors terminals
export TERM=xterm-256color

# Disable xterm <C-S> freezed mode
stty stop ''

# Syntax highlighting on stdin
hilite () {
    lang=$1
    source-highlight --out-format=esc --output=STDOUT --src-lang=$lang
}


##————————————————————————————————————————————————————————————————————
##                   1. Utilities

# Heroku
export PATH=/usr/local/heroku/bin:$PATH

# Cask (emacs plugin management)
export PATH=$HOME/.cask/bin:$PATH


##————————————————————————————————————————————————————————————————————
##                   2. Programming


##————————————————————————————————————————————————————————————————————
##                   2.1 Ruby

export PATH="`ruby -e 'puts Gem.user_dir'`/bin:$PATH"

# RVM
export PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"


##————————————————————————————————————————————————————————————————————
##                   2.2 Python

# Virtualenv (Python)
export WORKON_HOME=$HOME/.virtualenvs
export PIP_VIRTUALENV_BASE=$WORKON_HOME
export PIP_RESPECT_VIRTUALENV=true


##————————————————————————————————————————————————————————————————————
##                   2.3 Node

# NPM, global install in user directory
export PATH=$HOME/.local/bin:$PATH