source $HOME/.profile

# Path to my oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Load oh-my-zshrc configuration.
source $ZSH/oh-my-zshrc

# Customize to your needs...
export EDITOR=vim
export LUA_INIT="_PROMPT='lua > '"

alias q='exit'
alias :q='exit'
alias visudo='sudo -E visudo'
 
# Disable xterm <C-S> freezed mode
stty stop ''


# RVM
#####
# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" 
 
# RVM bash completion
[[ -r "$HOME/.rvm/scripts/completion" ]] && source "$HOME/.rvm/scripts/completion"


# Virtualenvwrapper (Python)
###########################
# Load virtualenvwrapper.sh
source /usr/bin/virtualenvwrapper.sh
