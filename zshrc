# File: .zshrc
# Creation: 2013
# Author: Samuel El-Borai aka dgellow <samuel.elborai@gmail.com>
# Website: https://github.com/dgellow/home-bootstrapping
# Description: My zshrc

##————————————————————————————————————————————————————————————————————
##                   0. Meta

source $HOME/.profile

# Accept comments in command line
setopt interactivecomments

##————————————————————————————————————————————————————————————————————
##                   1. Oh-my-zsh

# Path to my oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Load oh-my-zshrc configuration.
source $ZSH/oh-my-zshrc

# Load fuzzy finder
source ~/.fzf.zsh
