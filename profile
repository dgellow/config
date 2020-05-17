# File: .profile
# Creation Date: 2013
# Author: Samuel El-Borai <sam@elborai.me>
# Website: https://github.com/dgellow/config
# Description: My sh environment

##————————————————————————————————————————————————————————————————————
##                   0. Meta

# Aliases
alias cdgit='cd $(git rev-parse --show-cdup)'

# Main editor
export EDITOR="nano"

# 256 colors terminals
export TERM=xterm-256color

# Disable xterm <C-S> freezed mode
stty stop ''

# Syntax highlighting on stdin.
# Requires "source-highlight" as dependency
# Usage: cat file.sh | hilite sh
hilite() {
    lang=$1
    source-highlight --out-format=esc --output=STDOUT --src-lang=$lang
}

# Set `less` encoding
export LESSCHARSET=utf-8

##————————————————————————————————————————————————————————————————————
##                   1. Utilities

# Homebrew (mac os x package management)
export PATH=/usr/local/bin:$PATH

##————————————————————————————————————————————————————————————————————
##                   2. Programming

##————————————————————————————————————————————————————————————————————
##                   2.1 Ruby

# Currently not used

##————————————————————————————————————————————————————————————————————
##                   2.2 Python

# Currently not used

##————————————————————————————————————————————————————————————————————
##                   2.3 Node

# Currently not used

##————————————————————————————————————————————————————————————————————
##                   2.4 Nim

# Add Nimble binaries to PATH
export PATH="$HOME/.nimble/bin:$PATH"

##————————————————————————————————————————————————————————————————————
##                   2.4 Rust

# Add Cargo binaries to PATH
export PATH="$HOME/.cargo/bin:$PATH"

##————————————————————————————————————————————————————————————————————
##                   2.4 Golang

# Set GOPATH for external packages
export GOPATH="$HOME/Development/Go/gopath"

# Add golang binaries to PATH
export PATH="$GOPATH/bin:$PATH"

# Ensure directories exists
mkdir -p $GOPATH/{pkg,src}
