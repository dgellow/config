# File: .bashrc
# Creation Date: 2013
# Author: Samuel El-Borai <sam@elborai.me>
# Website: https://github.com/dgellow/config
# Description: My bashrc

##————————————————————————————————————————————————————————————————————
##                   0. Meta

# Check for an interactive session
[ -z "$PS1" ] && return

##————————————————————————————————————————————————————————————————————
##                   1. Prompt

source ~/.git-prompt.sh

function git_prompt_branch() {
	GIT_PS1_SHOWDIRTYSTATE="false"
	GIT_PS1_SHOWSTASHSTATE="false"
	GIT_PS1_SHOWUNTRACKEDFILES="true"
	GIT_PS1_SHOWUPSTREAM="auto"
	GIT_PS1_SHOWCOLORHINTS="true"

	local branch=$(__git_ps1)
	echo $branch
}

function composition() {
	local branch=$(git_prompt_branch)
	if [[ -n "$rvm_version" ]] || [[ -n "$branch" ]]; then
		branch="\e[1;33m$branch"
	fi
}

PS1="\n \e[1;31mREC °\e[0m: \D{%d-%m-%y %H:%M:%S} \u/\e[4m\h\e[0m \w\$(composition)\e[0m\n -> "
