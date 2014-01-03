# File: .bashrc
# Creation: 2013
# Author: Samuel El-Borai aka dgellow <samuel.elborai@gmail.com>
# Website: https://github.com/dgellow/home-bootstrapping
# Description: My bashrc

##————————————————————————————————————————————————————————————————————
##                   0. Meta

# Check for an interactive session
[ -z "$PS1" ] && return
 

##————————————————————————————————————————————————————————————————————
##                   1. Prompt

source ~/.git-prompt.sh
 
function git_prompt_branch {
	GIT_PS1_SHOWDIRTYSTATE="false"
	GIT_PS1_SHOWSTASHSTATE="false"
	GIT_PS1_SHOWUNTRACKEDFILES="true"
	GIT_PS1_SHOWUPSTREAM="auto"
	GIT_PS1_SHOWCOLORHINTS="true"


	local branch=$(__git_ps1)
	echo $branch
}
 
function rvm_prompt_version {
	local version=$(~/.rvm/bin/rvm-prompt i v g u | awk '{split($0,a,"-"); print substr (a[1], 0, 2), a[2], a[3]}')
	echo $version
}
 
function composition {
	local branch=$(git_prompt_branch)
	local rvm_version=$(rvm_prompt_version)
 
	if [[ -n "$rvm_version" ]] || [[ -n "$branch" ]]; then
		if [[ -z "$rvm_version" ]]; then
			rvm_version="\t\t\t\t\t"
		else
			rvm_version="\e[1;36m$rvm_version\t\t\t\t"
		fi
 
		branch="\e[1;33m$branch"
 
		echo -e "\n $rvm_version $branch"
	fi
}
 
PS1="\n \e[1;31mREC °\e[0m: \D{%d-%m-%y %H:%M:%S} \u/\e[4m\h\e[0m \w\$(composition)\e[0m\n -> "


##————————————————————————————————————————————————————————————————————
##                   2. Programming


##————————————————————————————————————————————————————————————————————
##                   2.1 Ruby

# RVM bash completion
[[ -r "$HOME/.rvm/scripts/completion" ]] && source "$HOME/.rvm/scripts/completion"