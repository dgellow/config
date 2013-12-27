# Check for an interactive session
[ -z "$PS1" ] && return
 
alias ls='ls --color=auto'
 
#
# Prompt configuration
#
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
 
 
#
# Personnal configuration
#
 
export EDITOR=vim
export LUA_INIT="_PROMPT='lua > '"
 
alias visudo='sudo -E visudo'
alias q='exit'
 
alias :q='exit'
 
# Disable xterm <C-S> freezed mode
stty stop ''

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" 
 
# RVM bash completion
[[ -r "$HOME/.rvm/scripts/completion" ]] && source "$HOME/.rvm/scripts/completion"