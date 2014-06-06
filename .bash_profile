# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs
if [ ! -z `which xflux 2>/dev/null` ]
then
    xflux -z 01721 -k 2700
fi

PATH=$PATH:$HOME/.local/bin:$HOME/bin

alias ls="ls --color"
alias ll="ls -l --color"
alias la="ls -a --color"

export PATH
