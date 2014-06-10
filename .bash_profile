# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs
## xflux for nice lighting
if [ ! -z `which xflux 2>/dev/null` ]
then
    xflux -z 01721 -k 2700
fi
## gpg-agent because it needs help
if [ ! -z `which gpg-agent 2>/dev/null` ]
then
    # at least on arch, gpg-agent echos the environment var it needs
    # but doesn't actually set it
    eval `gpg-agent --daemon`
fi

PATH=$PATH:$HOME/.local/bin:$HOME/bin

alias ls="ls --color"
alias ll="ls -l --color"
alias la="ls -a --color"

export PATH
