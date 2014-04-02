# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs
if [ ! -z `which xflux` ]
then
    xflux -z 01721 -k 2700
fi

PATH=$PATH:$HOME/.local/bin:$HOME/bin

export PATH
