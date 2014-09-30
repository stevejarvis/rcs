# set the path
export PATH=/usr/local/bin:$PATH:/Users/steve/bin:/Users/sjarvis/bin:/usr/texbin

# history
setopt hist_ignore_all_dups inc_append_history
HISTFILE=~/.zhistory
HISTSIZE=4096
SAVEHIST=4096

# globbing
set extendedglob

# load perforce config if found
if [ -f "$HOME/.p4c" ]; then
    export P4CONFIG=$HOME/.p4c
fi

# aliased commands
alias ll="ls -l"
alias la="ls -a"
alias hd="hexdump"
alias emacsw="/Applications/Emacs.app/Contents/MacOS/Emacs &"
alias p4='p4 -zmaxScanRows=10000000 -zmaxResults=10000000 -zmaxLockTime=600000'

# init colors, prompt
autoload -U compinit promptinit colors 
compinit
promptinit
colors

export CLICOLOR=1

# set prompt
PROMPT="%{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m %{$fg_no_bold[yellow]%}%1~ %{$reset_color%}%# "
RPROMPT="[%{$fg_no_bold[yellow]%}%?%{$reset_color%}]"
