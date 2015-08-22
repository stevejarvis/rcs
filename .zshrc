# set the path
export PATH=/usr/local/sbin:/usr/local/bin:/Users/steve/bin:/Users/sjarvis/bin:/home/sjarvis/bin:/usr/texbin:$PATH

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

# docker CIP business
export DOCKER_HOST=192.168.50.4:2375
function dt {
    docker run -ti --rm \
           -v ~/Perforce/LP-SJARVIS-OSX/depot/Smartgrid/Technical/Software:/sw \
           -e GTAFUSER=jarvis \
           cip bash --login ${@}
}

# aliased commands
alias ll="ls -l"
alias la="ls -a"
alias hd="hexdump"
alias emacsw="/Applications/Emacs.app/Contents/MacOS/Emacs &"
# vsat aliases
alias p4='p4 -zmaxScanRows=10000000 -zmaxResults=10000000 -zmaxLockTime=600000'
alias cdev='cd ~/Perforce/LP-SJARVIS-OSX/depot/Smartgrid/Technical/Software/Code/devel/'
alias cvag='cd ~/Perforce/LP-SJARVIS-OSX/depot/Smartgrid/Technical/Software/tools/vagrant/'

# key bindings
autoload -U select-word-style
select-word-style bash
bindkey '^l' backward-kill-word

# init colors, prompt
autoload -U compinit promptinit colors
compinit
promptinit
colors

export CLICOLOR=1

# shell in emacs mode
bindkey -e

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export TERM=xterm-256color
export EDITOR=emacs

# set prompt
make_prompt() {
    DIR=`pwd|sed -e "s|$HOME|~|"`;
    if [ ${#DIR} -gt 30 ]; then
      CWD="${DIR:0:12}...${DIR:${#DIR}-15}"
    else
      CWD="$DIR"
    fi
    PROMPT="%{$fg[green]%}%n@%m %{$fg[cyan]%}$CWD %{$reset_color%}%# "
    RPROMPT="[%{$fg_no_bold[yellow]%}%?%{$reset_color%}]"
}

precmd() { make_prompt; }

# Ubuntu dev
export DEBFULLNAME="Steve Jarvis"
export DEBEMAIL="sajarvis@bu.edu"
