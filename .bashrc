# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# source functions file
[ -f ${HOME}/rcs/functions.sh ] && source ${HOME}/rcs/functions.sh

# don't put duplicate lines in the history. See bash(1) for more options
HISTCONTROL=ignoredups

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

export EDITOR=$(which emacs)

# set dev specific locations
export P4CONFIG=${HOME}/.p4c

alias p4="p4 -zmaxScanRows=10000000 -zmaxResults=10000000 -zmaxLockTime=600000"
alias hd="hexdump"

alias ls="ls -G"
alias ll="ls -l"
alias la="ls -a"
alias sl="ls"
alias grep="grep --color=auto"
# force password, avoid "too many authentication failures"
alias sshp="ssh -o PreferredAuthentications=password -o PubkeyAuthentication=no"

export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced

# make the prompt
function prompt {

    EXITSTATUS="$?"
    BOLD="\[\033[1m\]"
    RED="\[\033[1;31m\]"
    YELLOW="\[\033[1;33m\]"
    GREEN="\[\e[1;32m\]"
    BLUE="\[\e[1;36m\]"
    OFF="\[\033[m\]"

    # shortened CWD
    DIR=$( pwd|sed -e "s|$HOME|~|" );
    if [ ${#DIR} -gt 30 ]; then
      CWD="${DIR:0:12}...${DIR:${#DIR}-15:${#DIR}}"
    else
      CWD="$DIR"
    fi

    PS1="${YELLOW}\u@\h ${BLUE}${CWD} "
    # prompt color based on exit status
    if [ "${EXITSTATUS}" -eq 0 ]
    then
        PS1=$PS1"${GREEN}\$ ${OFF}"
    else
        PS1=$PS1"${RED}\$ ${OFF}"
    fi

    PS2="${BOLD}>${OFF} "
}

PROMPT_COMMAND=prompt

# symlink rcs
function symlink_rcs {
    for rc in .bashrc .tmux.conf .vimrc .gitconfig .gdbinit
    do
        # assumes rcs is clones to home
        ln -fs ${HOME}/rcs/${rc} ${HOME}/${rc}
    done
    # .emacs goes elsewhere
    ln -fs ${HOME}/rcs/.emacs ${HOME}/.emacs.d/init.el
}

# docker
# could auto-start, but don't want to. at least subsequent shells
# will be ready.
if [ -n "$(which docker-machine 2>/dev/null)" ] && [ `docker-machine status defware` == "Running" ]
then
    eval $(docker-machine env defware)
fi

# Ubuntu dev
export DEBFULLNAME="Steve Jarvis"
export DEBEMAIL="sajarvis@bu.edu"# path

# add directory to path only if it doesn't already exist and is a dir
# paths are prepended
pathadd() {
    if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
        PATH="$1:${PATH+"$PATH"}"
    fi
}

pathadd /usr/local/bin
pathadd /usr/local/sbin
pathadd /usr/texbin
pathadd ${HOME}/.local/bin
pathadd ${HOME}/bin
