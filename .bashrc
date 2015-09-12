# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

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
export CCSROOT=${HOME}/Perforce/LP-SJARVIS-OSX
export TECHNICAL=${CCSROOT}/Smartgrid/Technical
export CCSUSER=${CCSROOT}/Smartgrid/Users
export CODE=${TECHNICAL}/Software/Code
export DEVEL=${CODE}/devel/

alias cdev="cd ${DEVEL}"
alias cuser="cd ${CCSUSER}/sjarvis"

alias main="source /share/ccsenv/ccsenv main"
alias devel="source /share/ccsenv/ccsenv devel"
alias p4='p4 -zmaxScanRows=10000000 -zmaxResults=10000000 -zmaxLockTime=600000'
alias hd='hexdump'

alias ls="ls -G"
alias ll="ls -l"
alias la="ls -a"
alias sl="ls"
alias grep="grep --color=auto"

export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced

# just for now...
export GTAFUSER=jarvis

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
    ln -f ${HOME}/rcs/.emacs ${HOME}/.emacs.d/init.el
}

# docker
# could auto-start, but don't want to. at least subsequent shells
# will be ready.
if [ `docker-machine status default` == "Running" ]
then
    eval $(docker-machine env default)
fi

# create the image if not exist or updated
function build_tnp_centos {
    cd ${DEVEL}/tools/dev-tools/ccs-centos7-build
    docker build -f Dockerfile -t tnp-centos .
    cd -
}

# i named the instance "ash"...
function ash {
    tnp_centos_name="ash"
    docker run -ti \
           --volume=${CCSUSER}/sjarvis/dockerHome:/root/ \
           --volume=${CCSROOT}:/root/cip \
           --volume=${HOME}/.emacs.d:/root/.emacs.d/ \
           --name=${tnp_centos_name} \
           --workdir=/root \
           --rm \
           tnp-centos \
           bash
}

# utility function for cleaning up unnamed docker containers
function docker_rmia {
    docker rmi $(docker images | grep '^<none>' | awk '{print $3}')
}

# Ubuntu dev
export DEBFULLNAME="Steve Jarvis"
export DEBEMAIL="sajarvis@bu.edu"# path

export PATH=${HOME}/bin:/usr/local/bin:/usr/local/sbin:/usr/texbin:${DEVEL}/tools/python/atlas:${DEVEL}/../builds/scripts:${CCSROOT}Smartgrid/Users/sjarvis/bin:$PATH
