export PATH=/usr/local/bin:$PATH:/Users/steve/bin:/Users/sjarvis/bin:/usr/texbin

if [ -f "$HOME/.p4c" ]; then
    export P4CONFIG=$HOME/.p4c
fi

function prompt {

    EXITSTATUS="$?"
    BOLD="\[\033[1m\]"
    RED="\[\033[1;31m\]"
    YELLOW="\[\033[1;33m\]"
    GREEN="\[\e[1;32m\]"
    BLUE="\[\e[1;34m\]"
    OFF="\[\033[m\]"

    PROMPT="${GREEN}\u@\h ${BLUE}\W${OFF}\n"

    #PS1 orignally was: \h:\W \u\$
    PS1="${GREEN}\u@\h ${YELLOW}\w\n"
    if [ "${EXITSTATUS}" -eq 0 ]
    then
        PS1=$PS1"${GREEN}\$ ${OFF}"
    else
        PS1=$PS1"${RED}\$ ${OFF}"
    fi

    PS2="${BOLD}>${OFF} "
}

PROMPT_COMMAND=prompt

alias ll="ls -l"
alias la="ls -a"
alias hd="hexdump"
alias emacsw="/Applications/Emacs.app/Contents/MacOS/Emacs &"
alias p4='p4 -zmaxScanRows=10000000 -zmaxResults=10000000 -zmaxLockTime=600000'

export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced
