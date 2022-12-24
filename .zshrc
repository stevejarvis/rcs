# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/Users/steve/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
#ZSH_THEME="robbyrussell"
ZSH_THEME="jarvis-custom"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
 if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='emacs'
 else
   export EDITOR='emacs'
 fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

alias hd="hexdump"

alias ls="ls -G"
alias ll="ls -l"
alias la="ls -a"
alias sl="ls"

alias sshp="ssh -o PreferredAuthentications=password -o PubkeyAuthentication=no"

# add directory to path only if it doesn't already exist and is a dir
# paths are prepended
pathadd() {
    if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
        PATH="$1:${PATH+"$PATH"}"
    fi
}

pathadd /usr/local/bin
pathadd /usr/local/go/bin
pathadd /usr/local/sbin
pathadd /usr/texbin
pathadd ${HOME}/.local/bin
pathadd ${HOME}/bin
pathadd ${HOME}/go/bin
pathadd ${HOME}/google-cloud-sdk/bin
# NOTE this is probably why ppl use rvm, it'll make a mess
# with multiple versions of same lib.
for version in $(ls ${HOME}/.gem/ruby); do
    pathadd ${HOME}/.gem/ruby/${version}/bin
done
pathadd /opt/homebrew/opt/ruby/bin
export PATH

# Python version management with pyenv
export PYENV_ROOT=${HOME}/.pyenv
export PIPENV_PYTHON=${PYENV_ROOT}/shims/python
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

# And Node management, with nvm
export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && . "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

# Source local.sh for other things, if exists.
if [ -f $HOME/.local.sh ]; then
    source $HOME/.local.sh
fi

#alias ghgo='open "https://$(git remote -v | head -n1 | awk \'{print $2}\' | cut -d@ -f2 | sed -e \'s|:|/|\')"'
alias cfd=cloudflared
alias kctl=kubectl
alias kubeone="env HTTPS_PROXY=socks5://127.0.0.1:1234 kubectl"

alias tf=terraform

bindkey -e
bindkey '^[[1;9C' forward-word
bindkey '^[[1;9D' backward-word

eval "$(/opt/homebrew/bin/brew shellenv)"
