autoload -U compinit promptinit
compinit
promptinit

autoload -U colors && colors
PROMPT="%{$fg[green]%}%n%{$reset_color%}%{$fg[yellow]%} in %{$reset_color%}%{$fg_no_bold[cyan]%}%1~ %{$reset_color%}%# "
RPROMPT="%{$fg_no_bold[yellow]%}%?%{$reset_color%}"
