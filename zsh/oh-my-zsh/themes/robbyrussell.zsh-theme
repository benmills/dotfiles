PROMPT='
[ %n@%{$fg[white]%}%M %{$fg[cyan]%}%/ %{$fg_bold[blue]%}$(git_prompt_info)%{$reset_color%}]
%{$fg[yellow]%}>%{$reset_color%} '

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[yellow]%}%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%} "

