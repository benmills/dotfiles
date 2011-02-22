. ~/.zsh/aliases
. ~/.zsh/completion

# Oh my zsh!
export ZSH=$HOME/.zsh/oh-my-zsh
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="robbyrussell"
plugins=(git)
source $ZSH/oh-my-zsh.sh

#Path
export PATH=/opt/local/bin:/opt/local/sbin:$PATH:$HOME/.bin
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"  # This loads RVM into a shell session.

#Source
source ~/.bin/bashmarks.sh

#Config
fpath=(~/.zsh/functions $fpath)
autoload -U ~/.zsh/functions/*(:t)

setopt NO_BG_NICE # don't nice background tasks
setopt NO_HUP
setopt NO_LIST_BEEP
setopt LOCAL_OPTIONS # allow functions to have local options
setopt LOCAL_TRAPS # allow functions to have local traps
setopt HIST_VERIFY
setopt SHARE_HISTORY # share history between sessions ???
setopt EXTENDED_HISTORY # add timestamps to history
setopt PROMPT_SUBST
setopt CORRECT
setopt COMPLETE_IN_WORD
setopt IGNORE_EOF

setopt APPEND_HISTORY # adds history
setopt INC_APPEND_HISTORY SHARE_HISTORY  # adds history incrementally and share it across sessions
setopt HIST_IGNORE_ALL_DUPS  # don't record dupes in history
setopt HIST_REDUCE_BLANKS

