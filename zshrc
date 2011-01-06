. ~/.zsh/aliases
. ~/.zsh/completion

# Oh my zsh!
export ZSH=$HOME/.zsh/oh-my-zsh
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="robbyrussell"
plugins=(git)
source $ZSH/oh-my-zsh.sh

#Path
export PATH=/opt/local/bin:/opt/local/sbin:$PATH

#Config
fpath=(~/.zsh/functions $fpath)
autoload -U ~/.zsh/functions/*(:t)
