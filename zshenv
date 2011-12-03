. ~/.zsh/aliases
. ~/.zsh/completion
. ~/.zsh/config
. ~/.zsh/prompt

export SYSTEM_SCRIPTS=~/bt/system-scripts
. ~/bt/system-scripts/pairing_stations/ec2env

export PATH="/usr/local/bin:$PATH"
export PATH="/usr/local/ruby186_p383/bin:/opt/local/bin:/opt/local/sbin:$PATH"
export PATH="/opt/local/lib/mysql5/bin:$PATH"
export PATH="/usr/local/ruby/bin:$PATH"
export PATH="/opt/local/lib/postgresql84/bin:$PATH"
export PATH="$PATH:/opt/local/pear/bin"
export PATH="$PATH:$SYSTEM_SCRIPTS/bin"
export PATH="$PATH:$EC2_HOME/bin:$EC2_AMI_HOME/bin"
export MANPATH=/opt/local/share/man:$MANPATH

_rake () {
  if [ -f Rakefile ]; then
    compadd `rake --silent --tasks | cut -d " " -f 2`
  fi
}

compdef _rake rake

_cap () {
  if [ -f Capfile ]; then
    compadd `cap -vT | grep '^cap' | cut -d ' ' -f 2`
  fi
}

compdef _cap cap

source ~/bt/system-scripts/pairing_stations/aliases
eval `ssh-agent`
