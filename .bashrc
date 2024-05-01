#!/bin/bash

# Load bash functions.
source "$HOME/.bash_functions"

# Setup our path.
kd_add_path_tail "$HOME/bin"

# Check window size after each command.
shopt -s checkwinsize

# History settings.
export HISTFILE=$HOME/.history
export HISTCONTROL=ignoredups:erasedups
export HISTSIZE=100000
export HISTFILESIZE=100000

# Append to history, don't overwrite it.
shopt -s histappend
export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"

# Make rm, cp, mv safe.
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"

# use exa instead of ls
alias ls="exa --color=always"
alias ll="exa --color=always -l"
alias la="exa --color=always -la"

# du / df
alias du="du -h"
alias df="df -h"

# more / less
alias more="less -R"
alias less="less -R"

# grep
alias grep="grep --color=always"

# tar
alias tar="COPYFILE_DISABLE=true tar"

# rg
alias rg="rg --color=always"

# jq
alias jq="jq --color-output"

# fzf
kd_execute "$HOME/.fzf.bash"

# Load our ssh key into ssh-agent.
if [ -z "$SSH_AUTH_SOCK" ] ; then
  eval "$(ssh-agent -s)"
  ssh-add ~/.ssh/id_ed25519
fi

# rust
kd_execute "$HOME/.cargo/env"

# golang
kd_add_path_tail "/usr/local/go/bin"
export GOPATH="$(go env GOPATH)"
kd_add_path_tail $GOPATH/bin

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# aws-vault
export AWS_VAULT_KEYCHAIN_NAME="login"

# Load platform-specific things.
kd_execute "$HOME/.bashrc_prompt"
kd_execute "$HOME/.bashrc_linux"
kd_execute "$HOME/.bashrc_macos"
kd_execute "$HOME/.bashrc_kubectl"

# Cleanup our final path.
kd_uniquify_path

# Setup direnv.
eval "$(direnv hook bash)"

# Start in your home directory.
cd $HOME

# Clear the screen.
clear
