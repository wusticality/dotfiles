#!/bin/bash

# Load bash functions.
source "$HOME/.bash_functions"

# Setup our path.
kd_add_path_tail "$HOME/bin"
kd_add_path_tail "$HOME/.local/bin"

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
alias ls="eza --color=always"
alias ll="eza --color=always -l"
alias la="eza --color=always -la"

# du / df
alias du="du -h"
alias df="df -h"

# more / less
alias more="bat --plain"
alias less="bat --plain"

# grep
alias grep="grep --color=always"

# tar
alias tar="COPYFILE_DISABLE=true tar"

# rg
alias rg="rg --color=always"

# jq
alias jq="jq --color-output"

# # Load our ssh key into ssh-agent.
# if [ -z "$SSH_AUTH_SOCK" ] ; then
#   eval "$(ssh-agent -s)"
#   ssh-add ~/.ssh/id_ed25519
# fi

# rust
kd_execute "$HOME/.cargo/env"

# golang
if command -v go >/dev/null 2>&1; then
    kd_add_path_tail "/usr/local/go/bin"
    export GOPATH="$(go env GOPATH)"
    kd_add_path_tail $GOPATH/bin
fi

# Load platform-specific things.
kd_execute "$HOME/.bashrc_prompt"
kd_execute "$HOME/.bashrc_linux"
kd_execute "$HOME/.bashrc_macos"
kd_execute "$HOME/.bashrc_kubectl"

# fzf
if command -v fzf >/dev/null 2>&1; then
    eval "$(fzf --bash)"
fi

# mise
if command -v mise >/dev/null 2>&1; then
    eval "$(mise activate bash)"
fi

# Cleanup our final path.
kd_uniquify_path

# Setup direnv.
if command -v direnv >/dev/null 2>&1; then
    eval "$(direnv hook bash)"
fi

# Clear the screen.
clear
