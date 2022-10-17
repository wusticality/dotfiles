#!/bin/bash

# Do nothing if not interactive.
# case $- in
#     *i*) ;;
#     *) return;;
# esac

# Add a path if it exists.
kd_add_path () {
    [ -d $1 ] && export PATH=$PATH:$1
}

# Execute file if it exists.
kd_execute () {
    [ -r $1 ] && . $1
}

# Clear our path.
unset PATH

# Setup our path.
kd_add_path "$HOME/bin"
kd_add_path "/opt/homebrew/bin"
kd_add_path "/usr/local/bin"
kd_add_path "/usr/local/sbin"
kd_add_path "/usr/bin"
kd_add_path "/usr/sbin"
kd_add_path "/bin"
kd_add_path "/sbin"

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

# Set the prompt.
export PS1="\[\033]0;\u@\h \w\007\]\[\033[01;33m\][\[\033[00m\]\[\033[01;32m\]\u@\h\[\033[00m\] \[\033[01;31m\]\w\[\033[00m\]\[\033[01;33m\]]\[\033[00m\]\n\[\033[01;31m\]$ \[\033[00m\]"

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

# golang
kd_add_path "/usr/local/go/bin"
export GOPATH="$(go env GOPATH)"
kd_add_path $GOPATH/bin

# rust
kd_execute "$HOME/.cargo/env"

# Setup platform-specific things.
kd_execute "$HOME/.bashrc_linux"
kd_execute "$HOME/.bashrc_styra"

# Start in your home directory.
cd $HOME

# Clear the screen.
clear
