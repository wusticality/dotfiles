#!/bin/bash

# Load bash functions.
source "$HOME/.bash_functions"

# Load platform-specific login scripts.
kd_execute "$HOME/.bash_profile_linux"

# Load our .bashrc file when logging in.
[ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
