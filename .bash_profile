#!/bin/bash

# Load bash functions.
source "$HOME/.bash_functions"

# Load platform-specific login config.
kd_execute "$HOME/.bash_profile_macos"

# Load our .bashrc file when logging in.
[ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
