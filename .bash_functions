#!/bin/bash

# Add a path if it exists.
kd_add_path () {
    [ -d $1 ] && export PATH=$PATH:$1
}

# Execute a file if it exists.
kd_execute () {
    [ -r $1 ] && . $1
}

# Load platform-specific functions.
kd_execute "$HOME/.bash_functions_linux"
