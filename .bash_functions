#!/bin/bash

# Add a path before all other paths if it exists.
kd_add_path_head() {
    if [[ -d $1 ]] && [[ ! $PATH =~ (^|:)"$1"(:|$) ]]; then
        export PATH="$1:$PATH"
    fi
}

# Add a path after all other paths if it exists.
kd_add_path_tail() {
    if [[ -d $1 ]] && [[ ! $PATH =~ (^|:)"$1"(:|$) ]]; then
        export PATH="$PATH:$1"
    fi
}

# Remove duplicate entries in PATH.
kd_uniquify_path() {
    export PATH=$(echo -n $PATH | awk -v RS=: -v ORS=: '!($0 in a) {a[$0]; print}' | sed 's/:$//')
}

# Execute a file if it exists.
kd_execute() {
    [ -r $1 ] && . $1
}

# Load platform-specific functions.
kd_execute "$HOME/.bash_functions_linux"
