#!/bin/bash

# Load common x settings.
[ -f "$HOME/.xcommon" ] && source "$HOME/.xcommon"

# Start xmonad.
exec xmonad
