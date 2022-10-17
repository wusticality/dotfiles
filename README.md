# dotfiles

The repository for all my dotfiles. To set this up properly, you will need to clone the repo and then copy the `.git` folder into your home directory. Then set the following `git` setting:

`git config --local status.showUntrackedFiles no`

This will prevent `git` from tracking existing files unless they are explicitly added.

## Rust Analyzer

To setup `rust-analyzer` for local development, first clone the [repo](https://github.com/rust-lang/rust-analyzer) locally. Then execute the following command to install it:

`cargo xtask install --server`

Note that you will have to do this anytime you run `rustup update`.

## Setting up the keyboard.

Because `xmonad` uses left alt as the modifier key, and because it's more natural to use the left super key as meta, we need to swap these two keys (on both sides of the spacebar). This has proved to be very difficult to figure out. My first attempt was to use `xmodmap`, but my settings were constantly being forgotten. I then tried adding this to my `xmonad` startup hook:

```
spawnOnce "setxkbmap -option altwin:swap_alt_win"
```

This worked _most_ of the time, but sometimes it would stop working. The final solution was to add these `setxkbmap` settings to `/etc/default/keyboard`:

```
XKBOPTIONS="altwin:swap_alt_win"
```

This will cause these settings to persist in the event of unwelcomed changes.
