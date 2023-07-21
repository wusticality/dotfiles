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

## Building Emacs from source.

You'll have to build Emacs from source to get the jit / fast json functionality:

```
git clone https://git.savannah.gnu.org/git/emacs.git
cd emacs
./autogen.sh
./configure --without-pop \
    --without-imagemagick \
    --without-compress-install \
    --without-dbus \
    --without-xwidgets \
    --without-gconf \
    --without-gsettings \
    --without-sound \
    --with-gnutls \
    --with-mailutils \
    --with-native-compilation=aot \
    --with-json \
    --with-modules \
    --with-harfbuzz \
    --with-threads \
    --with-x \
    CC="gcc-12" \
    CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer"
make NATIVE_FULL_AOT=1 -j$(nproc)
sudo make install
```

You can install dependencies by looking at [this](https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation) article.

## Enabling ssh access.

To allow ssh connections, do something like [this](https://linuxhint.com/enable-ssh-server-pop-os/).

## Fixing play / pause.

You will need to install `playerctl` to enable play / pause functionality:

```
sudo apt install playerctl
```
