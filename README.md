# dotfiles

The repository for all my dotfiles. To set this up properly, you will need to clone the repo and then copy the `.git` folder into your home directory. Then set the following `git` setting:

`git config --local status.showUntrackedFiles no`

This will prevent `git` from tracking existing files unless they are explicitly added.

## Setting up the keyboard.

We need to swap the super and alt keys and disable caps lock. Add this file to `/etc/X11/xorg.conf.d/00-keyboard.conf`:

```
Section "InputClass"
    Identifier "apex-pro"
    MatchIsKeyboard "on"
    Option "XkbLayout" "us"
    Option "XkbModel" "pc105"
    Option "XkbOptions" "altwin:swap_alt_win,caps:none"
EndSection
```

## Packages from `pacman`

```
adobe-source-sans-fonts
alacritty
base-devel
bash-completion
bottom
clang
discord
dmenu
dosfstools
dust
efibootmgr
emacs-nativecomp
eza
firefox
flameshot
git
git-lfs
grub
j4-dmenu-desktop
jre-openjdk
mtools
nano
networkmanager
nitrogen
noto-fonts
numlockx
nvidia
nvidia-settings
openssh
os-prober
pavucontrol
pcmanfm
picom
pipewire
pipewire-pulse
playerctl
procs
ripgrep
signal-desktop
sudo
telegram-desktop
ttf-croscore
ttf-fira-code
ttf-fira-mono
ttf-fira-sans
ttf-ibm-plex
ttf-inconsolata
ttf-jetbrains-mono
ttf-liberation
ttf-liberation-mono-nerd
ttf-roboto
ttf-roboto-mono
ttf-ubuntu-font-family
vim
vulkan-tools
vulkan-validation-layers
wget
wireplumber
xclip
xdg-utils
xf86-video-vesa
xmonad
xmonad-contrib
xorg-server
xorg-xinit
xscreensaver
xterm
```

### Packages from the `AUR`

```
clion
rustrover
slack-desktop
ttf-meslo
ttf-tahoma
yay
zoom
```

### Install 1Password

https://support.1password.com/install-linux/#arch-linux

## Enabling ssh access.

To allow ssh connections, do something like [this](https://linuxhint.com/enable-ssh-server-pop-os/).

## Fixing play / pause.

You will need to install `playerctl` to enable play / pause functionality:

```
sudo apt install playerctl
```

You will also need to modify the script that launches xmonad. On my current machine, the script is here:

`/usr/share/xsessions/xmonad.desktop`

Modify the `Exec` line to be the following:

```
dbus-launch --exit-with-session xmonad-session
```

Then reboot your computer.

## Firefox

I modify the following Firefox settings (via `about:config`):

### Show http / https in address bar

Open `about:config` and set `browser.urlbar.trimURLs` to `false`.

### Disable middle mouse paste

Open `about:config` and set `middlemouse.paste` to `false`.

### Change scrollbar style

Open `about:config` and set `widget.non-native-theme.scrollbar.style` to `4`.

### Remove fullscreen animations

This is to remove fullscreen animations when pressing the `F11` key. Enter `about:config` and add a new number preference with the name `ui.prefersReducedMotion` and set its value to `1`.

## Numlock

You'll need to install the following program to enable numlock on startup:

`sudo apt install numlockx`

## Adjust the DPI

You'll need to adjust the DPI for some applications:

### Chrome

Edit `/usr/share/applications/google-chrome.desktop` and add this parameter to every `Exec` line:

`--force-device-scale-factor=2.2`

### Brave

Edit `/usr/share/applications/brave-browser.desktop` and add this parameter to every `Exec` line:

`--force-device-scale-factor=2.4`

## Windows VM

I couldn't figure out how to get secure boot and TPM 2.0 support working. At first I tried this approach:

https://adamsimpson.net/writing/windows-11-as-kvm-guest

Then I got fed up and just disabled both using this hack:article:

https://www.isumsoft.com/windows-11/install-windows-11-without-tpm-and-secure-boot.html

Once Windows is installed, you'll need to install the VirtIO guest tools within the VM:

https://fedorapeople.org/groups/virt/virtio-win/direct-downloads/latest-virtio/virtio-win-guest-tools.exe
