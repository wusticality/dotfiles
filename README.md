# dotfiles

The repository for all my dotfiles. To set this up properly, you will need to clone the repo and then copy the `.git` folder into your home directory. Then set the following `git` setting:

`git config --local status.showUntrackedFiles no`

This will prevent `git` from tracking existing files unless they are explicitly added.

## Emacs

Install [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus) via Homebrew:

```
brew tap d12frosted/emacs-plus
brew install emacs-plus@30
```

Native compilation and tree-sitter are included by default in Emacs 30+. To upgrade:

```
brew upgrade emacs-plus@30
```

## tmux

Install [TPM](https://github.com/tmux-plugins/tpm) (Tmux Plugin Manager):

```
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
```

Then open tmux and press `C-x I` (capital I) to install plugins. This sets up [tmux-resurrect](https://github.com/tmux-plugins/tmux-resurrect) and [tmux-continuum](https://github.com/tmux-plugins/tmux-continuum) for session persistence across reboots.
