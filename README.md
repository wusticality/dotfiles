# dotfiles

The repository for all my dotfiles. To set this up properly, you will need to clone the repo and then copy the `.git` folder into your home directory. Then set the following `git` setting:

`git config --local status.showUntrackedFiles no`

This will prevent `git` from tracking existing files unless they are explicitly added.

## Rust Analyzer

To setup `rust-analyzer` for local development, first clone the [repo](https://github.com/rust-lang/rust-analyzer) locally. Then execute the following command to install it:

`cargo xtask install --server`

Note that you will have to do this anytime you run `rustup update`.

## Getting Emacs from homebrew.

To get the jit / fast json functionality, you'll need to install Emacs through a special `brew` recipe. You can learn more about it [here](https://github.com/d12frosted/homebrew-emacs-plus):

```
brew tap d12frosted/emacs-plus
brew install emacs-plus --with-native-comp --with-modern-papirus-icon
```

Once built, you'll need to copy the `Emacs.app` directory into `/Applications`. At present, it's installed here:

`/opt/homebrew/opt/emacs-plus/Emacs.app`
