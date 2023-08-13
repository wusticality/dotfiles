# dotfiles

The repository for all my dotfiles. To set this up properly, you will need to clone the repo and then copy the `.git` folder into your home directory. Then set the following `git` setting:

`git config --local status.showUntrackedFiles no`

This will prevent `git` from tracking existing files unless they are explicitly added.

## Rust Analyzer

To setup `rust-analyzer` for local development, first clone the [repo](https://github.com/rust-lang/rust-analyzer) locally. Then execute the following command to install it:

`cargo xtask install --server`

Note that you will have to do this anytime you run `rustup update`.
