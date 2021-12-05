# 2021 Advent of Code Solutions


This year I started using [Nix][] and [Guix][] to manage my dev environment.
(Nix for macOS, and Guix for Linux.)
Here's how those work.

  [guix]: https://guix.gnu.org/
  [nix]: https://nixos.org/





## Guix


I haven't gotten the Ruby Gems managed by Guix.
(Yet!)
Until I solve that,
you have to install them separately
inside the Guix environment.

```
$ guix shell -m manifest.scm
$ gem install --user-install immutable-ruby  # First time only
$ export GEM_PATH=$HOME/.local/share/gem/ruby/3.0.0:$GEM_PATH
```





## Nix


Both Ruby and the Gems are managed by Nix,
so it's one command to be up and running:

```
$ nix shell
```
