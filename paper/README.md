# Paper

This directory contains a Typst paper draft for Concrete.

Typst is provided through the repository's nix dev shell.

To build the paper:

```sh
make paper
```

To watch for changes while editing:

```sh
XDG_CACHE_HOME=$(pwd)/.cache nix --extra-experimental-features "nix-command flakes" develop --command typst watch paper/main.typ paper/main.pdf
```
