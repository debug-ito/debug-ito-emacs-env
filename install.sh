#!/bin/bash

thisdir=$(cd $(dirname $0) && pwd)

mkdir -p "$HOME/.emacs.d"
mkdir -p "$HOME/.emacs.d/lib"
ln -s "$thisdir/dot.emacs.el" "$HOME/.emacs.el"
ln -s "$thisdir/debugito.el" "$HOME/.emacs.d/lib/"
ln -s "$thisdir/snippets" "$HOME/.emacs.d/"
touch "$HOME/.emacs.d/custom.el"

mkdir -p "$HOME/.config/"
ln -s "$thisdir/stylish-haskell" "$HOME/.config/"
