#!/usr/bin/env zsh

[[ $PWD = ~/.dotfiles ]] || exit 1

for i in dot.*; do
	ln -Ts .dotfiles/${i} ~/${i#dot}
done
