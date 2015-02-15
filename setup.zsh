#!/usr/bin/env zsh

[[ $PWD = ~/.dotfiles ]] || exit 1

if [[ $(uname -s) = FreeBSD ]]; then
	if [[ -x /compat/linux/bin/ln ]]; then
		LN=/compat/linux/bin/ln
	else
		echo "On FreeBSD and no GNU ln found, exiting"
		exit 1
	fi
fi
LN=${LN:-ln}

for i in dot.*; do
	${LN} -Ts .dotfiles/${i} ~/${i#dot}
done
