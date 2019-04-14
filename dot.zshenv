# -*- shell-script -*-
export GOPATH=~/.go/
export LESS="FRX"
export PATH=${PATH/::/:}
export GPG_TTY=$(tty)
export EDITOR=emacsclient
export VISUAL=$EDITOR
path=( $HOME/.gem/ruby/*/bin(/N) $HOME/.cabal/bin(/N) $HOME/.go/bin(/N) $HOME/.cargo/bin(/N) $HOME/.local/bin(/N) $path /usr/sbin(/N) /usr/local/sbin(/N) /sbin(/N) )
typeset -xU path
