# -*- shell-script -*-
export GOPATH=~/src/go
export LESS="FRX"
export PATH=${PATH/::/:}
export GPG_TTY=$(tty)
export EDITOR=emacsclient
export VISUAL=$EDITOR
path=(
    ~/Library/Python/*/bin(/N)
    ~/.gem/ruby/*/bin(/N)
    ~/.cabal/bin(/N)
    $GOPATH/bin(/N)
    ~/.cargo/bin(/N)
    ~/.poetry/bin(/N)
    ~/.local/bin(/N)
    /usr/local/bin(/N)
    /usr/local/sbin(/N)
    /usr/sbin(/N)
    /sbin(/N)
    $path
)
manpath=(
    ~/.local/share/man(/N)
    $manpath
)
fpath=(
    ~/.local/share/zsh(/N)
    $fpath
)
typeset -xU path manpath fpath
[ -S ~/.gnupg/S.gpg-agent.ssh ] && export SSH_AUTH_SOCK=~/.gnupg/S.gpg-agent.ssh
[ -S /run/user/$EUID/gnupg/S.gpg-agent.ssh ] && export SSH_AUTH_SOCK=/run/user/$EUID/gnupg/S.gpg-agent.ssh
(( $+commands[rbenv] )) && eval "$(rbenv init -)"
