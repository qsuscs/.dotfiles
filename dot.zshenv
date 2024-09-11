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
    ${KREW_ROOT:-$HOME/.krew}/bin
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
typeset -xU path manpath
[ -S ~/.gnupg/S.gpg-agent.ssh ] && export SSH_AUTH_SOCK=~/.gnupg/S.gpg-agent.ssh
[ -S /run/user/$EUID/gnupg/S.gpg-agent.ssh ] && export SSH_AUTH_SOCK=/run/user/$EUID/gnupg/S.gpg-agent.ssh
[ -S $XDG_RUNTIME_DIR/mpd/socket ] && export MPD_HOST=$XDG_RUNTIME_DIR/mpd/socket
(( $+commands[rbenv] )) && eval "$(rbenv init -)"
