# Locale
set -x LC_ALL "en_US.UTF-8"
set -x LANG "en_US.UTF-8"
set -x LANGUAGE "en_US.UTF-8"

# FZF
set -x FZF_DEFAULT_COMMAND 'fd --type file --follow --hidden --exclude .git --exclude target'
set -x FZF_DEFAULT_OPTS '--ansi --preview-window "right:60%" --preview "bat --color=always --style=header,grid --line-range :300 {}"'

# Rust
set -gx PATH "$HOME/.cargo/bin" $PATH;
set -x RUST_BACKTRACE 1

# Golang
set -x GO111MODULE on
set -x GOPATH $HOME/Documents/go/gopath
set -gx PATH "$GOPATH/bin" $PATH

# NPM
set -x NPMGLOBAL $HOME/.npm-global
set -gx PATH "$NPMGLOBAL/bin" $PATH

# rbenv
set -x RBENV_SHELL fish
set -gx PATH "$HOME/.rbenv/shims" $PATH
