export GOPATH="$HOME/.go"
export npm_config_prefix=~/.node_modules

# PATH
PATH="$GOPATH/bin:$PATH"
PATH="$HOME/.cargo/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="$HOME/.node_modules/bin:$PATH"
export PATH

export HISTSIZE=1000
export HISTFILESIZE=1000

if [ "$BASH" ]; then
  if [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
  fi
fi
