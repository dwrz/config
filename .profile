# Set GOPATH.
export GOPATH="$HOME/.go"

# Set PATH.
PATH="$GOPATH/bin:$PATH"
PATH="$HOME/.cargo/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="$HOME/.node_modules/bin:$PATH"
export PATH

if [ "$BASH" ]; then
  if [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
  fi
fi