dotfiles
========

Minimal shell + git config for macOS.

Install
-------

```sh
git clone https://github.com/bearpelican/dotfiles.git ~/dotfiles

ln -sfn ~/dotfiles/home/.zshrc     ~/.zshrc
ln -sfn ~/dotfiles/home/.gitconfig ~/.gitconfig
ln -sfn ~/dotfiles/home/.gitignore ~/.gitignore
```

Back up anything already at those paths first — `ln -sfn` overwrites.

Machine-specific settings (extra `PATH` entries, work credentials, tool
initialization) go in `~/.zshrc.local`, which `.zshrc` sources if present and
which is intentionally not tracked here.

Jupyter
-------

Only needed on machines running notebooks:

```sh
mkdir -p ~/.jupyter/nbconfig
ln -sfn ~/dotfiles/jupyter/notebook.json ~/.jupyter/nbconfig/notebook.json
```

`jupyter/jupyter_notebook_config.py` (remote server: bind 0.0.0.0, no browser)
and `jupyter/plugin.jupyterlab-settings` (JupyterLab keybindings) are copied
into place manually as needed.
