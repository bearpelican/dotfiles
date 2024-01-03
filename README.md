dotfiles
========
follow this- https://github.com/technicalpickles/homesick


Setting up dotfiles in aws
1. Create ssh key and add to github
2. git clone git@github.com:bearpelican/dotfiles.git
3. mv .bashrc .bashrc.bak
4. ln -s dotfiles/server/.bashrc .bashrc
5. source .bashrc
6. ln -s dotfiles/home/.gitconfig .gitconfig
7. ln -s dotfiles/server/.tmux.conf .tmux.conf
8. mkdir -p .jupyter/nbconfig
9. ln -s dotfiles/jupyter/notebook.json .jupyter/nbconfig/notebook.json


Mac Shortcuts:
Text editing: emacs shortcuts
Finder Path editing:
* Go to file path: Command + Shift + G
* Copy file path: Right click on file, Hold Option
