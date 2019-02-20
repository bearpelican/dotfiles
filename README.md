dotfiles
========
follow this- https://github.com/technicalpickles/homesick


Setting up dotfiles in aws
1. Create ssh key and add to github
2. git clone git@github.com:bearpelican/dotfiles.git
3. git checkout origin/fastaiv2
4. mv .bashrc .bashrc.bak
5. ln -s dotfiles/server/.bashrc .bashrc
6. source .bashrc
7. ln -s dotfiles/home/.gitconfig .gitconfig
8. ln -s dotfiles/server/.tmux.conf .tmux.conf
9. mkdir .jupyter/nbconfig
10. ln -s dotfiles/home/notebook.json .jupyter/nbconfig/notebook.json
