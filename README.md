dotfiles
========
follow this- https://github.com/technicalpickles/homesick


Setting up dotfiles in paperspace
1. Create ssh key and add to github
2. git clone git@github.com:bearpelican/dotfiles.git
3. git checkout origin/fastaiv2
4. ln -s dotfiles/home/.bashrc .bashrc
5. source .bashrc
6. ln -s dotfiles/home/.gitconfig .gitconfig
7. mkdir .jupyter/nbconfig
8. ln -s dotfiles/home/notebook.json .jupyter/nbconfig/notebook.json
