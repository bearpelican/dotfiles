#! bin/bash
sudo apt-get update -y
sudo apt-get -o Dpkg::Options::=--force-confdef -o Dpkg::Options::=--force-confnew upgrade -y
sudo apt-get install emacs -y
sudo apt-get autoremove -y
sudo apt-get autoclean -y

# link bashrc things
git clone https://github.com/bearpelican/dotfiles.git
mv ~/.bashrc ~/.bashrc.bak
ln -s ~/dotfiles/home/server/.bashrc ~/.bashrc
source ~/.bashrc
ln -bfs ~/dotfiles/home/.gitconfig ~/.gitconfig
mkdir -p .jupyter/nbconfig
ln -bfs ~/dotfiles/home/notebook.json ~/.jupyter/nbconfig/notebook.json
ln -bfs ~/dotfiles/home/server/.tmux.conf ~/.tmux.conf
tmux source-file ~/.tmux.conf

# generate keygen for ssh
if [ ! -f ~/.ssh/id_rsa.pub ]; then
    yes "" | ssh-keygen -t rsa -N ""
    echo "SSH Key generated. Please enter this into github"
    cat ~/.ssh/id_rsa.pub
fi

# install anaconda
curl -O https://repo.anaconda.com/archive/Anaconda3-5.2.0-Linux-x86_64.sh
bash Anaconda3-5.0.1-Linux-x86_64.sh -b

# update conda and python
conda update -n base conda -y
pip install --upgrade pip
conda install python -y


# install nvidia
sudo apt-get purge nvidia* -y
sudo apt-get autoremove -y
sudo apt-get autoclean -y
sudo rm -rf /usr/local/cuda*

sudo apt-key adv --fetch-keys http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64/7fa2af80.pub
echo "deb https://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64 /" | sudo tee /etc/apt/sources.list.d/cuda.list

sudo apt-get update  -y
sudo apt-get -o Dpkg::Options::="--force-overwrite" install cuda-9-2 cuda-drivers -y

sudo ldconfig
nvidia-smi

# MAY NEED TO REBOOT COMPUTER HERE
sudo reboot