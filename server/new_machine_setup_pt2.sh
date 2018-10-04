#! bin/bash
# install anaconda
# installs pytorch from source into env pytorch_source


# install anaconda
CONDA_SH=Anaconda3-5.2.0-Linux-x86_64.sh
curl -O https://repo.anaconda.com/archive/$CONDA_SH
bash $CONDA_SH -b
rm $CONDA_SH
. ~/anaconda3/etc/profile.d/conda.sh

# update conda and python
conda update -n base conda -y
~/anaconda3/bin/pip install --upgrade pip
conda install python -y


# Create pytorch source environment
ENV=pytorch_source
ENV_BIN=~/anaconda3/envs/$ENV/bin
conda create -n $ENV -y
conda install --name $ENV python
conda install --name $ENV numpy pyyaml mkl mkl-include setuptools cmake cffi typing -y
conda install --name $ENV -c mingfeima mkldnn -y
conda install --name $ENV -c pytorch magma-cuda90 -y


# Download and install pytorch source
git clone --recursive https://github.com/pytorch/pytorch.git
pushd ~/pytorch
CUDA_HOME=/usr/local/cuda NCCL_ROOT_DIR=/usr/local/cuda/targets/x86_64-linux/ NCCL_LIB_DIR=/usr/local/cuda/targets/x86_64-linux/lib NCCL_INCLUDE_DIR=/usr/local/cuda/targets/x86_64-linux/include $ENV_BIN/python setup.py install
popd

# Install rest of dependencies
PIP_ENV=$ENV_BIN/pip
$PIP_ENV install torchvision torchtext
conda install --name $ENV jupyter bcolz scipy tqdm -y
$PIP_ENV install sklearn-pandas
$PIP_ENV install tensorboardX

# THIS VERSION IS SLOWER THAN JUST INSTALLING FROM PIP BELOW...
# sudo apt install yasm
# https://gist.github.com/soumith/01da3874bf014d8a8c53406c2b95d56b
# seems like it's faster without the top version
$PIP_ENV uninstall pillow -y
sudo apt-get install libjpeg-turbo8-dev
CC="cc -mavx2" $PIP_ENV install -U --force-reinstall pillow-simd



# Create fastai environment
conda create -n fastai --clone pytorch_source
FASTAI_PIP_ENV=~/anaconda3/envs/fastai/bin/pip
# FASTAI_PIP_ENV install opencv-python isoweek pandas-summary seaborn graphviz
# conda install seaborn python-graphviz -y
git clone https://github.com/fastai/fastai.git
pushd fastai
tools/run-after-git-clone
FASTAI_PIP_ENV install -e .
popd fastai

