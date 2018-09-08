#! bin/bash

# Install cudnn 7.2.1
CUDNN_FILE=cudnn-9.2-linux-x64-v7.2.1.38.tgz
wget https://s3-us-west-2.amazonaws.com/ashaw-fastai-imagenet/$CUDNN_FILE
tar -xf $CUDNN_FILE
sudo cp -R ~/cuda/include/* /usr/local/cuda-9.2/include
sudo cp -R ~/cuda/lib64/* /usr/local/cuda-9.2/lib64
rm $CUDNN_FILE
rm -rf ~/cuda

# Install nccl 2.2.13 - might not need this
wget https://s3-us-west-2.amazonaws.com/ashaw-fastai-imagenet/nccl_2.2.13-1%2Bcuda9.2_x86_64.txz
tar -xf nccl_2.2.13-1+cuda9.2_x86_64.txz
sudo cp -R ~/nccl_2.2.13-1+cuda9.2_x86_64/* /usr/local/cuda-9.2/targets/x86_64-linux/
# sudo cp -R ~/nccl_2.2.13-1+cuda9.2_x86_64/* /lib/nccl/cuda-9.2
sudo ldconfig
rm nccl_2.2.13-1+cuda9.2_x86_64.txz
rm -rf nccl_2.2.13-1+cuda9.2_x86_64

sudo apt-get install libcupti-dev

# Create pytorch source environment
ENV=pytorch_source
conda create -n $ENV -y
conda install --name $ENV numpy pyyaml mkl mkl-include setuptools cmake cffi typing -y
conda install --name $ENV -c mingfeima mkldnn -y
conda install --name $ENV -c pytorch magma-cuda90 -y


# Download and install pytorch source
git clone --recursive https://github.com/pytorch/pytorch.git
cd ~/pytorch
pushd ~/pytorch
CUDA_HOME=/usr/local/cuda NCCL_ROOT_DIR=/usr/local/cuda/targets/x86_64-linux/ NCCL_LIB_DIR=/usr/local/cuda/targets/x86_64-linux/lib NCCL_INCLUDE_DIR=/usr/local/cuda/targets/x86_64-linux/include python setup.py install
popd

# Install rest of dependencies
PIP_ENV=~/anaconda3/envs/$ENV/bin/pip
$PIP_ENV install torchvision torchtext
conda install --name $ENV jupyter bcolz scipy tqdm -y
$PIP_ENV install sklearn-pandas
$PIP_ENV install tensorboardX

# THIS VERSION IS SLOWER THAN JUST INSTALLING FROM PIP BELOW...
# Install libjpeg-turbo with pillow-simd
# sudo apt install yasm
# https://gist.github.com/soumith/01da3874bf014d8a8c53406c2b95d56b
# seems like it's faster without the top versioncond
$PIP_ENV uninstall pillow -y
CC="cc -mavx2" $PIP_ENV install -U --force-reinstall pillow-simd



# Create fastai environment
conda create -n fastai_v1 --clone pytorch_source
FASTAI_PIP_ENV=~/anaconda3/envs/fastai_v1/bin/pip
FASTAI_PIP_ENV install opencv-python isoweek pandas-summary seaborn graphviz
# conda install seaborn python-graphviz -y
git clone https://github.com/fastai/fastai_v1.git
ln -s ~/fastai/fastai_v1 ~/anaconda3/envs/fastai_v1/lib/python3.7/site-packages

