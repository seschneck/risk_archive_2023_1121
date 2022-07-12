#!/bin/bash
# execute.sh

#untar R installation
tar -xzf R402.tar.gz
tar -xzf SLIBS.tar.gz
tar -xzf train.tar.gz
export LD_LIBRARY_PATH=$(pwd)/SS:$LD_LIBRARY_PATH

#use that R installation
export PATH=$(pwd)/R/bin:$PATH
export RHOME=$(pwd)/R
export R_LIBS=$PWD/packages

#run R script, passing in args
Rscript mak_ema_features_chtc.R $1 $2
