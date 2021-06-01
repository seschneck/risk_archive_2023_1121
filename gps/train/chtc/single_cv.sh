#!/bin/bash
#single_cv.sh

#untar R installation
tar -xzf SLIBS.tar.gz 
tar -xzf R361.tar.gz
tar -xzf risk_packages.tar.gz

#use that R installation along with pooled packages
export PATH=$PWD/R/bin:$PATH
export RHOME=$PWD/R
export R_LIBS=$PWD/packages
export LD_LIBRARY_PATH=$(pwd)/SS:$LD_LIBRARY_PATH

#run R script, passing in job as arg
Rscript single_cv.R $1
