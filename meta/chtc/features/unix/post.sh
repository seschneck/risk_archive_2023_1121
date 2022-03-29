#!/bin/bash
# post.sh

# move data to features 
mv features_*.rds features 


# move input files into input_files
mkdir input_files
mv p*.sh *.rds jobs.csv *.R sub.sub execute.sh input_files

