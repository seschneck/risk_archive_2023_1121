#!/bin/bash
# post.sh

# move data to features 
mv features_*.rds features 


# put files to transfer into zip files
# zip -r -m features features
# zip -r -m error error
# zip -r -m output output

# move input files into input_files
mkdir input_files
mv p*.sh *.rds jobs.csv *.R sub.sub execute.sh input_files

