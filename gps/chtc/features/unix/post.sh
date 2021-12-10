#!/bin/bash
# post.sh

# move data to features
mv features*.csv features

 #move error files to error
mv error*.err error

# move output files to output
mv output*.out output

# put files to transfer into zip files
zip -r -m features features
zip -r -m error error
zip -r -m output output

# move input files into input_files
mkdir input_files
mv p*.sh *.csv *.R sub.sub execute.sh input_files

