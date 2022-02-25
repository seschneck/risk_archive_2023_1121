#!/bin/bash
# post.sh

# move data to features 
# argument list too long if using mv * - have to break apart
mv features_10*.rds features
mv features_11*.rds features
mv features_12*.rds features
mv features_13*.rds features
mv features_14*.rds features
mv features_15*.rds features
mv features_16*.rds features
mv features_17*.rds features
mv features_18*.rds features
mv features_19*.rds features
mv features_20*.rds features
mv features_21*.rds features
mv features_22*.rds features
mv features_23*.rds features
mv features_24*.rds features
mv features_25*.rds features
mv features_26*.rds features
mv features_27*.rds features
mv features_28*.rds features
mv features_29*.rds features
mv features_3*.rds features
mv features_4*.rds features
mv features_5*.rds features
mv features_6*.rds features
mv features_7*.rds features
mv features_8*.rds features
mv features_9*.rds features
mv features_*.rds features # catches any missed results


# put files to transfer into zip files
zip -r -m features features
zip -r -m error error
zip -r -m output output

# move input files into input_files
mkdir input_files
mv p*.sh *.rds jobs.csv *.R sub.sub execute.sh input_files

