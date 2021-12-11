#!/bin/bash
# post.sh
# chmod u+x post.sh to set permissions.   ./post.sh to run

# move data to features
mv features*.csv features

# put files to transfer into zip files
zip -r -m features features
zip -r -m error error
zip -r -m output output

