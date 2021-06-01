#!/bin/bash
# post.sh

#zip files
zip -r -m results results*.rds
zip -r -m out output*.out
zip -r -m err error*.err

#to run type:
#chmod +x post.sh
#  ./post.sh
