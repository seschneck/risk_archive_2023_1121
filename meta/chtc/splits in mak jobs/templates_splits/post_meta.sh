#!/bin/bash
# post_meta.sh

#move data to results
mv results_*.rds meta_results

#move error files to error
mv error*.err meta_error

#move output files to output
mv output*.out meta_output

#put files to transfer into zip files
zip -r -m meta_results meta_results
zip -r -m meta_error meta_error
zip -r -m meta_output meta_output
