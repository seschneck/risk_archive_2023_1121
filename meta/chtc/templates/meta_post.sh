#!/bin/bash
# POST_match.sh

#move data to results
mv results_*.rds match_results

#move error files to error
mv error*.err match_error

#move output files to output
mv output*.out match_output

#put files to transfer into zip files
zip -r -m match_results match_results
zip -r -m match_error match_error
zip -r -m match_output match_output
