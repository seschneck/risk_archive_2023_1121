# Version 1
Version run for CBIT presentation.    

- Calculated for all three windows, and lead = 0. 
- There were many issues with the features that needed to be corrected.  See next version

# Version 2

Run for meeting with Nowak group
- Only run for 1day features at this point
- removed item 1 from max and min features.   Handled by lapse counting

- removed 12, and 24 hour windows for items 8-10.  Were high missing
- recognized that the missing rate is 13% for items 1-7 for 12 hour window.  OK?  Wait to see if it ends up in any models
- recognized that the missing rate is 9% for items 8-10 for 48 hour window.  OK?  Wait to see if it ends up in any models
- recognized that the missing rate is 6% for items 8-10 for 72 hour window.  OK?  Wait to see if it ends up in any models
- recognized that the missing rate is 5% for items 1-7 for 12 hour window.  OK?  Wait to see if it ends up in any models
- recognized that the missing rate is 3% for items 1-7 for 12 hour window.  OK?  Wait to see if it ends up in any models
- Added a ratecount of completed emas as features

# Version 3

First version after Nowak group
- Remove proportion scores
- Only using "all" feature set (now without proportions)
- Added back smote_1
- Updated score_min and score_max to use mean baseline (rather than min/max of baseline)
- Added score_most_recent for items 2-10
- Added feature for day of week for label being predicted

Only use all features set
? p12.l0.dratecount.count.lapse	 missing for p12 and all other durations for 142 observations?  Not all subjects but first observation for most?