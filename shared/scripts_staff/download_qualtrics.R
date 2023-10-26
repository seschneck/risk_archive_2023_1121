# download_qualtrics
# Run once at study end to download session forms, feedback, and 
# other Risk qualtrics survey data 


# Libraries & paths
raw_path <- "P:/StudyData/RISK/raw_data/qualtrics"
library(StudySupport)
library(tidyverse)

## Enrollment --------------------------------------

# Enrollment Database-RISK,
d_ED <- apiGetSurvey (SurveyID = 'SV_er3BuWp5ZLOjAzP', UseLabels = FALSE ) 
write_csv(d_ED, file.path(raw_path, 'enrollment_database_rawAPI.csv'))

# Enrollment Database-CLEAN,
d_ED_clean <- edbGetCleanDB(SurveyID = 'SV_er3BuWp5ZLOjAzP') 
write_csv(d_ED_clean, file.path(raw_path, 'enrollment_database_clean.csv'))


## Phone Screening --------------------------------------

# Phone Screen Battery - RISK (04/13/2018-09/10/2019), 
d_PSB1_SF <- apiGetSurvey (SurveyID = 'SV_bKHIZeF1RBMTSXr', UseLabels = FALSE )
write_csv(d_PSB1_SF, file.path(raw_path, 'phone_screen_4.csv'))

# Phone Screen Battery (03/27/2018-06/07/2018)-RISK, 
d_PSB2_SF <- apiGetSurvey (SurveyID = 'SV_9EwFgEzJrMj6Uap', UseLabels = FALSE )
write_csv(d_PSB2_SF, file.path(raw_path, 'phone_screen_3.csv'))

# Phone Screen Battery (09/14/2017-04/30/2018)-RISK,
d_PSB3_SF <- apiGetSurvey (SurveyID = 'SV_bE1Hc7S4qX04FZH', UseLabels = FALSE )
write_csv(d_PSB3_SF, file.path(raw_path, 'phone_screen_2.csv'))

# Phone Screen Battery (01/19/2017-08/03/2017)-RISK, 
d_PSB4_SF <- apiGetSurvey (SurveyID = 'SV_50dTQxEbednaSWh', UseLabels = FALSE )
write_csv(d_PSB4_SF, file.path(raw_path, 'phone_screen_1.csv'))


## Session Forms --------------------------------------
# Screening
# Screen SOP/Session Form (01/17/2018-09/19/2018) - RISK, 
d_SSF1_ <- apiGetSurvey(SurveyID='SV_8dHQWRHtYVPXjvf', UseLabels = FALSE)
write_csv(d_SSF1_, file.path(raw_path, 'session_screen_3.csv'))

# Session Form Screen Battery (10/09/2017-11/22/2017) - RISK, 
d_SSF2 <- apiGetSurvey(SurveyID='SV_9nLLkVXQa8Fr7SJ', UseLabels = FALSE)
write_csv(d_SSF2, file.path(raw_path, 'session_screen_2.csv'))

#Session Form Screen Battery (12/02/2017-10/04/2017) - RISK, 
d_SSF3 <- apiGetSurvey(SurveyID='SV_eQIxBIm5yNmza4J', UseLabels = FALSE)
write_csv(d_SSF3, file.path(raw_path, 'session_screen_1.csv'))


# Intake
#Session Form Intake Battery (02/02/2017-10/04/2017)-RISK,
d_ISF1 <- apiGetSurvey(SurveyID='SV_3VmIJvblYsSRLFj', UseLabels = FALSE)
write_csv(d_ISF1, file.path(raw_path, 'session_intake_3.csv'))

#Session Form Intake Battery (10/14/2017-03/01/2018)-RISK,
d_ISF2 <- apiGetSurvey(SurveyID='SV_9Z7jcYTueFJhXNz', UseLabels = FALSE)
write_csv(d_ISF2, file.path(raw_path, 'session_intake_2.csv'))

#Intake SOP/Session Form (02/28/2018-09/25/2019)-RISK,
d_ISF3 <- apiGetSurvey(SurveyID='SV_bKOWdAaAvbYDZjL', UseLabels = FALSE)
write_csv(d_ISF3, file.path(raw_path, 'session_intake_1.csv'))


# Followup 1&2
#Session Form Followup 1 & 2 Battery (03/31/2017-10/02/2017)-RISK,
d_SFF1 <- apiGetSurvey(SurveyID='SV_3sZaisKdLIzqrg9', UseLabels = FALSE)
write_csv(d_SFF1, file.path(raw_path, 'session_followup12_3.csv'))

#Session Form Followup 1 & 2 Battery (11/01/2017-05/01/2018)- RISK,
d_SFF2 <- apiGetSurvey(SurveyID='SV_25cfIkcmAg5CqkR', UseLabels = FALSE)
write_csv(d_SFF2, file.path(raw_path, 'session_followup12_2.csv'))

#Follow Up 1 & 2 SOP/Session Form (05/01/2018-11/07/2019) - RISK,
d_SFF3 <- apiGetSurvey(SurveyID='SV_0TYHA6ZZJIIG2EZ', UseLabels = FALSE)
write_csv(d_SFF3, file.path(raw_path, 'session_followup12_1.csv'))

#Not sure why but there is no data here. Perhaps it was created but never used (date overlaps _3 above). I reordered the other items.
#Session Form Followup 1 & 2 Battery (09/29/2017)- RISK,
# d_SFF4 <- apiGetSurvey(SurveyID='SV_5cMpUmQsiX7z8Ff', UseLabels = FALSE)
# write_csv(d_SFF4, file.path(raw_path, 'session_followup12_0.csv'))


# Followup 3
#Session Form Followup (06/05/2017) 3-RISK,
d_FinalSF1 <- apiGetSurvey(SurveyID='SV_7NWbgwJi9HraNed', UseLabels = FALSE)
write_csv(d_FinalSF1, file.path(raw_path, 'session_followup3_4.csv'))

#Session Form Followup 3 Battery (09/15/2017-10/04/2017)-RISK,
d_FinalSF2 <- apiGetSurvey(SurveyID='SV_3OZHtzKM3DobAMZ', UseLabels = FALSE)
write_csv(d_FinalSF2, file.path(raw_path, 'session_followup3_3.csv'))

#Session Form Followup 3 Battery (10/19/2017-03/27/2018) - RISK,
d_FinalSF3 <- apiGetSurvey(SurveyID='SV_aVKJF1YIymDGxlr', UseLabels = FALSE)
write_csv(d_FinalSF3, file.path(raw_path, 'session_followup3_2.csv'))

#Follow up 3 SOP/Session Form (05/03/2018-12/05/2019) - RISK,
d_FinalSF4 <- apiGetSurvey(SurveyID='SV_cMahpnPZchotCW9', UseLabels = FALSE)
write_csv(d_FinalSF4, file.path(raw_path, 'session_followup3_1.csv'))






## EMA --------------------------------------
# EMA Morning (EMAM) Battery-RISK	
d_EMAM <- apiGetSurvey (SurveyID = 'SV_bBGKAzNQeyXuN6Z', UseLabels = FALSE )
write_csv(d_EMAM, file.path(raw_path, 'ema_morning.csv'))
#
# EMA Later (EMAL) Battery-RISK	
d_EMAL <- apiGetSurvey (SurveyID = 'SV_aViOZNT66b7wff7', UseLabels = FALSE )
write_csv(d_EMAL, file.path(raw_path, 'ema_later.csv'))

# Sleep Schedule Battery - RISK	
d_Sleep <- apiGetSurvey (SurveyID = 'SV_exqjLqg1OohSqvX', UseLabels = FALSE )
write_csv(d_Sleep, file.path(raw_path, 'sleep_schedule.csv'))



## Feedback --------------------------------------
# Feedback Battery - RISK, 
d_FB <- apiGetSurvey (SurveyID = 'SV_3mda25xsdUcADNr', UseLabels = FALSE )
write_csv(d_FB, file.path(raw_path, 'feedback.csv'))

