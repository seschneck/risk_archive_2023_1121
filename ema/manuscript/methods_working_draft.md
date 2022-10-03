# Method
## Research Transparency

## Participants
We recruited participants in early recovery (1-8 weeks of abstinence) from alcohol use disorder in Madison, Wisconsin, USA, to participate in a 3-month longitudinal study. Participants were recruited through print and targeted digital advertisements and partnerships with treatment centers. We required that participants:

1.  were 18 years of age or older,
2.  were able to write and read in English,
3.  had at least moderate alcohol use disorder (\>= 4 DSM-5 symptoms<!--^[We measured DSM-5 symptoms with a self-report survey administered to participants during the in-person screening visit.]-->),
4.  were abstinent from alcohol for at least 1 week but no longer than 2 months,
5.  were willing to use a single smartphone (their personal phone or one provided by us) while enrolled in the study.

We also excluded participants if they exhibited severe symptoms of psychosis or paranoia<!--^[Psychosis and paranoia were defined as scores greater than 2.2 or 2.8, respectively, on the psychosis or paranoia scales of the on the Symptom Checklist – 90 (SCL-90) [@derogatisSCL90OutpatientPsychiatric1973].]-->. 

One hundred ninety-two participants were eligible for enrollment. Of these participants, 191 consented to participate in the study at the screening session and 169 subsequently enrolled in the study at the enrollment visit which occurred approximately one week later. Fifteen participants discontinued prior to the first monthly follow-up visit. The remaining 154 participants provided study measures for one (N = 14), two (N = 7) or three (N = 133) months. 

We excluded data from one participant who appeared to not have a goal of abstinence during their participation (i.e., they had lapses every day on study except for one day and reported they were uncertain if their goal was abstinence on the daily EMA and monthly follow-up surveys). We also excluded data from two participants who showed evidence of careless responding (e.g., completing 2-4 EMAs within an hour and providing different responses) and unusually low compliance (e.g., only 5 EMAs completed over one month) rendering their lapse labels unusable. 

Our final sample consisted of 151 participants. Participants were mostly white (87%), roughly half were male (51%) and the mean age was 41 years (SD = 12). 

```Consider reporting other information like: Participants were on study for an average of 85 days out of the possible 90 days. All participants provided at least one month of data. Participants had endorsed using on average 4 other types of drugs (not including alcohol) over their lifetime. Additionally, participants on average scored a 9 on a self-report version of the DSM-5 symptom criteria for alcohol use disorder. Generally, scores of 2-3 are considered mild, 4-5 are considered moderate, and 6+ considered severe alcohol use disorder.```

```Discuss if we want any demographic/AUD history/mental health tables or flowchart of participant retention.```

## Procedure
Participants completed five study visits over the course of approximately three months. After an initial phone screen, participants attended an in-person screening visit where we determined eligibility, obtained informed consent, and collected self-report measures of individual differences (e.g., demographics, mental health and alcohol use history). Eligible and consented participants returned approximately one week later to enroll in the study. Three additional follow-up visits  occurred about every 30 days participants were on study. At each follow-up visit, we collected additional self-report and interview measures (e.g., Alcohol Timeline Followback).

For the entire duration on study, participants were expected to complete EMAs four times each day. Other personal sensing data streams (geolocation, cellular communications, sleep quality, and audio check-ins) were collected as part of the larger grant's aims (R01 AA024391). A full description of the procedure and data collected at each visit can be found at the study's OSF page <!--Insert link here-->. All procedures were approved by the University of Wisconsin-Madison Institutional Review Board.

## Measures
### EMA
Participants completed a brief (7-10 questions) EMA four times each day following reminders from us that were sent by text message. These text messages included a link to a Qualtrics survey, optimized for completion on their smartphone. 

All four EMAs included seven items that asked about alcohol use not yet reported, current affective state (pleasantness and arousal), greatest urge to drink alcohol since the last EMA, and any pleasant or positive events, any hassles or stressful events, and any exposure to risky situations (i.e., people, places, or things) that occurred since the last EMA. The first EMA each day asked an additional three questions about how likely participants were to encounter a risky situation, encounter a stressful event, and drink alcohol in the upcoming week. 

The first and last EMAs of the day were scheduled within one hour of participants' typical wake and sleep times. The other two EMAs were each scheduled randomly within the first and second halves of the participants' typical day. All EMAs were separated from each other by at least one hour.

### Lapses
<!--citation for validity of self-reported alcohol use: https://pubmed.ncbi.nlm.nih.gov/26160523/-->

Lapses were derived from the first item of each EMA ("Have you drank any alcohol that you have not yet reported?"). If participants answered yes to this question they were prompted to enter the hour and date of the first unreported drink and the hour and date of their last drink. Following this information, participants were asked whether their goal was still to remain abstinent in the future. At each monthly follow-up visit an Alcohol Timeline Followback was administered by study staff to identify unreported lapses and resolve conflicting (e.g., lapse end time predating lapse start time) or incomplete (e.g., no lapse end time) reports. 

### Individual Differences
At the screening visit we collected self-report information about demographics, mental health, and drug and alcohol use history.

## Data Analytic Strategy
### Labels
We defined a lapse as the hour onset of any drinking period. 

```
- lapse onset obtained from EMA item 1
- excluded 24 hours following a lapse onset
- excluded lapse conflicts reported in TLFB 
- excluded lapses over 24 hours?
- remaining hours were labeled as no lapse
```

### Features
```
- Created features from lapse labels (e.g., time of day and day of week) and predictors.
- total # of features extracted from 10 EMA items. 
- window sizes for features (12, 24, etc.)
- feature transformations/computations (counts, minimum, etc) - provide concrete examples 
- raw and change features
```

Features were calculated using several different time windows of data (6, 12, 24, 48, 72, and 168 hours). For example if we were using a window of 24 hours and wanted to predict probability of a lapse at 6:00 PM then we would use all available features since 6:00 PM the day before up until the hour we were predicting for. We believed it was important to include varying time windows because it is likely that some features, like a steep increase in urge to drink, may be more temporally close to a lapse (i.e., 6 hours) and other features, like anticipated stressful situations, may reflect more distant lapses (i.e, 168 hours).

### Risk Prediction
```
- statistical algorithms
- resampling (up, down)?
```

We used 10-fold cross-validation to select and evaluate the best performing model configuration. Cross-validation folds were grouped by participant ID. This ensured that the performance estimates reflected model performance on new data (i.e., participants in the held-in data set were not also in the held-out data set). 

The best model configuration was selected based on the area under the ROC curve (AUC). This model was then evaluated on performance metrics of sensitivity, specificity, and positive predictive value (PPV). 

```
Mention 0-lag and different prediction windows (1 hour, 1 day, 1 week)
```


### Info in my FYP that we might want to include in results - just need to double check numbers
```
- Across participants there were a total of 1029 positive lapses. 
- There was variation in the frequency of lapses, ranging from 0-75 lapses per participant (M = 6.8, SD = 12.0).  
- Only 56% of participants (N = 84) reported a lapse. However, this was expected since our participants all had a goal of abstinence from alcohol.
```