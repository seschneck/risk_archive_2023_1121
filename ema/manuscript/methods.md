# Method
## Research Transparency

## Participants
We recruited participants in early recovery (1-8 weeks of abstinence) from alcohol use disorder in Madison, Wisconsin, USA, to participate in a 3-month longitudinal study. Participants were recruited through print and targeted digital advertisements and partnerships with treatment centers. We required that participants:

1.  were 18 years of age or older,
2.  were able to write and read in English,
3.  had at least moderate alcohol use disorder (\>= 4 DSM-5 symptoms^[We measured DSM-5 symptoms with a self-report survey administered to participants during the in-person screening visit.]),
4.  were abstinent from alcohol for at least 1 week but no longer than 2 months,
5.  were willing to use a single smartphone (their personal phone or one provided by us) while enrolled in the study.

We also excluded participants if they exhibited severe symptoms of psychosis or paranoia^[Psychosis and paranoia were defined as scores greater than 2.2 or 2.8, respectively, on the psychosis or paranoia scales of the on the Symptom Checklist – 90 (SCL-90) [@derogatisSCL90OutpatientPsychiatric1973].]. 

One hundred ninety-two participants were eligible for enrollment. Of these participants, 191 consented to participate in the study at the screening session and 169 subsequently enrolled in the study at the enrollment visit which occurred approximately one week later. Fifteen participants discontinued prior to the first monthly follow-up visit. <!--GEF: might be a good place to refer to a consort diagram/enrollment chart (or maybe below the next paragraph?). i'm left wondering why these people didn't enroll - though not sure if we have that information-->

We excluded data from one participant who appeared to not have a goal of abstinence during their participation (i.e., they had lapses every day on study except for one day and reported they were uncertain if their goal was abstinence on the daily EMA and monthly follow-up surveys). We also excluded data from two participants who showed evidence of careless responding (e.g., completing 2-4 EMAs within an hour and providing different responses) and unusually low compliance (e.g., only 5 EMAs completed over one month), rendering their lapse labels unusable. 

Our final sample consisted of 151 participants. Participants provided study measures for one (N = 14), two (N = 6) or three (N = 131) months. Participants were mostly white (87%), roughly half were male (51%), and the mean age was 41 years (SD = 12). 

<!--KW: Discuss if we want any demographic/AUD history/mental health tables or flowchart of participant retention.-->
<!--GEF: see my comment above, i think the flowchart would be helpful! demographic/AUD history/other participant characteristic tables probably most relevant in results -->

## Procedure
Participants completed five study visits over the course of approximately three months. After an initial phone screen, participants attended an in-person screening visit where we determined eligibility, obtained informed consent, and collected self-report measures of individual differences (e.g., demographics, mental health and alcohol use history). Eligible and consented participants returned approximately one week later to enroll in the study. Three additional follow-up visits  occurred about every 30 days participants were on study. At each follow-up visit, we collected additional self-report and interview measures (e.g., Alcohol Timeline Followback). <!--GEF: rather than provide an example here of the TLFB, i think you could just refer to Measures below. or could refer above when you talk about IDs and then say nothing here.-->

For the entire duration on study, participants were expected to complete EMAs four times each day. Other personal sensing data streams (geolocation, cellular communications, sleep quality, and audio check-ins) were collected as part of the larger grant's aims (R01 AA024391). A full description of the procedure and data collected at each visit can be found at the study's OSF page [<!--Insert link here-->]. All procedures were approved by the University of Wisconsin-Madison Institutional Review Board.

## Measures
### EMA
<!--citation for validity of self-reported alcohol use: https://pubmed.ncbi.nlm.nih.gov/26160523/-->
Participants completed a brief (7-10 questions) EMA four times each day following reminders from us that were sent by text message. These text messages included a link to a Qualtrics survey, optimized for completion on their smartphone. 

All four EMAs included seven items that asked about alcohol use not yet reported, current affective state (pleasantness and arousal), greatest urge to drink alcohol since the last EMA, any pleasant or positive events, any hassles or stressful events, and any exposure to risky situations (i.e., people, places, or things) that occurred since the last EMA. The first EMA each day asked an additional three questions about how likely participants were to encounter a risky situation, encounter a stressful event, and drink alcohol in the upcoming week. 

The first and last EMAs of the day were scheduled within one hour of participants' typical wake and sleep times. The other two EMAs were each scheduled randomly within the first and second halves of the participants' typical day. All EMAs were separated from each other by at least one hour.

### Individual Differences
At the screening visit we collected self-report information about demographics, mental health, and drug and alcohol use history. These measures are used to describe our sample. Only age, sex, race, education, and marital status are used as features for our analyses. <!--Refer to OSF for full list of measures--> <!--GEF: if you're using them to describe the sample, do they need to be listed/mentioned here? -->

## Data Analytic Strategy
Data preprocessing and modeling were done in RStudio, using the tidyverse and tidymodels ecosystems. <!--KW: Will add version numbers and references-->

### Lapse Labels
We created rolling lapse windows that varied in width (i.e., 1 hour, 24 hours, and 168 hours). Each window shifted hour by hour for prediction.

We only included lapse and no lapse windows that we were confident were accurately labeled. A valid lapse window must contain a lapse observation. A valid no lapse window must have all observations labeled as no lapse (i.e., no excluded observations). <!--GEF: after reading the next two paragraphs, i'm not sure this paragraph is necessary. possibly, you could move this information into the subsequent paragraphs? but i feel like it's already there and is clearer in those paragraphs -->

We derived lapse labels from the first item of each EMA ("Have you drank any alcohol that you have not yet reported?"). If participants answered yes to this item, they were prompted to enter the hour and date of the first unreported drink (i.e., lapse onset) and the hour and date of their last drink (i.e., lapse offset). To be labeled as a lapse, the observation must have an hour associated with it, not be in the future, and the onset and offset of lapse must be ordered correctly. 

We used the EMA and other data (e.g., Alcohol Timeline Follow-back <!--GEF: cite -->) to label no lapse observations. Observations that we could not definitively label as no lapse were excluded from sampling (e.g., occurred within 24 hours of lapse onset, contained lapse reported retrospectively <!--GEF: this second example isn't clear to me-->).


### Feature Engineering
Features were calculated from different periods of data (6, 12, 24, 48, 72, and 168 hours prior to observation). Features were derived from EMA questions, demographics (i.e., age, white vs. other race, sex, education, and marital status), previous history of lapses, and date and time of observation (i.e., evening vs other hour, and day of week). 

We created features using both raw (e.g., min., max., median, most recent response, and total counts) and change (e.g., within-subject baseline comparisons) scores. 

This gave us a total of 267,283 features for 1-hour lapse windows, 274,175 features for 24-hour lapse windows, and 270,077 features for 168-hour lapse windows.


### Model Training and Evaluation

**1. algorithms**  
We considered three candidate classification statistical algorithms (elasticnet, random forest, xgboost) that differed across various characteristics expected to affect model performance (e.g., flexibility, ability to handle higher-order interactions natively, complexity, linear vs. non-linear). <!-- [also changed previous sentence to expand slightly]. Suggested addition: These algorithms are well-established with documented good "out of box" performance, and they vary with respect to the degree of feature selection performed automatically during model fitting.--> <!--cite Kunn & Johnson APM -->

**2. Model configurations**   
Each candidate algorithm was tuned for its associated hyperparameters <!--(i.e., model tuning parameters)--> and resampled using two methods (up and down sampling). <!--GEF: Resampling approaches are designed to address class imbalances by sampling additional minority class observations (upsampling) or removing majority class observations (downsampling) within held-in data.-->

**3. k-fold**  
We trained all possible model configurations (i.e., <!--combination of -->algorithm, hyperparameter values, and resampling method) using 10-fold cross validation. <!--GEF suggest adding a 1-2 sentence description here--> Cross-validation folds were grouped by participant ID (i.e., participants in the held-in data set were not also in the held-out data set). 

**4. pre-processing**  
Generic (e.g., handling of missing data and zero-variance variables) and algorithmic-specific (e.g., dummy coding and normalization) pre-processing steps were estimated using held in data and applied to held out data<!--KW: possibly reference supplemental recipe code here-->.

**5. selection** <!--GEF: if this subsection gets labeled, consider calling it performance metric?--> 
The best model configuration was selected <!--GEF: using cross-validation--> based on the primary performance metric of interest, area under the receiver operating characteristic curve (AUC ROC).

**6. evaluation**  
We evaluated performance of our best model configuration by averaging the AUC ROC across the 10 held out folds. In addition to our primary performance metric, we report the average sensitivity, specificity, balanced accuracy, and positive predictive value (PPV) from all held out folds<!--KW: will cite source for these metrics - tidymodels reference or IAML-->. We also provide the ROC and Precision-Recall (PR) curves. 

**7. interpretability**  
We calculated Shapley Additive Explanations (SHAP) scores to provide a global (i.e., across participants) index of feature importance.<!--KW: Not sure best spot for this section yet-->



# Results
<!--Information for results: Participants were on study for an average of 85 days out of the possible 90 days. Participants had endorsed using on average 4 other types of drugs (not including alcohol) over their lifetime. Additionally, participants on average scored a 9 on a self-report version of the DSM-5 symptom criteria for alcohol use disorder. Generally, scores of 2-3 are considered mild, 4-5 are considered moderate, and 6+ considered severe alcohol use disorder.-->

<!--Move to results: Across participants there were a total of 1029 unique lapses. There was variation in the frequency of lapses, ranging from 0-75 lapses per participant (M = 6.8, SD = 12.0). Only 56% of participants (N = 84) reported a lapse. However, this was expected since our participants all had a goal of abstinence from alcohol.-->

<!--Citation for ROC cutoffs - https://journals.copmadrid.org/ejpalc/art/ejpalc2018a5 (.58 = small effect size, .69 = medium effect size, .79 = large effect size, corresponding to Cohen's d of .2, .5, .8 respectively).-->