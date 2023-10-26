print('Correcting errors in files in P:/StudyData/RISK/database')

library(lmSupport)
library(tidyverse)
DataBasePath = 'P:/StudyData/RISK/database'


#EMA Morning -----------------------------------------
dE1 = read_rds(file.path(DataBasePath, 'ema_morning.rds'))

dE1$EMA_1.5[dE1$SubID=='002' & dE1$UTC==1491307485] = 3

dE1$EMA_1.5[dE1$SubID=='003' & dE1$UTC==1491057449] = 3

dE1$UTC[dE1$SubID=='001' & is.na(dE1$UTC)] = 1488489431
dE1$UTC[dE1$SubID=='003' & is.na(dE1$UTC)] = 1491250545

dE1 = dE1[!(dE1$SubID=='037' & dE1$UTC==1519322422),] #Remove two incorrectly entered EMAs
dE1 = dE1[!(dE1$SubID=='037' & dE1$UTC==1518715474),] 

dE1 = dE1[!(dE1$SubID=='043' & dE1$UTC==1528033409),]

dE1$EMA_1.1[dE1$SubID =='054' & dE1$UTC==1527860288] = '05-31-2018'
dE1$EMA_1.3[dE1$SubID =='054' & dE1$UTC==1527860288] = '05-31-2018'

dE1$EMA_1.3[dE1$SubID =='131' & dE1$UTC==1539875873] = '10-17-2018'

dE1$EMA_1.1[dE1$SubID =='137' & dE1$UTC==1541326066] = '11-04-2018'
dE1$EMA_1.3[dE1$SubID =='137' & dE1$UTC==1541326066] = '11-04-2018'

dE1$EMA_1.1[dE1$SubID =='092' & dE1$UTC==1535825983] = '08-31-2018'

write_rds(dE1, file.path(DataBasePath, 'ema_morning.rds'))
rm(dE1)

#EMA Later -------------------------------------------
dE2 = read_rds(file.path(DataBasePath, 'ema_later.rds'))

dE2$UTC[dE2$SubID=='001' & is.na(dE2$UTC)] = c(1491347005, 1495673213)
dE2$UTC[dE2$SubID=='002' & is.na(dE2$UTC)] = 1496088878

dE2$EMA_1.5[dE2$SubID=='002' & dE2$UTC==1491102061] = 3
dE2$EMA_1.5[dE2$SubID=='003' & dE2$UTC==1490497234] = 3
dE2$EMA_1.5[dE2$SubID=='003' & dE2$UTC==1491103187] = 3
dE2$EMA_1.5[dE2$SubID=='025' & dE2$UTC==1512346255] = 3

dE2$EMA_1.4[dE2$SubID=='025' & dE2$UTC==1515210778] = 23

dE2$EMA_1[dE2$SubID=='009' & dE2$UTC==1506559474] = 2
dE2$EMA_1.1[dE2$SubID=='009' & dE2$UTC==1506559474] = '09-27-2017'
dE2$EMA_1.2[dE2$SubID=='009' & dE2$UTC==1506559474] = 12
dE2$EMA_1.3[dE2$SubID=='009' & dE2$UTC==1506559474] = '09-27-2017'
dE2$EMA_1.4[dE2$SubID=='009' & dE2$UTC==1506559474] = 13
dE2$EMA_1.5[dE2$SubID=='009' & dE2$UTC==1506559474] = 3

dE2$EMA_1[dE2$SubID=='009' & dE2$UTC==1506633764] = 2
dE2$EMA_1.1[dE2$SubID=='009' & dE2$UTC==1506633764] = '09-28-2017'
dE2$EMA_1.2[dE2$SubID=='009' & dE2$UTC==1506633764] = 12
dE2$EMA_1.3[dE2$SubID=='009' & dE2$UTC==1506633764] = '09-28-2017'
dE2$EMA_1.4[dE2$SubID=='009' & dE2$UTC==1506633764] = 13
dE2$EMA_1.5[dE2$SubID=='009' & dE2$UTC==1506633764] = 3

dE2$EMA_1[dE2$SubID=='009' & dE2$UTC==1506795415] = 2
dE2$EMA_1.1[dE2$SubID=='009' & dE2$UTC==1506795415] = '09-29-2017'
dE2$EMA_1.2[dE2$SubID=='009' & dE2$UTC==1506795415] = 12
dE2$EMA_1.3[dE2$SubID=='009' & dE2$UTC==1506795415] = '09-29-2017'
dE2$EMA_1.4[dE2$SubID=='009' & dE2$UTC==1506795415] = 13
dE2$EMA_1.5[dE2$SubID=='009' & dE2$UTC==1506795415] = 3

dE2$EMA_1[dE2$SubID=='042' & dE2$UTC==1521242318] = 2
dE2$EMA_1.1[dE2$SubID=='042' & dE2$UTC==1521242318] = '03-15-2018'
dE2$EMA_1.2[dE2$SubID=='042' & dE2$UTC==1521242318] = 12
dE2$EMA_1.3[dE2$SubID=='042' & dE2$UTC==1521242318] = '03-15-2018'
dE2$EMA_1.4[dE2$SubID=='042' & dE2$UTC==1521242318] = 13
dE2$EMA_1.5[dE2$SubID=='042' & dE2$UTC==1521242318] = 3

dE2$EMA_1[dE2$SubID=='047' & dE2$UTC==1523939114] = 2
dE2$EMA_1.1[dE2$SubID=='047' & dE2$UTC==1523939114] = '04-10-2018'
dE2$EMA_1.2[dE2$SubID=='047' & dE2$UTC==1523939114] = 16
dE2$EMA_1.3[dE2$SubID=='047' & dE2$UTC==1523939114] = '04-10-2018'
dE2$EMA_1.4[dE2$SubID=='047' & dE2$UTC==1523939114] = 17
dE2$EMA_1.5[dE2$SubID=='047' & dE2$UTC==1523939114] = 3

dE2$EMA_1.1[dE2$SubID=='065' & dE2$UTC==1526697053] = '05-18-2018'
dE2$EMA_1.3[dE2$SubID=='065' & dE2$UTC==1523939114] = '05-18-2018'

dE2$EMA_1.1[dE2$SubID=='065' & dE2$UTC==1526585615] = '05-17-2018'
dE2$EMA_1.3[dE2$SubID=='065' & dE2$UTC==1526585615] = '05-17-2018'

dE2$EMA_1.4[dE2$SubID=='020' & dE2$UTC==1514796205] = 23

dE2$EMA_1.3[dE2$SubID=='026' & dE2$UTC==1510972226] = '11-18-2017'

dE2$EMA_1.3[dE2$SubID=='118' & dE2$UTC==1537642706] = '09-22-2018'
dE2$EMA_1.1[dE2$SubID=='118' & dE2$UTC==1538613095] = '10-03-2018'

dE2$EMA_1.1[dE2$SubID=='137' & dE2$UTC==1538895481] = '10-06-2018'
dE2$EMA_1.3[dE2$SubID=='137' & dE2$UTC==1539564730] = '10-14-2018'
dE2$EMA_1.3[dE2$SubID=='137' & dE2$UTC==1541383130] = '11-04-2018'
dE2$EMA_1.1[dE2$SubID=='137' & dE2$UTC==1541275293] = '11-03-2018'

dE2$EMA_1.1[dE2$SubID=='161' & dE2$UTC==1546379852] = '12-31-2018'

dE2$EMA_1[dE2$SubID=='080' & dE2$UTC==1533045108] =2
dE2$EMA_1.1[dE2$SubID=='080' & dE2$UTC==1533045108] = '07-30-2018'
dE2$EMA_1.2[dE2$SubID=='080' & dE2$UTC==1533045108] = 17
dE2$EMA_1.3[dE2$SubID=='080' & dE2$UTC==1533045108] = '07-30-2018'
dE2$EMA_1.4[dE2$SubID=='080' & dE2$UTC==1533045108] = 17
dE2$EMA_1.5[dE2$SubID=='080' & dE2$UTC==1533045108] = 3

dE2$EMA_1.4[dE2$SubID=='081' & dE2$UTC==1534382135] = 18

dE2$EMA_1.1[dE2$SubID=='084' & dE2$UTC==1536465966] = '09-09-2018'

dE2$SubID[dE2$SubID=='101'] = '135'

dE2$EMA_1.1[dE2$SubID=='131' & dE2$UTC==1540163411] = '10-21-2018'
dE2$EMA_1.1[dE2$SubID=='131' & dE2$UTC==1540693052 ] = '10-27-2018' 
dE2$EMA_1.1[dE2$SubID=='131' & dE2$UTC==1543018646 ] = '11-22-2018' 
dE2$EMA_1.3[dE2$SubID=='131' & dE2$UTC==1542167431 ] = '11-13-2018' 

dE2$EMA_1.1[dE2$SubID=='137' & dE2$UTC==1538895481] = '10-21-2018'
dE2$EMA_1.3[dE2$SubID=='137' & dE2$UTC==1539564730] = '11-13-2018' 
dE2$EMA_1.3[dE2$SubID=='137' & dE2$UTC==1541383130] = '11-13-2018' 
 
dE2$EMA_1.3[dE2$SubID=='121' & dE2$UTC==1540782677] = '10-28-2018' 

dE2$EMA_1.1[dE2$SubID == '042' & dE2$UTC == 1521938445]  = '03-24-2018'

dE2$EMA_1.2[dE2$SubID == '191' & dE2$UTC == 1553300770] = 19
dE2$EMA_1.3[dE2$SubID == '191' & dE2$UTC == 1553300770] = '03-22-2019'
dE2$EMA_1.1[dE2$SubID == '191' & dE2$UTC == 1554158223] = '04-01-2019'

dE2$EMA_1[dE2$SubID == '211' & dE2$UTC == 1554011917] = 1
dE2$EMA_1.1[dE2$SubID == '211' & dE2$UTC == 1554011917] = NA
dE2$EMA_1.2[dE2$SubID == '211' & dE2$UTC == 1554011917] = NA
dE2$EMA_1.3[dE2$SubID == '211' & dE2$UTC == 1554011917] = NA
dE2$EMA_1.4[dE2$SubID == '211' & dE2$UTC == 1554011917] = NA
dE2$EMA_1.5[dE2$SubID == '211' & dE2$UTC == 1554011917] = NA

write_rds(dE2, file.path(DataBasePath, 'ema_later.rds'))
rm(dE2)


#Screening Battery -----------------------------------
dS = read_rds(file.path(DataBasePath, 'screen.rds'))

dS$SubID[dS$SubID =='008' & dS$UTC == 1498754567] = '010'

dS$SubID[dS$SubID =='022' & dS$UTC == 1508281036] = '023'

dS$SubID[dS$SubID =='149' & dS$UTC == 1539634716 ] = '150'

dS$UTC[dS$SubID=='001'] = 1487176523

dS$UTC[dS$SubID=='009'] = 1498062441

dS$DEM2_2[dS$SubID=='001'] = 2
dS$DEM2_4[dS$SubID=='001'] = 0
dS$DEM2_6[dS$SubID=='001'] = 0
dS$DEM2_8[dS$SubID=='001'] = 0

dS$UTC[dS$SubID=='002'] = 1487364008

dS$ASSIST_3_1[dS$SubID=='002'] = 1
dS$ASSIST_3_2[dS$SubID=='002'] = 1
dS$ASSIST_3_3[dS$SubID=='002'] = 1
dS$ASSIST_3_4[dS$SubID=='002'] = 1
dS$ASSIST_3_5[dS$SubID=='002'] = 1
dS$ASSIST_3_6[dS$SubID=='002'] = 1
dS$ASSIST_3_7[dS$SubID=='002'] = 1
dS$ASSIST_3_8[dS$SubID=='002'] = 1
dS$ASSIST_4_1[dS$SubID=='002'] = 1
dS$ASSIST_4_2[dS$SubID=='002'] = 1
dS$ASSIST_4_3[dS$SubID=='002'] = 1
dS$ASSIST_4_4[dS$SubID=='002'] = 1
dS$ASSIST_4_5[dS$SubID=='002'] = 1
dS$ASSIST_4_6[dS$SubID=='002'] = 1
dS$ASSIST_4_7[dS$SubID=='002'] = 1
dS$ASSIST_4_8[dS$SubID=='002'] = 1



dS$DEM2_2[dS$SubID=='002'] = 2
dS$DEM2_4[dS$SubID=='002'] = 1
dS$DEM2_6[dS$SubID=='002'] = 4
dS$DEM2_8[dS$SubID=='002'] = 0



dS$UTC[dS$SubID=='003'] =1488906926 

dS$DEM2_2[dS$SubID=='003'] = 4
dS$DEM2_4[dS$SubID=='003'] = 0
dS$DEM2_6[dS$SubID=='003'] = 0
dS$DEM2_8[dS$SubID=='003'] = 0

dS$AUH_6_7[dS$SubID=='020'] = 0
dS$AUH_6.1[dS$SubID=='020'] = ''

dS$AUH_10[dS$SubID=='028'] = 0

dS$ASSIST_1_8[dS$SubID=='052'] = 1

dS$ASSIST_8[dS$SubID=='034'] = 1

dS$AUH_4[dS$SubID == '059'] = 40

dS$SubID[dS$UTC == 1522185338 & dS$SubID=='055'] = '061'

dS$AUH_8_Month[dS$SubID=='064'] = 5

dS$SubID[dS$SubID =='079' & dS$UTC == 1529615510] = '078'

dS$DEM2_2[dS$SubID=='103'] = 1
dS$DEM2_2[dS$SubID=='102'] = 2
dS$DEM2_2 = as.numeric(dS$DEM2_2)

dS$AUH_1[dS$SubID == '211'] = '14'
dS$AUH_1 = as.numeric(dS$AUH_1)

dS$UTC[dS$SubID == '187'] = 1549044060

#Fix issues with Income (DEM_7) reporting (SEEMS TO NO LONGER BE AN ISSUE?)
dS$DEM_7 = str_replace_all(dS$DEM_7,',','')
dS$DEM_7 = as.numeric(dS$DEM_7)


write_rds(dS, file.path(DataBasePath, 'screen.rds'))
rm(dS)  

#Intake Battery -----------------------------------
dI = read_rds(file.path(DataBasePath, 'intake.rds'))

dI$MSPSS_1[dI$SubID=='001'] =	5
dI$MSPSS_2[dI$SubID=='001'] =	5
dI$MSPSS_3[dI$SubID=='001'] =	6
dI$MSPSS_4[dI$SubID=='001'] =	5
dI$MSPSS_5[dI$SubID=='001'] =	5
dI$MSPSS_6[dI$SubID=='001'] =	5
dI$MSPSS_7[dI$SubID=='001'] =	5
dI$MSPSS_8[dI$SubID=='001'] =	5
dI$MSPSS_9[dI$SubID=='001'] =	5
dI$MSPSS_10[dI$SubID=='001'] =	5
dI$MSPSS_11[dI$SubID=='001'] =	5
dI$MSPSS_12[dI$SubID=='001'] =	5


dI$UTC[dI$SubID=='001'] = 1487877413

dI$MAM_1.3_5[dI$SubID=='001'] = 1
dI$MAM_1.7[dI$SubID=='001'] =	1
dI$MAM_11[dI$SubID=='001'] =	2
dI$MAM_12[dI$SubID=='001'] =	2
dI$MAM_19[dI$SubID=='001'] =	2
dI$MAM_20[dI$SubID=='001'] =	30
dI$MAM_22[dI$SubID=='001'] =	2

dI$MAM_1.3_1[dI$SubID=='002'] = 1
dI$MAM_1.3_2[dI$SubID=='002'] = 1
dI$MAM_1.4[dI$SubID=='002'] =	4
dI$MAM_11[dI$SubID=='002'] =	2
dI$MAM_12[dI$SubID=='002'] =	0
dI$MAM_19[dI$SubID=='002'] =	2
dI$MAM_20[dI$SubID=='002'] =	30
dI$MAM_22[dI$SubID=='002'] =	3

dI$MAM_1.3_1[dI$SubID=='003'] = 1
dI$MAM_1.3_5[dI$SubID=='003'] = 1
dI$MAM_1.7[dI$SubID=='003'] =	2
dI$MAM_11[dI$SubID=='003'] =	2
dI$MAM_12[dI$SubID=='003'] =	4
dI$MAM_19[dI$SubID=='003'] =	2
dI$MAM_20[dI$SubID=='003'] =	14
dI$MAM_22[dI$SubID=='003'] =	3

dI$MAM_22[dI$SubID=='033'] = 3

dI$MAM_10[dI$SubID==137 & dI$UTC==1538421324] = 0
dI$MAM_10 = as.numeric(dI$MAM_10)

dI$UTC[dI$SubID=='173' & is.na(dI$UTC)] = 1549043340

dI$MAM_1.4[dI$SubID=='187'] = 5  #fix from ",5"
dI$MAM_1.4 = as.integer(dI$MAM_1.4)

dI$MAM_8[dI$SubID=='062'] = 0  #fix from ",,,0"
dI$MAM_8[dI$SubID=='137'] = 1  #fix from ",1"
dI$MAM_8 = as.integer(dI$MAM_8)

#reclass MAM_15
dI$MAM_15[dI$MAM_15=='None'] = "0"
dI$MAM_15[dI$MAM_15=='O'] = "0"
dI$MAM_15 = as.integer(dI$MAM_15)

dI = filter(dI, !(SubID == '042' & UTC == 1523911892))

write_rds(dI, file.path(DataBasePath, 'intake.rds'))
rm(dI)  


#Followup12 ----------------------------------------------------
dF12 = read_rds(file.path(DataBasePath, 'followup12.rds'))

dF12$MAM_1.3_5[dF12$SubID=='001' & dF12$UTC==1490978317] = 1
dF12$MAM_1.7[dF12$SubID=='001' & dF12$UTC==1490978317] =	1
dF12$MAM_11[dF12$SubID=='001' & dF12$UTC==1490978317] =	1
dF12$MAM_12[dF12$SubID=='001' & dF12$UTC==1490978317] =	2
dF12$MAM_19[dF12$SubID=='001' & dF12$UTC==1490978317] =	1
dF12$MAM_20[dF12$SubID=='001' & dF12$UTC==1490978317] =	30
dF12$MAM_22[dF12$SubID=='001' & dF12$UTC==1490978317] =	3
dF12$MAM_10[dF12$SubID=='030' & dF12$UTC==1513809800] = 0  #change from "00"
dF12$MAM_10[dF12$MAM_10 == ",0"] = 0 
dF12$MAM_10 = as.numeric(dF12$MAM_10)

dF12$MAM_1.3_5[dF12$SubID=='003' & dF12$UTC==1495646392] = 1

dF12$UTC[dF12$SubID=='033' & is.na(dF12$UTC)] = 1519142940

dF12$MAM_1.3_2[dF12$SubID == '033' & dF12$UTC==1515684753]=1
dF12$MAM_1.4[dF12$SubID == '033' & dF12$UTC==1515684753]=4

dF12$UTC[dF12$SubID=='042' & dF12$UTC==1524606662] = 1523911892

dF12$UTC[dF12$SubID=='054' & dF12$UTC==1531847769] = 1524770168

dF12$UTC[dF12$SubID=='208' & is.na(dF12$UTC)] = 1558648860

dF12$MAM_12[dF12$SubID=='043' & dF12$MAM_12==',2'] = 2  #fix from ",2"
dF12$MAM_12 = as.integer(dF12$MAM_12)

dF12$MAM_15[dF12$MAM_15=='None'] = '0'
dF12$MAM_15[dF12$MAM_15=='O'] = '0'
dF12$MAM_15 = as.integer(dF12$MAM_15)


#nicotine/e-cig (2)
dF12$ASSIST_2_1[dF12$SubID == "019" & dF12$UTC == 1511803223 ] <- dF12$ASSIST_2_9[dF12$SubID == "019" & dF12$UTC == 1511803223]
# adrenaline(2)
dF12$ASSIST_2_4[dF12$SubID == "039" & dF12$UTC == 1521472701 ] <- dF12$ASSIST_2_9[dF12$SubID == "039" & dF12$UTC == 1521472701]
# alprazolam(2)
dF12$ASSIST_2_6[dF12$SubID == "092" & dF12$UTC == 1535385426 ] <- dF12$ASSIST_2_9[dF12$SubID == "092" & dF12$UTC == 1535385426]
# Muscle relaxants (3)
dF12$ASSIST_2_6[dF12$SubID == "143" & dF12$UTC == 1541792344 ] <- dF12$ASSIST_2_9[dF12$SubID == "143" & dF12$UTC == 1541792344]
# Nyquil (3)
dF12$ASSIST_2_6[dF12$SubID == "149" & dF12$UTC == 1543265226 ] <- dF12$ASSIST_2_9[dF12$SubID == "149" & dF12$UTC == 1543265226]
# sudafed (4)
dF12$ASSIST_2_4[dF12$SubID == "175" & dF12$UTC == 1551721068 ] <- dF12$ASSIST_2_9[dF12$SubID == "175" & dF12$UTC == 1551721068]
# cough syrup/same participant as previous (4)
dF12$ASSIST_2_6[dF12$SubID == "175" & dF12$UTC == 1551721068 ] <- dF12$ASSIST_2_9[dF12$SubID == "175" & dF12$UTC == 1551721068]
# vaping (4)
dF12$ASSIST_2_1[dF12$SubID == "179" & dF12$UTC == 1555354074 ] <- dF12$ASSIST_2_9[dF12$SubID == "179" & dF12$UTC == 1555354074]
# pain medication (4)
dF12$ASSIST_2_8[dF12$SubID == "270" & dF12$UTC == 1570132338 ] <- dF12$ASSIST_2_9[dF12$SubID == "270" & dF12$UTC == 1570132338]
# cough syrup (4)
dF12$ASSIST_2_6[dF12$SubID == "269" & dF12$UTC == 1573148495 ] <- dF12$ASSIST_2_9[dF12$SubID == "269" & dF12$UTC == 1573148495]
dF12 <- select(dF12, -c(ASSIST_2_9, ASSIST_2_9_TEXT))

# adrenaline(4)
dF12$ASSIST_3_4[dF12$SubID == "039" & dF12$UTC == 1521472701 ] <- dF12$ASSIST_3_9[dF12$SubID == "039" & dF12$UTC == 1521472701]
# alprazolam(2)
dF12$ASSIST_3_6[dF12$SubID == "092" & dF12$UTC == 1535385426 ] <- dF12$ASSIST_3_9[dF12$SubID == "092" & dF12$UTC == 1535385426]
# Muscle relaxants (3)
dF12$ASSIST_3_6[dF12$SubID == "143" & dF12$UTC == 1541792344 ] <- dF12$ASSIST_3_9[dF12$SubID == "143" & dF12$UTC == 1541792344]
# adderall (2)
dF12$ASSIST_3_4[dF12$SubID == "158" & dF12$UTC == 1549492578 ] <- dF12$ASSIST_3_9[dF12$SubID == "158" & dF12$UTC == 1549492578]
# sudafed (3)
dF12$ASSIST_3_4[dF12$SubID == "175" & dF12$UTC == 1551721068 ] <- dF12$ASSIST_3_9[dF12$SubID == "175" & dF12$UTC == 1551721068]
# cough syrup/same participant as previous (3)
dF12$ASSIST_3_6[dF12$SubID == "175" & dF12$UTC == 1551721068 ] <- dF12$ASSIST_3_9[dF12$SubID == "175" & dF12$UTC == 1551721068]
# vaping (4)
dF12$ASSIST_3_1[dF12$SubID == "179" & dF12$UTC == 1555354074 ] <- dF12$ASSIST_3_9[dF12$SubID == "179" & dF12$UTC == 1555354074]
# Muscle relaxants (2)
dF12$ASSIST_3_6[dF12$SubID == "245" & dF12$UTC == 1567105892 ] <- dF12$ASSIST_3_9[dF12$SubID == "245" & dF12$UTC == 1567105892]
# cough syrup (2)
dF12$ASSIST_3_6[dF12$SubID == "269" & dF12$UTC == 1573148495 ] <- dF12$ASSIST_3_9[dF12$SubID == "269" & dF12$UTC == 1573148495]
dF12 <- select(dF12, -c(ASSIST_3_9, ASSIST_3_9_TEXT))

write_rds(dF12, file.path(DataBasePath, 'followup12.rds'))
rm(dF12) 

#Followup3 ----------------------------------------------------
#FileName = 'Followup3.csv'
#dF3 = read.csv(file.path(DataBasePath, FileName), header = TRUE, as.is=TRUE)
#dF3$SubID = varPadString(dF3$SubID,3,'0')

#dF3$UTC[dF3$SubID=='007'] = 1505491356

#Sleep schedule  ------------------------------
dSS = read_rds(file.path(DataBasePath, 'sleep_schedule.rds'))
dSS$SubID = varPadString(dSS$SubID,3,'0')

#UTC updates not needed b/c now use ScheduleStart for UTC
#dSS$UTC[dSS$SubID=='001' & is.na(dSS$UTC)] = 1488403762
#dSS$UTC[dSS$SS_UTC_old==1490098740] = 1490098740
#dSS$UTC[dSS$SS_UTC_old==1490312100] = 1490312100
#dSS$SS_UTC_old = NULL
#dSS$UTC[dSS$SubID=='006' & is.na(dSS$UTC)] = 1497483141

#Fix old format for subjects 1-3 (moved from fun_database)
dSS$MoWake[dSS$SubID=='001' & dSS$UTC==1488434400 ] = '6:00'
dSS$MoBed[dSS$SubID=='001'  & dSS$UTC==1488434400 ] = '20:00'
dSS$TuWake[dSS$SubID=='001' & dSS$UTC==1488434400 ] = '6:00'
dSS$TuBed[dSS$SubID=='001'  & dSS$UTC==1488434400 ] = '20:00'
dSS$WeWake[dSS$SubID=='001' & dSS$UTC==1488434400 ] = '6:00'
dSS$WeBed[dSS$SubID=='001'  & dSS$UTC==1488434400 ] = '20:00'
dSS$ThWake[dSS$SubID=='001' & dSS$UTC==1488434400 ] = '6:00'
dSS$ThBed[dSS$SubID=='001'  & dSS$UTC==1488434400 ] = '20:00'
dSS$FrWake[dSS$SubID=='001' & dSS$UTC==1488434400 ] = '6:00'
dSS$FrBed[dSS$SubID=='001'  & dSS$UTC==1488434400 ] = '20:00'
dSS$SaWake[dSS$SubID=='001' & dSS$UTC==1488434400 ] = '6:00'
dSS$SaBed[dSS$SubID=='001'  & dSS$UTC==1488434400 ] = '20:00'
dSS$SuWake[dSS$SubID=='001' & dSS$UTC==1488434400 ] = '6:00'
dSS$SuBed[dSS$SubID=='001'  & dSS$UTC==1488434400 ] = '20:00'
dSS$MoWake[dSS$SubID=='002' & dSS$UTC==1490245200 ] = '5:30'
dSS$MoBed[dSS$SubID=='002'  & dSS$UTC==1490245200 ] = '20:30'
dSS$TuWake[dSS$SubID=='002' & dSS$UTC==1490245200 ] = '5:30'
dSS$TuBed[dSS$SubID=='002'  & dSS$UTC==1490245200 ] = '20:30'
dSS$WeWake[dSS$SubID=='002' & dSS$UTC==1490245200 ] = '5:30'
dSS$WeBed[dSS$SubID=='002'  & dSS$UTC==1490245200 ] = '20:30'
dSS$ThWake[dSS$SubID=='002' & dSS$UTC==1490245200 ] = '5:30'
dSS$ThBed[dSS$SubID=='002'  & dSS$UTC==1490245200 ] = '20:30'
dSS$FrWake[dSS$SubID=='002' & dSS$UTC==1490245200 ] = '5:30'
dSS$FrBed[dSS$SubID=='002'  & dSS$UTC==1490245200 ] = '20:30'
dSS$SaWake[dSS$SubID=='002' & dSS$UTC==1490245200 ] = '7:30'
dSS$SaBed[dSS$SubID=='002'  & dSS$UTC==1490245200 ] = '22:00'
dSS$SuWake[dSS$SubID=='002' & dSS$UTC==1490245200 ] = '7:00'
dSS$SuBed[dSS$SubID=='002'  & dSS$UTC==1490245200 ] = '20:30'
dSS$MoWake[dSS$SubID=='003' & dSS$UTC==1490158800 ] = '8:00'
dSS$MoBed[dSS$SubID=='003'  & dSS$UTC==1490158800 ] = '22:00'
dSS$TuWake[dSS$SubID=='003' & dSS$UTC==1490158800 ] = '8:00'
dSS$TuBed[dSS$SubID=='003'  & dSS$UTC==1490158800 ] = '22:00'
dSS$WeWake[dSS$SubID=='003' & dSS$UTC==1490158800 ] = '8:00'
dSS$WeBed[dSS$SubID=='003'  & dSS$UTC==1490158800 ] = '22:00'
dSS$ThWake[dSS$SubID=='003' & dSS$UTC==1490158800 ] = '8:00'
dSS$ThBed[dSS$SubID=='003'  & dSS$UTC==1490158800 ] = '22:00'
dSS$FrWake[dSS$SubID=='003' & dSS$UTC==1490158800 ] = '8:00'
dSS$FrBed[dSS$SubID=='003'  & dSS$UTC==1490158800 ] = '22:00'
dSS$SaWake[dSS$SubID=='003' & dSS$UTC==1490158800 ] = '8:00'
dSS$SaBed[dSS$SubID=='003'  & dSS$UTC==1490158800 ] = '22:00'
dSS$SuWake[dSS$SubID=='003' & dSS$UTC==1490158800 ] = '8:00'
dSS$SuBed[dSS$SubID=='003'  & dSS$UTC==1490158800 ] = '22:00'

dSS$UTC[dSS$SubID=='003' & dSS$UTC== -61620718164] = 1493182800
dSS$UTC[dSS$SubID=='066' & dSS$UTC== -61587281364] = 1526619600

write_rds(dSS, file.path(DataBasePath, 'sleep_schedule.rds'))
rm(dSS) 

