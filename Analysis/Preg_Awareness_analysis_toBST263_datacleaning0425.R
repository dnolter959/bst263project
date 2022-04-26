###############################################
#### To examine the discrepancies of hcg and self-reported pregnancy 
###############################################
library(foreign)
library(tidyverse)
library(survey)

# self-report pregnancy data
download.file("https://wwwn.cdc.gov/nchs/nhanes/2003-2004/RHQ_C.xpt", tf <- tempfile(), mode="wb")
report03 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2005-2006/RHQ_D.xpt", tf <- tempfile(), mode="wb")
report05 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2007-2008/RHQ_E.xpt", tf <- tempfile(), mode="wb")
report07 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2009-2010/RHQ_F.xpt", tf <- tempfile(), mode="wb")
report09 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/RHQ_G.xpt", tf <- tempfile(), mode="wb")
report11 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/RHQ_H.xpt", tf <- tempfile(), mode="wb")
report13 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/RHQ_I.xpt", tf <- tempfile(), mode="wb")
report15 <- foreign::read.xport(tf)
common_var <- Reduce(intersect, list(colnames(report03),colnames(report05), colnames(report07),colnames(report09),colnames(report11), colnames(report13), colnames(report15)))

report <- rbind(report03[, common_var],report05[, common_var],report07[,common_var],report09[,common_var],report11[,common_var],report13[,common_var],report15[,common_var])

report <- report%>% mutate(rep_preg=case_when(RHD143==1 ~ 1,
                                              RHD143==2 ~ 0,
                                              TRUE ~ NA_real_),
                          age_menarche=case_when(RHQ010 < 21  ~ RHQ010,
                                                  TRUE ~ NA_real_),
                           period_past_yr = case_when(RHQ031==1 ~ 1,
                                                      RHQ031==2 ~ 0,
                                                      TRUE ~ NA_real_),
                           age_last_perid = case_when(RHQ060 < 68  ~ RHQ060,
                                                      TRUE ~ NA_real_),
                           ever_preg = case_when(RHQ131==1 ~ 1,
                                                 RHQ131==2 ~ 0,
                                                 TRUE ~ NA_real_),
                           parity = case_when(RHQ160 < 21  ~ RHQ160,
                                              TRUE ~ NA_real_),
                           current_bf = case_when(RHQ200==1 ~ 1,
                                                  RHQ200==2 ~ 0,
                                                  TRUE ~ NA_real_),
                           hysterectomy = case_when(RHD280==1 ~ 1,
                                                    RHD280==2 ~ 0,
                                                    TRUE ~ NA_real_),
                           ever_birth_control_pill = case_when(RHQ420==1 ~ 1,
                                                               RHQ420==2 ~ 0,
                                                               TRUE ~ NA_real_),
                           ever_female_hormone = case_when(RHQ540==1 ~ 1,
                                                           RHQ540==2 ~ 0,
                                                           TRUE ~ NA_real_),
                           ever_hormone_estrogen = case_when(RHQ554==1 ~ 1,
                                                            RHQ554==2 ~ 0,
                                                            TRUE ~ NA_real_),
                           duration_hormone_estrogen = case_when(RHQ560Q < 56  ~ RHQ560Q,
                                                                 TRUE ~ NA_real_),
                           ever_hormone_combo = case_when(RHQ570==1 ~ 1,
                                                             RHQ570==2 ~ 0,
                                                             TRUE ~ NA_real_),
                           duration_hormone_combo = case_when(RHQ576Q < 31  ~ RHQ576Q,
                                                                 TRUE ~ NA_real_),
                           ever_estrogen_patch = case_when(RHQ580==1 ~ 1,
                                                            RHQ580==2 ~ 0,
                                                            TRUE ~ NA_real_),
                           duration_estrogen_patch = case_when(RHQ586Q < 27  ~ RHQ586Q,
                                                              TRUE ~ NA_real_),
                           ever_combo_patch = case_when(RHQ596==1 ~ 1,
                                                           RHQ596==2 ~ 0,
                                                           TRUE ~ NA_real_),
                           duration_combo_patch = case_when(RHQ602Q < 14  ~ RHQ602Q,
                                                               TRUE ~ NA_real_)) %>% 
  select (-common_var[-1])


# urine test hcg- only available for 20-44 years old
download.file("https://wwwn.cdc.gov/nchs/nhanes/2003-2004/UC_C.xpt", tf <- tempfile(), mode="wb")
hcg03 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2005-2006/UCPREG_D.xpt", tf <- tempfile(), mode="wb")
hcg05 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2007-2008/UCPREG_E.xpt", tf <- tempfile(), mode="wb")
hcg07 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2009-2010/UCPREG_F.xpt", tf <- tempfile(), mode="wb")
hcg09 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/UCPREG_G.xpt", tf <- tempfile(), mode="wb")
hcg11 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/UCPREG_H.xpt", tf <- tempfile(), mode="wb")
hcg13 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/UCPREG_I.xpt", tf <- tempfile(), mode="wb")
hcg15 <- foreign::read.xport(tf)
hcg<-rbind(hcg03, hcg05, hcg07, hcg09, hcg11, hcg13,hcg15)
hcg<-hcg[hcg$URXPREG==1 | hcg$URXPREG==2, ] # only keep those who did pregnancy test

# combining the report pregnancy and the test pregnancy
data <- merge(report, hcg, by='SEQN')
#data<-data %>% select(SEQN,URXPREG,RHD143)
data<-data[!is.na(data$rep_preg),] # delete those who do not have data on reported pregnancy

data <- data%>%rename(test_preg=URXPREG) %>% 
  mutate(preg = case_when(rep_preg==1 & test_preg==1 ~ 1, # pregnant and aware
                          rep_preg==0 & test_preg==1 ~ 2, # pregnant but unaware
                          TRUE ~ 0)) # not pregnant

table(data$preg) # 831 pregnant women

preg <- data[data$preg== 1 | data$preg == 2, ] 
# n = 842 pregnant women, 62 unaware

preg$aware <- ifelse(preg$preg==1, 1,0)
table(preg$aware) # 780 aware, 51 unaware

# merging in the demographic data
download.file("https://wwwn.cdc.gov/nchs/nhanes/2003-2004/DEMO_C.xpt", tf <- tempfile(), mode="wb")
demo03 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2005-2006/DEMO_D.xpt", tf <- tempfile(), mode="wb")
demo05 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2007-2008/DEMO_E.xpt", tf <- tempfile(), mode="wb")
demo07 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2009-2010/DEMO_F.xpt", tf <- tempfile(), mode="wb")
demo09 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/DEMO_G.xpt", tf <- tempfile(), mode="wb")
demo11 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/DEMO_H.xpt", tf <- tempfile(), mode="wb")
demo13 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DEMO_I.xpt", tf <- tempfile(), mode="wb")
demo15 <- foreign::read.xport(tf)
common_var <- Reduce(intersect, list(colnames(demo03),colnames(demo05), colnames(demo07),colnames(demo09),colnames(demo11), colnames(demo13), colnames(demo15)))

demo <- as.data.frame(rbind(demo03[, common_var],demo05[, common_var],demo07[,common_var],demo09[,common_var],demo11[,common_var],demo13[,common_var],demo15[,common_var]))

demo <- demo %>% mutate(survey_season = case_when(RIDEXMON==1 ~ 1,
                                                  RIDEXMON==2 ~ 2,
                                                  TRUE ~ NA_real_),
                        US_citizenship = case_when(DMDCITZN ==1 ~1,
                                                   DMDCITZN ==2 ~0,
                                                   TRUE ~ NA_real_),
                        education = case_when (DMDEDUC2 < 6 ~ DMDEDUC2,
                                               TRUE ~ NA_real_),
                        married = case_when (DMDMARTL < 8 ~ DMDMARTL,
                                             TRUE ~ NA_real_),
                        age = RIDAGEYR,
                        race = RIDRETH1,
                        number_ppl_household = DMDHHSIZ,
                        income_poverty_ratio = INDFMPIR) %>% select(-common_var[-1])


# survey_season
# 1 November 1 through April 30
# 2 May 1 through October 31

# categories of race:
# 1	Mexican American	
# 2	Other Hispanic
# 3	Non-Hispanic White
# 4	Non-Hispanic Black
# 5	Other Race - Including Multi-Racial

# categories of education
# 1	Less Than 9th Grade	
# 2	9-11th Grade (Includes 12th grade with no diploma)
# 3	High School Grad/GED or Equivalent
# 4	Some College or AA degree	
# 5	College Graduate or above	

# income_poverty_ratio is the ratio of household income divided by the poverty line


data <- merge(demo, preg, by='SEQN')


# merging in alcohol
download.file("https://wwwn.cdc.gov/nchs/nhanes/2003-2004/ALQ_C.xpt", tf <- tempfile(), mode="wb")
alco03 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2005-2006/ALQ_D.xpt", tf <- tempfile(), mode="wb")
alco05 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2007-2008/ALQ_E.xpt", tf <- tempfile(), mode="wb")
alco07 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2009-2010/ALQ_F.xpt", tf <- tempfile(), mode="wb")
alco09 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/ALQ_G.xpt", tf <- tempfile(), mode="wb")
alco11 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/ALQ_H.xpt", tf <- tempfile(), mode="wb")
alco13 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/ALQ_I.xpt", tf <- tempfile(), mode="wb")
alco15 <- foreign::read.xport(tf)

alco <- as.data.frame(rbind(alco03[,c('SEQN', 'ALQ101','ALQ120Q')],alco05[,c('SEQN', 'ALQ101','ALQ120Q')], alco07[,c('SEQN', 'ALQ101','ALQ120Q')],
              alco09[,c('SEQN', 'ALQ101','ALQ120Q')],alco11[,c('SEQN', 'ALQ101','ALQ120Q')],alco13[,c('SEQN', 'ALQ101','ALQ120Q')],
              alco15[,c('SEQN', 'ALQ101','ALQ120Q')]))

alco <- alco %>% mutate(past_yr_12drinks = case_when(ALQ101==1 ~ 1,
                                                     ALQ101==2 ~ 0,
                                                     TRUE ~ NA_real_),
                        num_day_alcohol = case_when(ALQ120Q < 366 ~ ALQ120Q,
                                                    TRUE ~ NA_real_)) %>% select(SEQN,past_yr_12drinks,num_day_alcohol )

# past_yr_12drinks: Had at least 12 alcohol drinks/1 yr?
# num_day_alcohol: How often drink alcohol over past 12 mos

data2 <- merge(data, alco, by='SEQN')


# Merging in smoking
download.file("https://wwwn.cdc.gov/nchs/nhanes/2003-2004/SMQ_C.xpt", tf <- tempfile(), mode="wb")
smoke03 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2005-2006/SMQ_D.xpt", tf <- tempfile(), mode="wb")
smoke05 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2007-2008/SMQ_E.xpt", tf <- tempfile(), mode="wb")
smoke07 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2009-2010/SMQ_F.xpt", tf <- tempfile(), mode="wb")
smoke09 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/SMQ_G.xpt", tf <- tempfile(), mode="wb")
smoke11 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/SMQ_H.xpt", tf <- tempfile(), mode="wb")
smoke13 <- foreign::read.xport(tf)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/SMQ_I.xpt", tf <- tempfile(), mode="wb")
smoke15 <- foreign::read.xport(tf)

smoke <- rbind(smoke03[,c('SEQN', 'SMQ020',"SMQ040")],smoke05[,c('SEQN', 'SMQ020',"SMQ040")],smoke07[,c('SEQN', 'SMQ020',"SMQ040")], 
               smoke09[,c('SEQN', 'SMQ020',"SMQ040")],smoke11[,c('SEQN', 'SMQ020',"SMQ040")],smoke13[,c('SEQN', 'SMQ020',"SMQ040")],
               smoke15[,c('SEQN', 'SMQ020',"SMQ040")])

# SMQ020:  Smoked at least 100 cigarettes in life 
# SMQ040: Do you now smoke cigarettes for those responded yes to SMQ020

smoke <- smoke%>%mutate(smoker=case_when(SMQ020 == 2 ~ 0, 
                                         SMQ020 == 1 & SMQ040 ==3 ~ 1, 
                                         SMQ020 == 1 & (SMQ040 ==1|SMQ040 ==2) ~ 2)) %>% select (SEQN, smoker)

# smoker
# 0 never smoker
# 1 ever smoker
# 2 current smoker

analysis <- merge(smoke, data2, by='SEQN')

analysis <- analysis %>% select(-preg, -test_preg,-rep_preg)
  
summary(analysis)
# outcome: aware: 1-aware of pregnancy, 0-unaware of pregnancy

# Note - YZ:
# most of key variables have complete data, while some variables (eg. estrogen use, hormone patch use)
# have many missing values (eg. 700 missing) - would recommend not to add these variables with much missingness to the model


# save(analysis, file='/Users/yuzhang/Documents/Study/Courses/G2-Spring/BST 263/Group Project/preg_aware_update0425.RData')
# load('/Users/yuzhang/Documents/Study/Courses/G2-Spring/BST 263/Group Project/preg_aware.RData')



