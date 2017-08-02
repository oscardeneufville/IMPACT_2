### testing a script to scale the physical activity variables in the IMPACt wide data ###
## load packages
library(tidyverse)
library(readr)
#importing the IMPACT wide data
IMPACT_wide <- read_csv("~/Dropbox/Work/WFBMC/Stats/R Projects/IMPACT_2/Data/IMPACT.wide 2.15.17.csv")
View(IMPACT_wide)
names(IMPACT_wide)

##creating scaled LPA and MVPA variables in the wide dataset
test_wide_scaled <- IMPACT_wide %>%
  select(ID:DeltaM12PHS, -(CHAMPSMVTotal:CHAMPSWalk12MTotal25262728), WThr:PA12Mcatc)%>%
  filter(ValidDays_BL > 0)%>%
  mutate(LPAstd = (LPATotal/ValidDays_BL)*7)%>%
  filter(ValidDays_3M > 0)%>%
  mutate(LPA3Mstd = (LPA3MTotal/ValidDays_3M)*7) %>%
  filter(ValidDays_12M >0)%>%
  mutate(LPA12Mstd = (LPA12MTotal/ValidDays_12M)*7) %>%
  mutate(MVPAstd = (MVPATotal/ValidDays_BL)*7) %>%
  mutate(MVPA3Mstd = (MVPA3MTotal/ValidDays_3M)*7) %>%
  mutate(MVPA12Mstd = (MVPA12MTotal/ValidDays_12M)*7)

## creating 400m gait speed variables in the test_wideGS dataset
test_wideGS <- test_wide_scaled %>%
  mutate(Treatment = factor(Treatment, labels = c("GMCB", "Traditional")))%>%
  mutate(Gender = factor(Gender, labels = c("Male", "Female"))) %>%
  mutate(GS.400 = (400/W400M))%>%
  mutate(GS.400M3 = (400/W400M3))%>%
  mutate(GS.400M12 = (400/W400M12))

### creating categorical variables for Baseline gait speed ###
### Gaitspeed category is based on references in M Miller paper###
### 3 categories chosen, < 0.8 m/s, 0.8-1.0 m/s and >1.0 m/s###

test_wideGS$GS.cat <- cut(test_wideGS$GS.400, c(0, 0.8, 1.0, 1.68), labels= F)
test_wideGS$GS.3Mcat <- cut(test_wideGS$GS.400M3, c(0, 0.8, 1.0, 1.59), labels=F)
test_wideGS$GS.12Mcat <- cut(test_wideGS$GS.400M12, c(0, 0.8, 1.0, 1.68), labels=F)
names(test_wideGS)

### creating tables of gait speed categories ###
table(test_wideGS$GS.cat)
table(test_wideGS$GS.3Mcat)

## examining tables of gaitspeed by LPA, MVPA and PA.Level
table(test_wideGS$GS.cat, test_wideGS$LPAcatB)
table(test_wideGS$GS.cat, test_wideGS$MVPAcat)
table(test_wideGS$GS.cat, test_wideGS$PAcatc)

#Creating tibbles for each outcomes of interest
#LPA by baseline gaitspeed category
IMP_lpags <- test_wideGS%>%
  select(ID:BMI, LPAstd:TotalPA12Mstd, GS.400:GS.12Mcat) %>% #select relevant variables
  filter(ID != 11, ID != 76)%>% # removing subjects 11 and 76 who had extremely high levels of activity
  na.omit()%>% #removing the NA's
  group_by(Treatment, GS.cat)%>%
  summarise(LPA.s_mean = mean(LPAstd), #adding a data point for the mean by GS.cat
            LPA.s_ci = 1.96 * sd(LPAstd)/sqrt(n())) # adding 95% CI's

# removing outliers
MP_lpags2 <- test_wideGS%>%
  select(ID:BMI, LPAstd:TotalPA12Mstd, GS.400:GS.12Mcat) %>% #select relevant variables
  filter(ID != 11, ID != 76)%>% # removing subjects 11 and 76 who had extremely high levels of activity
  na.omit()%>% #removing the NA's
  group_by(Treatment, GS.cat)%>%
  summarise(LPA.s_mean = mean(LPAstd), #adding a data point for the mean by GS.cat
            LPA.s_ci = 1.96 * sd(LPAstd)/sqrt(n())) # adding 95% CI's

#MVPA by baseline gaitspeed category
IMP_mvpags <- test_wideGS%>%
  select(ID:BMI, LPAstd:TotalPA12Mstd, GS.400:GS.12Mcat) %>% #select relevant variables
  na.omit()%>% #removing the NA's
  group_by(Treatment, GS.cat)%>%
  summarise(MVPA.s_mean = mean(MVPAstd), #adding a data point for the mean by GS.cat
            MVPA.s_ci = 1.96 * sd(MVPAstd)/sqrt(n())) # adding 95% CI'stable(test_wideGS$GS.12Mcat)



##creating scaled LPA and MVPA variables in the long dataset
test_long_scaled <- IMPACT_long %>%
  select(pid:TotalPAstd)%>%
  filter(ValidDays > 0)%>%
  mutate(LPAstd = (LPA/ValidDays)*7)%>%
  mutate(MVPAstd = (MVPA/ValidDays)*7) %>%
  mutate(TotalPAstd = (TotalPA/ValidDays)*7)