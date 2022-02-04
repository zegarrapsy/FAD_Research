#--------------------/ Demographic associated factors of depression among adults in 
#--------------------/ Peru during the COVID-19 pandemic: An exploratory analysis
#--------------------/ Data selection procedures

#--------------------/ Settings
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
options(scipen = 999)

#--------------------/ Packages
library(tidyverse)
library(haven)

#--------------------/ Health Questionnaire
data_sm <- read_sav("Data/CSALUD01.sav") %>% filter(QSRESULT==1) %>%
           mutate(id = paste0(HHID,QS20C)) %>% 
           select(HHID, QSSEXO, QS23, QS25N, QS25AA, starts_with("QS700"), QS709,QS26) %>% 
           rename(sexo    = QSSEXO,
                  edad    = QS23,
                  neduca  = QS25N,
                  lengua  = QS25AA,
                  phq9_01 = QS700A,
                  phq9_02 = QS700B,
                  phq9_03 = QS700C,
                  phq9_04 = QS700D,
                  phq9_05 = QS700E,
                  phq9_06 = QS700I,
                  phq9_07 = QS700F,
                  phq9_08 = QS700G,
                  phq9_09 = QS700H,
                  casado  = QS709,
                  seguro  = QS26)

#--------------------/ Household Questionnaie
data_rg <- read_sav("Data/RECH0.sav") %>% select(HHID,HV024,HV025,HV009) %>% 
           rename(region = HV024, area = HV025, n_per = HV009) 

data_ir <- read_sav("Data/RECH23.sav") %>% select(HHID,HV270,HV271,SHREGION) %>% 
           rename(iri_q = HV270, iri_s = HV271, region2= SHREGION)


#--------------------/ Merge data
final <- data_ir %>% left_join(data_rg,by="HHID")
final <- data_sm %>% left_join(final,by="HHID")

#--------------------/ Exclude missing data
final <- final %>% na.omit()

#--------------------/ Filter data
final <- final %>% filter(lengua<11) %>% 
                   mutate(n_per_cat = ifelse(n_per==1, 1, ifelse(n_per>1 & n_per<5,2,3)),
                          edad_cat = ifelse(edad<25, 1, ifelse(edad>24 & edad<40,2,ifelse(edad>39 & edad<60,3,4))),
                          region2_cat = ifelse(region2==1|region2==2, 1, ifelse(region2==3,2,3)),
                          lengua_cat = ifelse(lengua==10,1,2),
                          neduca_cat = ifelse(neduca==0|neduca==1,1,ifelse(neduca==2,2, ifelse(neduca==3,3,4))),
                          edad = as.numeric(edad)) 

#--------------------/ Depression categories
final <- final %>% mutate(total_depre = phq9_01+phq9_02+phq9_03+phq9_04+phq9_05+phq9_06+phq9_07+phq9_08+phq9_09)
final <- final %>% mutate(category = as.factor(case_when(total_depre <= 4 ~ 0,
                                                         total_depre <= 9 ~ 1,
                                                         total_depre <= 14 ~ 2,
                                                         total_depre <= 19 ~ 3,
                                                         total_depre <= 27 ~ 4))) 
levels(final$category) <- c("None","Mild","Moderate","Moderately severe","Severe")

#--------------------/ Export data
write_sav(final,"Data/depression_data.sav")


