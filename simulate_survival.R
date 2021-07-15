#sample augmentation with censoring cases

#rationale: the all-death registry data is missing recent patients who are still surviving 

#simulation parameter table is built based on national statistics on k-year survival rates: 
# 1-3 year: 50% 
# 3-5 year: 30%
# 5-10 year: 20%

install.packages("tidyverse")
install.packages("survminer")
library(tidyverse)
library(readxl)
library(magrittr)
library(survminer)

#load data fron one-drive
onedrive_root<-file.path("C:","Users",
                         "xsm7f" #user1
                         # "sxing" #user2
                        )

alsa_dat<-read_xlsx(file.path(onedrive_root,
                              "OneDrive - University of Missouri",
                              "#Grants",
                              "Analyses",
                              "ALS_registry_data",
                              "ALSA Mid America dataset 112520.xlsx")) %>%
  select(`Diagnosis to death (m)`) %>%
  mutate(row_id=1:n(),
         status=1) %>%
  rename(time=`Diagnosis to death (m)`)

survfit(Surv(time, status) ~ 1, data = alsa_dat)

yr<-c(2,3,5)
surv<-c(0.5,0.3,0.2)
time_rg<-list(c(1,4),c(3,6),c(5,11))
N<-nrow(alsa_dat)

alsa_sim<-c()
n0<-N
for(i in seq_along(yr)){
  n<-floor(yr[i]/10*N*surv[i]/(1-surv[i]))
  alsa_sim %<>%
    bind_rows(data.frame(row_id=(n0+1):(n0+n),
                         time=sample(time_rg[[i]][1]:time_rg[[i]][2],n,replace=T)*12))
  n0<-max(alsa_sim$row_id)
}

alsa_aug<-alsa_dat %>%
  bind_rows(alsa_sim %>% mutate(status=0)) %>%
  select(row_id,status,time)

#sanity check
ggsurvplot(
  fit = survfit(Surv(time, status) ~ 1, data = alsa_aug), 
  xlab = "Months", 
  ylab = "Overall survival probability")

survfit(Surv(time, status) ~ 1, data = alsa_aug)

#save augmented sample
write.csv(alsa_aug,file="./alsa_augmented.csv",row.names = F)
  

