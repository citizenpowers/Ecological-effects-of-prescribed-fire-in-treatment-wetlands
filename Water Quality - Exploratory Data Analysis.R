#the objective of this script is to describe long-term water quality effects from prescribed fire.

library(tidyverse)



# import data -------------------------------------------------------------

LIMSP_Provisional_Data_Tidy <-read_csv("./Data/LIMSP_Provisional_Data_Tidy.csv")


# Tidy Data ---------------------------------------------------------------

Long_term_tidy <- LIMSP_Provisional_Data_Tidy %>%
filter(`COLLECT_DATE`> "2025-06-01",MATRIX=="SW") %>% 
filter(VALUE<.07) %>% #removing outliers  
mutate(Year=year(`COLLECT_DATE`),Date=as.Date(`COLLECT_DATE`))  %>%
mutate(DEPTH=if_else(Date=="2026-06-16" & STATION=="Burn_B",0.09,DEPTH) )  

Daily_mean <- LIMSP_Provisional_Data_Tidy %>%
filter(`COLLECT_DATE`> "2025-06-01",MATRIX=="SW") %>%
filter(VALUE<.07) %>% #removing outliers 
mutate(Date=as.Date(`COLLECT_DATE`))  %>%
mutate(DEPTH=if_else(Date=="2026-06-16" & STATION=="Burn_B",0.09,DEPTH) ) %>%
group_by(Date,TEST_NAME) %>%
summarise(`daily mean`=mean(VALUE))

Difference_from_daily_mean <- Long_term_tidy %>%
left_join(Daily_mean,by=c("Date","TEST_NAME")) %>%
rowwise() %>%
mutate(`Diff from daily mean`=VALUE-`daily mean`)  


# Figures -----------------------------------------------------------------

#P forms  post-burn
ggplot(filter(Long_term_tidy,TEST_NAME %in% c("DOP","PP","SRP","TPO4")),aes(`COLLECT_DATE`,VALUE,color=Treatment,fill=Treatment,shape=Treatment))+
geom_point()+geom_smooth(se=F)+facet_wrap(Year~TEST_NAME,scales="free",nrow=2)  +theme_bw()

#P forms difference from daily mean
ggplot(filter(Difference_from_daily_mean,TEST_NAME %in% c("DOP","PP","SRP","TPO4")),aes(`COLLECT_DATE`,`Diff from daily mean`,color=Treatment,fill=Treatment,shape=Treatment))+
geom_hline(yintercept = 0)+geom_point()+geom_smooth(se=F)+facet_wrap(Year~TEST_NAME,scales="free",nrow=2)  +theme_bw()

#Depth vs Concentration
ggplot(filter(Long_term_tidy,TEST_NAME %in% c("DOP","PP","SRP","TPO4")),aes(DEPTH,VALUE,color=Treatment,fill=Treatment,shape=Treatment))+
geom_point()+geom_smooth(se=F)+facet_wrap(Year~TEST_NAME,nrow=2)+geom_smooth(aes(DEPTH,VALUE),color="grey20",fill="grey20",se=F,inherit.aes = F)  +theme_bw()


