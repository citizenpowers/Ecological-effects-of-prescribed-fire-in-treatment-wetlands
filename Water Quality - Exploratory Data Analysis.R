#the objective of this script is to describe long-term water quality effects from prescribed fire.

library(tidyverse)
library(paletteer)

library(lme4)
library(rstatix)

# import data -------------------------------------------------------------

LIMSP_Provisional_Data_Tidy <-read_csv("./Data/LIMSP_Provisional_Data_Tidy.csv")
Flow_and_Stage <- read_csv("Data/Flow and Stage.csv")

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

flow_stage_wide <- Flow_and_Stage %>%
mutate(Date=as.Date(DAILY_DATE)) %>%
select(`Flow.way.Location`,Date,`Flow (cfs)`,`Stage (NAVD88 ft)`)  %>%
group_by(`Flow.way.Location`,Date) %>%
summarise(`Daily Flow`=sum(`Flow (cfs)`,na.rm=T),`Daily Stage`=sum(`Stage (NAVD88 ft)`,na.rm=T)) %>%
pivot_wider(names_from=`Flow.way.Location`, values_from = c(`Daily Flow`,`Daily Stage`)) %>%
select(where(~ !all(is.na(.)))) 

WQ_flow_stage <- Long_term_tidy %>%
left_join(flow_stage_wide,by="Date")


# test for differences between treatments ------------------
TP_mod_data <-filter(Long_term_tidy,TEST_NAME=="TPO4") %>%
mutate(Burn=if_else(Treatment %in% c("Burn","Burn/Herbicide"),"Yes","No")) %>%
mutate(Herbicide=if_else(Treatment %in% c("Herbicide","Burn/Herbicide"),"Yes","No"))

TP_LM <- lm(VALUE ~ Burn+Herbicide, data = TP_mod_data )   #simple linear model
summary(TP_LM )

TP_LM_int <- lm(VALUE ~ Burn*Herbicide, data = TP_mod_data )   #simple linear model with interaction
summary(TP_LM_int )

TP_LM_depth <- lm(VALUE ~ Burn+Herbicide+DEPTH, data = TP_mod_data )   #linear model depth
summary(TP_LM_depth)

TP_mm <- lmer(VALUE ~ Burn+Herbicide  + (1 | STATION), data = TP_mod_data) #mixed effects model with random intercept
summary(TP_mm)

TP_mm_int <- lmer(VALUE ~ Burn*Herbicide  + (1 | STATION), data = TP_mod_data) #mixed effects model with random intercept and interaction
summary(TP_mm_int)

Complete_trips <- TP_mod_data %>%
group_by(Date)  %>%
summarise(n=n())  

friedman_data <- TP_mod_data %>%
mutate(STATION = str_replace_all(STATION, "[^[:alnum:] ]", "_")) %>%
left_join(Complete_trips,by="Date") %>%
filter(n==12)
  
TP_friedman_result <- friedman.test(VALUE ~ Date | STATION, data = friedman_data )   #fails when there is missing data
friedman_effsize(VALUE ~ Date | STATION, data = friedman_data)
pairwise_diff <- pairwise_wilcox_test(VALUE ~  STATION,paired = TRUE, data = friedman_data, p.adjust.method = "bonferroni")

TP_aov <- aov(VALUE~Treatment,data=TP_mod_data)
summary(TP_aov)


# Figures -----------------------------------------------------------------


# QC figures --------------------------------------------------------------
QC_Data <-  LIMSP_Provisional_Data_Tidy %>%
filter(`COLLECT_DATE`> "2025-01-01",MATRIX=="DI",TEST_NAME %in% c("TPO4","TDPO4","SRP"))

#Blanks time series 
ggplot(QC_Data ,aes(`COLLECT_DATE`,VALUE,color=TEST_NAME,fill=TEST_NAME))+coord_cartesian(ylim=c(0,0.005))+
geom_point(shape=21,size=3)+facet_wrap(year(COLLECT_DATE)~TEST_NAME,scales="free_x",nrow=2)+scale_fill_paletteer_d("nbapalettes::heat_vice") +
scale_color_paletteer_d("nbapalettes::heat_vice")+labs(y="Concentration (mg/L)")+theme_bw()


# P Time series -----------------------------------------------------------

#P forms  post-burn
ggplot(filter(Long_term_tidy,TEST_NAME %in% c("DOP","PP","SRP","TPO4")),aes(`COLLECT_DATE`,VALUE,color=Treatment,fill=Treatment,shape=Treatment))+
geom_point()+geom_smooth(se=F)+facet_wrap(Year~TEST_NAME,scales="free_x",nrow=2)  +theme_bw()

#P forms difference from daily mean
ggplot(filter(Difference_from_daily_mean,TEST_NAME %in% c("DOP","PP","SRP","TPO4")),aes(`COLLECT_DATE`,`Diff from daily mean`,color=Treatment,fill=Treatment,shape=Treatment))+
geom_hline(yintercept = 0)+geom_point()+geom_smooth(se=F)+facet_wrap(Year~TEST_NAME,scales="free_x",nrow=2)+coord_cartesian(ylim = c(-.015,.015))+theme_bw()

# Depth figs --------------------------------------------------

#Depth vs Concentration
ggplot(filter(Long_term_tidy,TEST_NAME %in% c("DOP","PP","SRP","TPO4")),aes(DEPTH,VALUE,color=Treatment,fill=Treatment,shape=Treatment))+
geom_point()+geom_smooth(se=F)+facet_wrap(Year~TEST_NAME,nrow=2)+geom_smooth(aes(DEPTH,VALUE),color="grey20",fill="grey20",se=F,inherit.aes = F)  +theme_bw()

#Stage time series
ggplot(Flow_and_Stage,aes(DAILY_DATE,`Stage (NAVD88 ft)`,color=STATION,fill=STATION))+geom_line()+geom_point()+facet_wrap(~Flow.way.Location,nrow=3) +theme_bw()


# Flow figs ---------------------------------------------------

#Flow time series all stations
ggplot(Flow_and_Stage,aes(DAILY_DATE,`Flow (cfs)`,color=SITE,fill=SITE))+geom_line()+geom_point()+facet_wrap(~Flow.way.Location,nrow=3)+
theme_bw()+facet_wrap(Flow.way.Location~year(DAILY_DATE),nrow=3)

#flow vs Concentration outflow
ggplot(filter(WQ_flow_stage,TEST_NAME %in% c("DOP","PP","SRP","TPO4")),aes(`Daily Flow_Outflow`,VALUE,color=Treatment,fill=Treatment,shape=Treatment))+
geom_point()+geom_smooth(se=F)+facet_wrap(Year~TEST_NAME,nrow=2)+geom_smooth(aes(`Daily Flow_Outflow`,VALUE),color="grey20",fill="grey20",se=F,inherit.aes = F)  +theme_bw()

#flow vs Concentration midlfow
ggplot(filter(WQ_flow_stage,TEST_NAME %in% c("DOP","PP","SRP","TPO4")),aes(`Daily Flow_Midflow`,VALUE,color=Treatment,fill=Treatment,shape=Treatment))+
geom_point()+geom_smooth(se=F)+facet_wrap(Year~TEST_NAME,nrow=2) +theme_bw()

#flow vs Concentration Inlfow
ggplot(filter(WQ_flow_stage,TEST_NAME %in% c("DOP","PP","SRP","TPO4")),aes(`Daily Flow_Inflow`,VALUE,color=Treatment,fill=Treatment,shape=Treatment))+
geom_point()+geom_smooth(se=F)+facet_wrap(Year~TEST_NAME,nrow=2) +theme_bw()




