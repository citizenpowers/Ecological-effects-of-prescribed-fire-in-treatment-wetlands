#Import and tidy fire water quality data


library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readxl)

# Import Data -------------------------------------------------------------

#work links
LIMSP_Provisional_Data <- read_excel("//ad.sfwmd.gov/dfsroot/data/RSSI/MISC/Fire project/Data/Water Quality Data/LIMSP Provisional Data.xlsx")

find("read_excel")
find("write.csv")
# Tidy Data ---------------------------------------------------------------

LIMSP_Provisional_Data_Tidy1 <- LIMSP_Provisional_Data %>%
select(SAMPLE_ID,STATION,MATRIX,COLLECT_METHOD,SAMPLE_TYPE,COLLECT_DATE,DEPTH,FIRST_TRIGGER_DATE,TEST_NAME,VALUE,UNITS, SAMP_COMMENT_NR ) %>%  
mutate(TEST_NAME=if_else(TEST_NAME=="OPO4","SRP",TEST_NAME)) %>%                              #rename OPO4 to SRP
mutate(Treatment=case_when(str_detect(STATION,"Untreated")~"Untreated",                       #add treatment variable
                             str_detect(STATION,"Burn_Herb")~"Burn/Herbicide",
                             STATION %in% c("Burn_A","Burn_B","Burn_C")~"Burn",
                             str_detect(STATION,"Herbicide")~"Herbicide",
                             TRUE ~ NA)) %>%
  mutate(STATION=ifelse(str_detect(STATION,"Burn_Herb"),str_replace(STATION,"Burn_Herb","Burn/Herbicide"),STATION)) %>%  
  mutate(Block=case_when(STATION %in% c("Untreated_A","Burn_A","Herbicide_A","Burn/Herbicide_A")~"A",          #add block variable
                         STATION %in% c("Untreated_B","Burn_B","Herbicide_B","Burn/Herbicide_B")~"B",
                         STATION %in% c("Untreated_C","Burn_C","Herbicide_C","Burn/Herbicide_C")~"C",
                         TRUE ~ NA))  %>%
  mutate(FIRST_TRIGGER_DATE=mdy_hms(FIRST_TRIGGER_DATE)) %>%
  mutate(`Burn Day`= as.numeric(difftime(date(COLLECT_DATE),date("2025-04-10 14:00:00 UTC"),units="day"))) %>% #calculate days to burn
  mutate(`Date_Treatment`=paste(`Burn Day`," ",Treatment)) 

#Calculate DOP 
DOP <- LIMSP_Provisional_Data_Tidy1 %>%
filter(TEST_NAME %in% c("TDPO4","SRP","TPO4"),COLLECT_METHOD=="GP")  %>%
pivot_wider(names_from = "TEST_NAME",values_from = "VALUE")  %>%
filter(!is.na(TDPO4)) %>%  
mutate(DOP=TDPO4-SRP,PP=TPO4-TDPO4) %>%
pivot_longer(names_to = "TEST_NAME",values_to = "VALUE",15:19) %>%
filter(TEST_NAME %in% c("PP","DOP"))  

#Add DOP to dataset 
LIMSP_Provisional_Data_Tidy <- bind_rows(LIMSP_Provisional_Data_Tidy1,DOP) 

write_csv(LIMSP_Provisional_Data_Tidy ,"./Data/LIMSP_Provisional_Data_Tidy.csv")


