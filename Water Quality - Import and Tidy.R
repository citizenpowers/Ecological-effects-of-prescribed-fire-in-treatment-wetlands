#Import and tidy fire water quality data


library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readxl)


library(odbc)
library(DBI)
library(glue)



# Open connection and download data from ERDP (DOES not work)---------------------------------------

con <- dbConnect(odbc::odbc(), UID = "???", PWD = "???", "erdp")                  #Open connection to wrep

db_flow_keys <- as.character(Flow_keys$DBkey)                                     #List flow stations to be downloaded

flow_query <- glue_sql("SELECT V_KEYWORD_TAB_DAILY.*
FROM V_KEYWORD_TAB_DAILY
WHERE V_KEYWORD_TAB_DAILY.DBKEY IN ({db_flow_keys*})
AND V_KEYWORD_TAB_DAILY.DAILY_DATE <= CURRENT_DATE
AND V_KEYWORD_TAB_DAILY.DAILY_DATE >= TO_DATE ('11/01/2024', 'MM/DD/YYYY')
AND V_KEYWORD_TAB_DAILY.STATISTIC_TYPE IN ('SUM', 'MEAN')",.con=con)             

flow_data <- DBI::dbGetQuery(con,flow_query)                                            #Query DB for flow

db_stage_keys <- as.character(Stage_keys$DBkey)                                         #List stage stations to be downloaded

stage_query <- glue_sql("SELECT V_KEYWORD_TAB_DAILY.*
FROM V_KEYWORD_TAB_DAILY
WHERE V_KEYWORD_TAB_DAILY.DBKEY IN ({db_stage_keys*})
AND V_KEYWORD_TAB_DAILY.DAILY_DATE <= CURRENT_DATE
AND V_KEYWORD_TAB_DAILY.DAILY_DATE >= TO_DATE ('11/01/2024', 'MM/DD/YYYY')
AND V_KEYWORD_TAB_DAILY.STATISTIC_TYPE IN ('SUM', 'MEAN')",.con=con) 

Stage_data <- DBI::dbGetQuery(con,stage_query)                                          #Query DB for flow

dbDisconnect(con)                                                                       #close connection






















# Import Data -------------------------------------------------------------

#work links
LIMSP_Provisional_Data <- read_excel("./Data/LIMSP Provisional Data.xlsx")

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


