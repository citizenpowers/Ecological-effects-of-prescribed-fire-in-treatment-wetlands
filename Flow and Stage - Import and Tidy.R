#This script downloads flow and stage data, then tidies data
#requires district network  connection to work. 


library(dplyr)
library(lubridate)
library(rvest)
library(stringr)
library(readr)
library(purrr)
library(odbc)
library(DBI)
library(glue)




Inflow_keys <- data.frame(`Flow-way Location`="Inflow",
                          Station=c("G380A_C", "G380B_C", "G380C_C", "G380D_C", "G380E_C" ,"G380F_C"),
                          SITE=c("G380A", "G380B", "G380C", "G380D", "G380E" ,"G380F"),
                          DBkey=c(91145,91146,91147, 91148, 91149,91150))

Midflow_keys <- data.frame(`Flow-way Location`="Midflow",
                           Station=c("G384A_C", "G384B_C", "G384C_C", "G384D_C", "G384E_C" ,"G384F_C"),
                           SITE=c("G384A", "G384B", "G384C", "G384D", "G384E" ,"G384F"),
                           DBkey=c(91160,91161,91162,91163,91164,91165))

Outflow_keys <- data.frame(`Flow-way Location`="Outflow",
                           Station=c("G381A_C", "G381B_C", "G381C_C", "G381D_C", "G381E_C" ,"G381F_C"),
                           SITE=c("G381A", "G381B", "G381C", "G381D", "G381E" ,"G381F"),
                           DBkey=c(91151,91152,91153, 91154, 91155,91156))


Inflow_stage_keys <-  data.frame(`Flow-way Location`="Inflow",
                                 Station=c("G380B_H", "G380B_T", "G380E_H", "G380E_T"),
                                 DBkey=c("T9931","T9932","T9997","T9998"))

Midflow_stage_keys <-  data.frame(`Flow-way Location`="Midflow",
                                  Station=c("G384C_H", "G384C_T"),
                                  DBkey=c("VV479","VV481"))

Outflow_stage_keys <-  data.frame(`Flow-way Location`="Outflow",
                                  Station=c("G381B_H", "G381B_T", "G381E_H", "G381E_T"),
                                  DBkey=c( "T1061", "T1063", "T1068", "T1070"))

Flow_keys <- bind_rows(Inflow_keys,Midflow_keys,Outflow_keys)

Stage_keys <- bind_rows(Inflow_stage_keys,Midflow_stage_keys,Outflow_stage_keys)

# Open connection and download data ---------------------------------------

con <- dbConnect(odbc::odbc(), UID = "pub", PWD = "pub", "wrep")                  #Open connection to wrep

db_flow_keys <- as.character(Flow_keys$DBkey)                                     #List flow stations to be downloaded

flow_query <- glue_sql("SELECT V_KEYWORD_TAB_DAILY.*
FROM V_KEYWORD_TAB_DAILY
WHERE V_KEYWORD_TAB_DAILY.DBKEY IN ({db_keys*})
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


# Tidy stage and flow -----------------------------------------------------

flow_and_stage_data <- flow_data  %>%
  select(DAILY_DATE,SITE,VALUE)  %>% rename("Flow (cfs)"=VALUE) %>%
  left_join(Stage_data %>% select(DAILY_DATE,SITE,VALUE )  %>% rename("Stage (NAVD88 ft)"=VALUE),by=c("DAILY_DATE","SITE"))  %>%
  left_join(select(Flow_keys,SITE,Flow.way.Location),by="SITE" ) 


# Save Date ---------------------------------------------------------------

write.csv(flow_and_stage_data,file="./Data/Flow and Stage/Flow and Stage.csv")

