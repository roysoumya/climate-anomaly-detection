#Loading the 925hpa Anomaly database
complete_set = read.csv('~/R/cSTAG/Results/Implementation 925/Twitter_Anomaly_925hpa_Database.csv',header=TRUE,row.names = 1)

library(dplyr)

jan1stweek2000 = complete_set %>% filter(parameter == 'air_temp',country_code == 'IN',month_name=='JAN', year==2000)
