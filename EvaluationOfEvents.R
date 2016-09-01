

#For easier query-checking we add a column named 'day_no'
month_days = c(31,28,31,30,31,30,31,31,30,31,30,31)
day_no = NULL

for (i in 1951:2014) {
  
  #Checking if it is a leap year or not
  if(i %% 400 == 0){
    month_days[2] = 29
  }
  else{
    if(i %% 100 != 0 && i %% 4 == 0){
      month_days[2] = 29
    }
    else{
      month_days[2] = 28
    }
  }
  
  for (j in 1:12) {
    for (k in 1:month_days[j]) {
      day_no = c(day_no , rep(k,4))
    }
  }
}

index = seq(1,93504,1)
tab1 = cbind(index,day_no)

com_data = merge(complete_data,tab1,by='index',all.x = TRUE)
write.csv(com_data,file = 'Twitter_Anomaly_925hpa_Database.csv')

tab2 = tab1
colnames(tab2) = c('start_index','start_day')
sp_temp_cluster = merge(sp_temp_cluster ,tab2,by='start_index',all.x = TRUE)
write.csv(sp_temp_cluster , file = 'Spatio-Temporal Anomaly Clusters.csv')

#Preprocessing ends

library(dplyr)

complete_data = read.csv('~/R/cSTAG/Results/Implementation 925/Twitter_Anomaly_925hpa_Database.csv', header = TRUE,row.names = 1)
sp_temp_cluster = read.csv('~/R/cSTAG/Results/Implementation 925/Spatio-Temporal Anomaly Clusters.csv',header =  TRUE,row.names = 1)



#Subsetting the data for the months having the number of air_temp anomalies >= 200
isel_anomaly_monthgreater200 = complete_data %>% filter(parameter=='air_temp' , month >= 3) %>% filter(month<7) %>% group_by(month,year) %>% summarise(total = n()) %>% filter(total>=200)%>% arrange(desc(total))
write.csv(isel_anomaly_monthgreater200 , file='Air_Temp_Anomaly_Months_200.csv')

#Subsetting the data for the months having the number of air_temp anomalies >= 200
isel_anomaly_monthgreater200 = complete_data %>% filter(parameter=='air_temp' , month > 3) %>% filter(month<7) %>% group_by(month,year) %>% summarise(total = n()) %>% filter(total>=200)%>% arrange(desc(total))
write.csv(isel_anomaly_monthgreater200 , file='Air_Temp_Anomaly_Months_200.csv')

#Table for dealing with cold waves
cold_wave_anomaly_check = complete_data %>% filter(country_code == 'IN',parameter %in% c('air_temp','omega') , month %in% c(1,2,12))  %>% group_by(month,year) %>% summarise(total = n()) %>%  arrange(desc(total))

#Table dealing with omega
omega_anomaly_check = complete_data %>% filter(country_code == 'IN',parameter == 'omega')  %>% group_by(month,year) %>% summarise(total = n()) %>%  arrange(desc(total))

#Table delaing with rel_humid
rel_humid_anomaly_check = complete_data %>% filter(country_code == 'IN',parameter == 'rel_humid')  %>% group_by(month,year) %>% summarise(total = n()) %>%  arrange(desc(total))


#Table delaing with uwind
uwind_anomaly_check = complete_data %>% filter(country_code == 'IN',parameter == 'uwind')  %>% group_by(month,year) %>% summarise(total = n()) %>%  arrange(desc(total))


#Table delaing with uwind
vwind_anomaly_check = complete_data %>% filter(country_code == 'IN',parameter == 'vwind')  %>% group_by(month,year) %>% summarise(total = n()) %>%  arrange(desc(total))

#For checking tropical cyclones over Bangladesh
vwind_anomaly_check_bn = complete_data %>% filter(country_code == 'BD')  %>% group_by(month,year) %>% summarise(total = n()) %>%  arrange(desc(total))

#Writing to .csv files
write.csv(cold_wave_anomaly_check , file='Cold_Wave_Anomaly_Months.csv')
write.csv(omega_anomaly_check , file='Omega_Anomaly_Months.csv')
write.csv(rel_humid_anomaly_check  , file='Rel_Humid_Anomaly_Months.csv')
write.csv(uwind_anomaly_check , file='UWind_Anomaly_Months.csv')
write.csv(vwind_anomaly_check , file='VWind_Anomaly_Months.csv')

library(rworldmap)
mapGriddedData(xlim = c(65.0,100.0), ylim = c(5.0,40.0))
