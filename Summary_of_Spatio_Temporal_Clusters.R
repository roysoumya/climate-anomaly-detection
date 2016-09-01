library(dplyr)

complete_data = read.csv('~/R/cSTAG/Results/Implementation 925/Twitter_Anomaly_925hpa_Database.csv', header = TRUE,row.names = 1)
sp_temp_cluster = read.csv('~/R/cSTAG/Results/Implementation 925/Spatio-Temporal Anomaly Clusters.csv',header =  TRUE,row.names = 1)
checkAnomalyTable = sp_temp_cluster %>% group_by(parameter,start_month,start_year) %>% summarise(total = n()) %>% arrange(desc(total))
checkRelHumid = checkAnomalyTable %>% filter(parameter == 'rel_humid')
checkAirTemp = checkAnomalyTable %>% filter(parameter == 'air_temp')
checkOmega = checkAnomalyTable %>% filter(parameter == 'omega')
checkUwind = checkAnomalyTable %>% filter(parameter == 'uwind')
checkVwind = checkAnomalyTable %>% filter(parameter == 'vwind')

#Introducing for each spatio-temporal cluster the number of anomalies of each of the 4 paraeters
#Then we perform inner join with complete_data
mod_data = complete_data %>% arrange(temp_sp_cluster_no)
sum_data1 = complete_data %>% group_by(temp_sp_cluster_no,parameter) %>% summarise(start_index = min(index),start_month = min(month) ,start_year = min(year) , size = n()) %>% arrange(desc(size))

overall_cluster_no = seq(1,nrow(sum_data1),1)
sum_data2 = cbind(sum_data1,as.data.frame(overall_cluster_no))


air_temp_anom_freq = NULL
uwind_anom_freq = NULL
vwind_anom_freq = NULL
rel_humid_anom_freq = NULL
length_cnt =NULL

for (i in 1:20432) {
  part_data = filter(mod_data ,temp_sp_cluster_no == i )
  length_cnt = c(length_cnt , nrow(part_data))
  
  temp_cnt=0
  uwind_cnt=0
  vwind_cnt=0
  rel_humid_cnt=0
  
  for (j in 1:nrow(part_data)) {
    if(part_data[j,4] != 0){
      temp_cnt = temp_cnt + 1
    }
    if(part_data[j,5] != 0 ){
      uwind_cnt = uwind_cnt + 1
    }
    if(part_data[j,6] != 0.0){
      rel_humid_cnt = rel_humid_cnt + 1
    }
    if(part_data[j,7] != 0.0){
      vwind_cnt = vwind_cnt + 1
    }
  }
  
  air_temp_anom_freq = c(air_temp_anom_freq,temp_cnt)
  uwind_anom_freq = c(uwind_anom_freq,uwind_cnt)
  vwind_anom_freq = c(vwind_anom_freq, vwind_cnt)
  rel_humid_anom_freq = c(rel_humid_anom_freq,rel_humid_cnt)

  
}


sp_temp_sum = cbind(sum_data1,air_temp_anom_freq,uwind_anom_freq,rel_humid_anom_freq,vwind_anom_freq)

#Revised summarisation database for more detailed understanding of the results
write.csv(sp_temp_sum,file = 'Spatio-Temporal Anomaly Clusters.csv')

#sum_data1 = complete_data %>% group_by(temp_sp_cluster_no) %>% summarise(start_month = min(month) ,start_year = min(year) , total = n()) %>% arrange(desc(total))

month_sum = sp_temp_sum %>% group_by(start_month , start_year) %>% summarise(total_anom_no = sum(total) ,air_temp_anom_freq = sum(air_temp_anom_freq), uwind_anom_freq = sum(uwind_anom_freq), vwind_anom_freq = sum(vwind_anom_freq),rel_humid_freq = sum(rel_humid_anom_freq) ,freq = n()) %>% arrange(desc(total_anom_no))

year_sum = sp_temp_sum %>% group_by(start_year) %>% summarise(total_anom_no = sum(total) ,air_temp_anom_freq = sum(air_temp_anom_freq), uwind_anom_freq = sum(uwind_anom_freq), vwind_anom_freq = sum(vwind_anom_freq),rel_humid_freq = sum(rel_humid_anom_freq) ,freq = n()) %>% arrange(desc(total_anom_no))

#Writing month-wise and year-wise summary report
write.csv(month_sum,file = '300hpa_Monthwise_Spatio_Temporal_Summary.csv')
write.csv(year_sum,file = '300hpa_Yearwise_Spatio_Temporal_Summary.csv')


#Start summarising based on the spatio-temporal clusters

temp_cnt=0
uwind_cnt=0
vwind_cnt=0
rel_humid_cnt=0

sum_data1 = complete_data %>% group_by(temp_sp_cluster_no) %>% summarise(start_month = min(month) ,start_year = min(year) , total = n()) %>% arrange(desc(total))

month_sum = sum_data1 %>% group_by(start_month , start_year) %>% summarise(total_anom_no = sum(total) ,freq = n()) %>% arrange(desc(total_anom_no))

year_sum = sum_data1 %>% group_by(start_year) %>% summarise(total_anom_no = sum(total) ,freq = n()) %>% arrange(desc(total_anom_no))


#Old algorithm for finding summaries for spatio-temporal points
for (i in 1:144278) {
  if(i != 144278 && mod_data[i,22] == mod_data[i+1,22]){
    if(mod_data[i,4] > 0){
      temp_cnt = temp_cnt + 1
    }
    if(mod_data[i,5] > 0 ){
      uwind_cnt = uwind_cnt + 1
    }
    if(mod_data[i,6] > 0.0){
      rel_humid_cnt = rel_humid_cnt + 1
    }
    if(mod_data[i,7] > 0.0){
      vwind_cnt = vwind_cnt + 1
    }
  }
  else{
    
    if(mod_data[i,4] != 0){
      temp_cnt = temp_cnt + 1
    }
    if(mod_data[i,5] != 0 ){
      uwind_cnt = uwind_cnt + 1
    }
    if(mod_data[i,6] != 0.0){
      rel_humid_cnt = rel_humid_cnt + 1
    }
    if(mod_data[i,7] != 0.0){
      vwind_cnt = vwind_cnt + 1
    }
    
    air_temp_anom_freq[curr_cnt] = temp_cnt
    uwind_anom_freq[curr_cnt] = uwind_cnt
    vwind_anom_freq[curr_cnt] = vwind_cnt
    rel_humid_anom_freq[curr_cnt] = rel_humid_cnt
    
    #Zeroing the parameters for the next iteration
    temp_cnt=0
    uwind_cnt=0
    vwind_cnt=0
    rel_humid_cnt=0
    
    #Incrementing the value of the count of the spatio-temporal cluster
    curr_cnt = curr_cnt + 1
  }
}
