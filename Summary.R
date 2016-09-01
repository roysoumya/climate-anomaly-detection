library(dplyr)

air_data_2d = read.csv(file = 'Air_TwitterAnomalyDetection.csv', header=TRUE)
rel_humid_data_2d = read.csv(file = 'Rel_Humid_TwitterAnomalyDetection.csv', header = TRUE)
uwind_data_2d = read.csv(file = 'UWind_TwitterAnomalyDetection.csv', header = TRUE)
vwind_data_2d = read.csv(file = 'VWind_TwitterAnomalyDetection.csv', header= TRUE)

air_meets_uwind = merge(air_data_2d,uwind_data_2d, by=c("grid_name","index"),all=TRUE)
names(air_meets_uwind)
air_meets_uwind = select(air_meets_uwind , -X.x, -X.y)

air3 = merge(air_meets_uwind,rel_humid_data_2d, by=c("grid_name","index"),all=TRUE)
names(air3)
air3 = select(air3 , -X)

air4 = merge(air3,vwind_data_2d, by=c("grid_name","index"),all=TRUE)
names(air4)
air4 = select(air4 , -X)

write.csv(air4 , file="Total_Twitter_Data_Old.csv", na=0)

#Extracting Month and year from Air land Data
setwd("~/R/KGP/Land Datasets")
read_data = read.csv(file = 'Air_Land.csv', header=TRUE)
calendar_data_15340 = select(read_data,X,Month,Year)

write.csv(calendar_data_15340 , file="Calendar_Daily_15340.csv")

#Reading calendar data and adding Month name
calendar_data = read.csv(file = 'Calendar_Daily_15340.csv',header = TRUE)
month_name = c('JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEPT','OCT','NOV','DEC')
month_day = select(calendar_data,month)

days_name = NULL
for (i in 1:15340) {
  cnt = month_day[i,1] %% 12
  if(cnt == 0){
    days_name[i] = month_name[12]
  } 
  else{
    days_name[i] = month_name[cnt]
  }
}
calendar_data = cbind(calendar_data,days_name)
write.csv(calendar_data , file="Calendar_Daily_15340.csv")

setwd('~/R/KGP/Twitter Anomaly Detection')
total_data = read.csv(file="Total_Twitter_Data_Old.csv", header = TRUE, row.names = 1)

write.csv(total_data , file="Total_Twitter_Data_Old.csv", na="0")
write.csv(sum_tab1, file = "Total_Twitter_plus_Calendar.csv")


#Calendar complete
#Now adding the name to the grids
#Already extracted using the Python Script: reverse_geocoding_large.py
#The location grids are stored in Location_Grid.csv

#Cleaning the Location_Grid.csv
location_grid = read.csv('~/R/Twitter India/Utility Files/Location_Grid.csv',header = TRUE)
grid_no = seq(1,225,1)
location_grid = cbind(grid_no,location_grid)

location_name = NULL
for (i in 1:225) {
  location_name[i] = paste(location_grid[i,2],location_grid[i,6],location_grid[i,5],sep = ',')
}
colnames(location_grid)[1] = 'grid_name'
colnames(location_grid)[3] = 'country_code'
clean_loc = cbind(select(location_grid,grid_name,country_code),location_name)
write.csv(clean_loc,file="Cleaned Location.csv")

#Performing inner join on the total data with the cleaned location data
total_data = read.csv("Total_Twitter_plus_Calendar.csv",header = TRUE)
data_plus_place = merge(total_data,clean_loc,by=grid_name,all.x = TRUE)

write.csv(data_plus_place,file = 'Total_Twitter_Place_plus_Calendar.csv')

#For the corresponding anomalies, determine the absolute value of the climatological variables
#Reading values from dataset values
air_data_2d = read.csv("Air_Data_2D.csv",header=TRUE,row.names=1)
uwind_data_2d = read.csv("Uwind_Data_2D.csv",header=TRUE,row.names=1)
rel_humid_data_2d = read.csv("Rel_Humid_Data_2D.csv",header=TRUE,row.names=1)
vwind_data_2d = read.csv("Vwind_Data_2D.csv",header=TRUE,row.names=1)
sp_humid_data_2d = read.csv("Sp_Humid_Data_2D.csv",header = TRUE,row.names=1)

air_temp_value =NULL
uwind_temp_value = NULL
vwind_temp_value = NULL
rel_humid_value = NULL
sp_humid_value = NULL

for (i in 1:144278) {
  co_l = data_plus_place[i,1]
  ro_w = data_plus_place[i,2]
  sp_humid_value[i] = sp_humid_data_2d[ro_w,co_l]
}

air_temp_value[i] = air_data_2d[ro_w,co_l]
uwind_temp_value[i] = uwind_data_2d[ro_w,co_l]
vwind_temp_value[i] = vwind_data_2d[ro_w,co_l]
rel_humid_value[i] = rel_humid_data_2d[ro_w,co_l]
complete_data = cbind(data_plus_place,air_temp_value,uwind_temp_value,vwind_temp_value,rel_humid_value,sp_humid_value)

write.csv(complete_data,file = 'Twitter_Anomaly_300hpa_Database.csv')

#Adding row no. and column no. for ease of discovering spatial neighbours
row_no = NULL
col_no = NULL
grid_name = seq(1,851,1)

for (i in 1:23) {
  for (j in 1:37) {
    row_no = c(row_no,i)
    col_no = c(col_no,j)
  }
}
row_col = cbind(grid_name,row_no,col_no)
row_col_fr = as.data.frame(row_col)

complete_data = read.csv('Twitter_Anomaly_300hpa_Database.csv', header = TRUE,row.names = 1)
complete_dat = merge(complete_data,row_col_fr,by = 'grid_name',all.x = TRUE)
write.csv(complete_dat,file = 'Twitter_Anomaly_300hpa_Database.csv')
