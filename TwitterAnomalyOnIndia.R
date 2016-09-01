
rm(list=ls())
#Generating the anomalies using the Anomaly Dtection package open-sourced by Twitter
#Uses the Seasonal-Hybrid ESD-Deviate test
library(AnomalyDetection)

#Writing functions to calculate anomalies on each type
func_anom = function(x,st=NULL){
  anomaly_store=NULL
  grid_name =NULL
  for (i in 1:225) {
    res = AnomalyDetectionVec(x[,i],direction = 'both',max_anoms = 0.05,period = 28,longterm_period = 488)
    cnt = nrow(res$anoms)
    if(cnt != 0){
      grid_name = c(grid_name,rep(i,cnt))
      anomaly_store = rbind(anomaly_store,res$anoms)
    }
  }
  season_anomaly = cbind(grid_name,anomaly_store)
  season_anomaly_fr = as.data.frame(season_anomaly)
  write.csv(season_anomaly,file= st)
}


#Starting with the omega data
air_data_2d = read.csv("Omega_500_1_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Omega_500_1951_1990_Twitter_Anomaly.csv")

air_data_2d = read.csv("Omega_850_1_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Omega_850_1951_1990_Twitter_Anomaly.csv")

air_data_2d = read.csv("Omega_925_1_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Omega_925_1951_1990_Twitter_Anomaly.csv")

air_data_2d = read.csv("Omega_500_2_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Omega_500_1991_2014_Twitter_Anomaly.csv")

air_data_2d = read.csv("Omega_850_2_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Omega_850_1991_2014_Twitter_Anomaly.csv")

air_data_2d = read.csv("Omega_925_2_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Omega_925_1991_2014_Twitter_Anomaly.csv")

#Starting with Relative Humidity
air_data_2d = read.csv("Relative_Hum_500_1_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Relative_Hum_500_1951_1990_Twitter_Anomaly.csv")

air_data_2d = read.csv("Relative_Hum_850_1_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Relative_Hum_850_1951_1990_Twitter_Anomaly.csv")

air_data_2d = read.csv("Relative_Hum_925_1_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Relative_Hum_925_1951_1990_Twitter_Anomaly.csv")

air_data_2d = read.csv("Relative_Hum_850_2_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Relative_Hum_850_1991_2014_Twitter_Anomaly.csv")

air_data_2d = read.csv("Relative_Hum_925_2_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Relative_Hum_925_1991_2014_Twitter_Anomaly.csv")

air_data_2d = read.csv("Surface_Relative_Hum_All_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Surface_Relative_Hum_1951_2014_Twitter_Anomaly.csv")

#Starting with Temperature values

air_data_2d = read.csv("Temp_500_1_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Temp_500_1951_1990_Twitter_Anomaly.csv")

air_data_2d = read.csv("Temp_850_1_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Temp_850_1951_1990_Twitter_Anomaly.csv")

air_data_2d = read.csv("Temp_700_1_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Temp_700_1951_1990_Twitter_Anomaly.csv")

air_data_2d = read.csv("Temp_500_2_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Temp_500_1991_2014_Twitter_Anomaly.csv")

air_data_2d = read.csv("Temp_850_2_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Temp_850_1991_2014_Twitter_Anomaly.csv")

#Starting with Uwind
air_data_2d = read.csv("Uwind_500_1_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Uwind_500_1951_1990_Twitter_Anomaly.csv")

air_data_2d = read.csv("Uwind_850_1_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Uwind_850_1951_1990_Twitter_Anomaly.csv")

air_data_2d = read.csv("Uwind_925_1_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Uwind_925_1951_1990_Twitter_Anomaly.csv")

air_data_2d = read.csv("Uwind_500_2_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Uwind_500_1991_2014_Twitter_Anomaly.csv")

air_data_2d = read.csv("Uwind_925_2_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Uwind_925_1991_2014_Twitter_Anomaly.csv")

air_data_2d = read.csv("Surface_Uwind_1_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Surface_Uwind_1951_1990_Twitter_Anomaly.csv")

air_data_2d = read.csv("Surface_Uwind_2_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Surface_Uwind_1991_2014_Twitter_Anomaly.csv")

#Starting with Vwind

air_data_2d = read.csv("Vwind_925_1_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Vwind_925_1951_1990_Twitter_Anomaly.csv")

air_data_2d = read.csv("Vwind_925_2_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Vwind_925_1991_2014_Twitter_Anomaly.csv")

air_data_2d = read.csv("Surface_Vwind_1_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Surface_Vwind_1951_1990_Twitter_Anomaly.csv")

air_data_2d = read.csv("Surface_Vwind_2_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Surface_Vwind_1991_2014_Twitter_Anomaly.csv")


air_data_2d = read.csv("Temp_925_1951_1980_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Temp_925_1951_1980_Twitter_Anomaly.csv")

air_data_2d = read.csv("Temp_925_1981_2014_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Temp_925_1981_2014_Twitter_Anomaly.csv")

air_data_2d = read.csv("Vwind_850_1951_1980_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Vwind_850_1951_1980_Twitter_Anomaly.csv")

air_data_2d = read.csv("Vwind_850_1981_2014_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Vwind_850_1981_2014_Twitter_Anomaly.csv")

air_data_2d = read.csv("Surface_Prec_Wtr_1951_1969_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Surface_Prec_Wtr_1951_1969_Twitter_Anomaly.csv")

air_data_2d = read.csv("Surface_Prec_Wtr_1970_2014_Data_2D.csv",header=TRUE,row.names = 1)
func_anom(air_data_2d,"Surface_Prec_Wtr_1970_2014_Twitter_Anomaly.csv")





vwind_data_2d = read.csv("Vwind_Data_2D.csv",header=TRUE,row.names = 1)
rel_humid_data_2d = read.csv("Rel_Humid_Data_2D.csv",header=TRUE,row.names = 1)



res = AnomalyDetectionVec(air_data_2d[,1],direction = 'both',max_anoms = 0.05,plot=TRUE,period = 3,longterm_period = 365)
if(is.null(res)){
  print('Good Morning')
}
ext = rep(1,nrow(res$anoms))
great = cbind(ext,res$anoms)


func_anom(vwind_data_2d,"VWind_TwitterAnomalyDetection.csv")
func_anom(rel_humid_data_2d,"Rel_Humid_TwitterAnomalyDetection.csv")


#Finding similar outliers with Change Time Series method
names(season_anomaly)=c("air_outliers3_loc","air_outliers3_time","air_outliers_ind")
names(uwind_anomaly)=c("air_outliers3_loc","air_outliers3_time","air_outliers_ind")
names(vwind_anomaly)=c("air_outliers3_loc","air_outliers3_time","air_outliers_ind")
names(rel_humid_anomaly)=c("air_outliers3_loc","air_outliers3_time","air_outliers_ind")

air_multi = merge(air_outliers,season_anomaly,by=c("air_outliers3_time","air_outliers3_loc"))
uwind_multi = merge(uwind_outliers,uwind_anomaly,by=c("air_outliers3_time","air_outliers3_loc"))
vwind_multi = merge(vwind_outliers,vwind_anomaly,by=c("air_outliers3_time","air_outliers3_loc"))
rel_humid_multi = merge(rel_humid_outliers,rel_humid_anomaly,by=c("air_outliers3_time","air_outliers3_loc"))


write.csv(air_multi,file="Air_Twitter_Change_Outliers.csv")
write.csv(uwind_multi,file="UWind_Twitter_Change_Outliers.csv")
write.csv(vwind_multi,file="VWind_Twitter_Change_Outliers.csv")
write.csv(rel_humid_multi,file="Rel_Humid_Twitter_Change_Outliers.csv")

