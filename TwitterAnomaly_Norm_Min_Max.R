#Generating Multivariate outliers on change data
#Using Modified Gaussian distribution original Model
#P(x) = P(x;u1,e1)*P(x;u2,e2)*P(x;u3,e3)*...*P(x;un,en)
#where P(x)=(x-u)**2/(2*e**2)

#Reading Change Data
score_fr = read.csv("Multivariate_Score.csv",header = TRUE,row.names=1)
air_change = read.csv("Air_Change_Time.csv",header=TRUE,row.names=1)
rel_humid_change = read.csv("Rel_Humid_Change_Time.csv",header=TRUE,row.names=1)
uwind_change = read.csv("Uwind_Change_Time.csv",header=TRUE,row.names=1)
vwind_change = read.csv("Vwind_Change_Time.csv",header=TRUE,row.names=1)

#Removing the first 2 rows reducing to 851 grids from 925 grids
air_change=air_change[,75:925]
rel_humid_change=rel_humid_change[,75:925]
uwind_change=uwind_change[,75:925]
vwind_change=vwind_change[,75:925]

#Normalising the change data using Min-Max Normalisation
#Generating vector for maximum and minimum
grid_max_air = NULL
grid_min_air =NULL
for (i in 1:851) {
  grid_min_air[i]=min(air_change[,i])
  grid_max_air[i]=max(air_change[,i])-grid_min_air[i]
}

grid_max_uwind = NULL
grid_min_uwind =NULL
for (i in 1:851) {
  grid_min_uwind[i]=min(uwind_change[,i])
  grid_max_uwind[i]=max(uwind_change[,i])-grid_min_uwind[i]
}
grid_max_vwind = NULL
grid_min_vwind =NULL
for (i in 1:851) {
  grid_min_vwind[i]=min(vwind_change[,i])
  grid_max_vwind[i]=max(vwind_change[,i])-grid_min_vwind[i]
}
grid_max_rel_humid = NULL
grid_min_rel_humid =NULL
for (i in 1:851) {
  grid_min_rel_humid[i]=min(rel_humid_change[,i])
  grid_max_rel_humid[i]=max(rel_humid_change[,i])-grid_min_rel_humid[i]
}


air_norm=matrix(,nrow = 15339,ncol = 851)
rel_humid_norm=matrix(,nrow = 15339,ncol = 851)
uwind_norm=matrix(,nrow = 15339,ncol = 851)
vwind_norm=matrix(,nrow = 15339,ncol = 851)

#Nornalising the data using Min-Max Normalisation
for (i in 5001:15339) {
  for (j in 1:851) {
    air_norm[i,j]= (air_change[i,j]-grid_min_air[j])/grid_max_air[j]
  }
}

for (i in 1:15339) {
  for (j in 1:851) {
    uwind_norm[i,j]= (uwind_change[i,j]-grid_min_uwind[j])/grid_max_uwind[j]
  }
}

for (i in 1:15339) {
  for (j in 1:851) {
    vwind_norm[i,j]= (vwind_change[i,j]-grid_min_vwind[j])/grid_max_vwind[j]
  }
}

for (i in 1:15339) {
  for (j in 1:851) {
    rel_humid_norm[i,j]= (rel_humid_change[i,j]-grid_min_rel_humid[j])/grid_max_rel_humid[j]
  }
}

#Writing to .csv files
#write.csv(air_norm,file = "Air_Change_MinMax.csv")
write.csv(uwind_norm,file = "UWind_Change_MinMax.csv")
write.csv(vwind_norm,file = "VWind_Change_MinMax.csv")
write.csv(rel_humid_norm,file = "Rel_Humidity_Change_MinMax.csv")

#Generating vector for mean and standard deviation
grid_sd_air = NULL
grid_mean_air =NULL
for (i in 1:851) {
  grid_mean_air[i]=mean(air_norm[,i])
  grid_sd_air[i]=sd(air_norm[,i])*sd(air_norm[,i])*2
}

grid_sd_uwind = NULL
grid_mean_uwind =NULL
for (i in 1:851) {
  grid_mean_uwind[i]=mean(uwind_norm[,i])
  grid_sd_uwind[i]=sd(uwind_norm[,i])*sd(uwind_norm[,i])*2
}
grid_sd_vwind = NULL
grid_mean_vwind =NULL
for (i in 1:851) {
  grid_mean_vwind[i]=mean(vwind_norm[,i])
  grid_sd_vwind[i]=sd(vwind_norm[,i])*sd(vwind_norm[,i])*2
}
grid_sd_rel_humid = NULL
grid_mean_rel_humid =NULL
for (i in 1:851) {
  grid_mean_rel_humid[i]=mean(rel_humid_norm[,i])
  grid_sd_rel_humid[i]=sd(rel_humid_norm[,i])*sd(rel_humid_norm[,i])*2
}

#Generating multivariate score
score = matrix(,nrow = 15339,ncol = 851)
for (i in 5001:15339) {
  for (j in 1:851) {
    air = ((air_norm[i,j] - grid_mean_air[j])**2)/(grid_sd_air[j])
    uwind= ((uwind_norm[i,j] - grid_mean_uwind[j])**2)/(grid_sd_uwind[j])
    vwind= ((vwind_norm[i,j] - grid_mean_vwind[j])**2)/(grid_sd_vwind[j])
    rel_humid = ((rel_humid_norm[i,j] - grid_mean_rel_humid[j])**2)/(grid_sd_rel_humid[j])
    score[i,j]=air*uwind*vwind*rel_humid
  }
}

#Generating frame
score_fr=as.data.frame(score)

#Storing the score matrix
write.csv(score_fr,file="Multivariate_Score.csv")

#Generating mean and threshold for each region
up3=0
down3 = 0
for (i in 1:851) {
  sto_mean = mean(score_fr[,i])
  sto_sd = sd(score_fr[,i])
  up3[i]=sto_mean+(3*sto_sd)
  down3[i]=sto_mean-(3*sto_sd)
}

up25=0
down25 = 0
for (i in 1:925) {
  sto_mean = mean(score_fr[,i])
  sto_sd = sd(score_fr[,i])
  up25[i]=sto_mean+(2.5*sto_sd)
  down25[i]=sto_mean-(2.5*sto_sd)
}

all_work = rbind(score_fr,up3,down3,up25,down25)

all_outliers3_time=NULL
all_outliers3_loc=NULL
all_outliers3_ind=NULL

for (i in 10001:15339) {
  for (j in 1:851) {
    if((score_fr[i,j] > up3[j])==TRUE){
      all_outliers3_time=c(all_outliers3_time,i)
      all_outliers3_loc=c(all_outliers3_loc,j)
      all_outliers3_ind=c(all_outliers3_ind,up3[j])
    }
    if((score_fr[i,j] < down3[j])==TRUE){
      all_outliers3_time=c(all_outliers3_time,i)
      all_outliers3_loc=c(all_outliers3_loc,j)
      all_outliers3_ind=c(all_outliers3_ind,down3[j])
    }
  }
}

all_outliers3=cbind(all_outliers3_time,all_outliers3_loc,all_outliers3_ind)
all_outliers3_fr=as.data.frame(all_outliers3)
write.csv(all_outliers3_fr,file="Multivariate3_Outliers.csv")

#Generating list of Similar outliers with Multivariate Outliers
#Reading the outliers files
air_outliers = read.csv("Air_Outliers.csv",header=TRUE,row.names=1)
rel_humid_outliers = read.csv("Rel_Humidity_Outliers.csv",header=TRUE,row.names=1)
uwind_outliers = read.csv("UWind_Outliers.csv",header=TRUE,row.names=1)
vwind_outliers = read.csv("VWind_Outliers.csv",header=TRUE,row.names=1)
multi_outliers = read.csv("Multivariate3_Outliers.csv",header=TRUE,row.names=1)

names(multi_outliers)=names(air_outliers)

air_multi = merge(air_outliers,multi_outliers,by=c("air_outliers3_time","air_outliers3_loc"))
uwind_multi = merge(uwind_outliers,multi_outliers,by=c("air_outliers3_time","air_outliers3_loc"))
vwind_multi = merge(vwind_outliers,multi_outliers,by=c("air_outliers3_time","air_outliers3_loc"))
rel_humid_multi = merge(rel_humid_outliers,multi_outliers,by=c("air_outliers3_time","air_outliers3_loc"))

#Writing in the .csv file
write.csv(air_multi,file="Air_Multi_Outliers.csv")
write.csv(uwind_multi,file="UWind_Multi_Outliers.csv")
write.csv(vwind_multi,file="VWind_Multi_Outliers.csv")
write.csv(rel_humid_multi,file="Rel_Humidity_Multi_Outliers.csv")

multi_specific = multi_outliers[10000:15340,]

hist(multi_outliers$all_outliers3_time,breaks = 504,xaxt="n",main = "Monthly Multi_Anomaly Dist.")
axis(1, at=seq(0,15340,365),labels=seq(1968,2010,1))

hist(multi_outliers$all_outliers3_loc,breaks = 851,xaxt="n",main = "Grid-wise Anomaly Dist.")
axis(1, at=seq(0,851,37),labels=seq(1,24,1))


res = AnomalyDetectionVec(air_data_2d[,363],direction = 'both',max_anoms = 0.05,period = 7,longterm_period = 365)
res$anoms

#Generating the anomalies using the Anomaly Dtection package open-sourced by Twitter
#Uses the Seasonal-Hybrid ESD-Deviate test
library(AnomalyDetection)
#Starting with the air data
air_data_2d = read.csv("Air_Data_2D.csv",header=TRUE,row.names = 1)
uwind_data_2d = read.csv("Uwind_Data_2D.csv",header=TRUE,row.names = 1)
vwind_data_2d = read.csv("Vwind_Data_2D.csv",header=TRUE,row.names = 1)
rel_humid_data_2d = read.csv("Rel_Humid_Data_2D.csv",header=TRUE,row.names = 1)


res = AnomalyDetectionVec(air_data_2d[,1],direction = 'both',max_anoms = 0.05,plot=TRUE,period = 3,longterm_period = 365)
if(is.null(res)){
  print('Good Morning')
}
ext = rep(1,nrow(res$anoms))
great = cbind(ext,res$anoms)

anomaly_store=NULL
grid_name =NULL
for (i in 1:851) {
  res = AnomalyDetectionVec(air_data_2d[,i],direction = 'both',max_anoms = 0.05,period = 7,longterm_period = 365)
  cnt = nrow(res$anoms)
  if(cnt != 0){
    grid_name = c(grid_name,rep(i,cnt))
    anomaly_store = rbind(anomaly_store,res$anoms)
  }
}

season_anomaly = cbind(grid_name,anomaly_store)
season_anomaly_fr = as.data.frame(season_anomaly)
write.csv(season_anomaly,file="Air_TwitterAnomalyDetection.csv")

#Uwind
anomaly_store=NULL
grid_name =NULL
for (i in 1:851) {
  res = AnomalyDetectionVec(uwind_data_2d[,i],direction = 'both',max_anoms = 0.05,period = 7,longterm_period = 365)
  cnt = nrow(res$anoms)
  if(cnt != 0){
    grid_name = c(grid_name,rep(i,cnt))
    anomaly_store = rbind(anomaly_store,res$anoms)
  }
}

uwind_anomaly = cbind(grid_name,anomaly_store)
write.csv(uwind_anomaly,file="UWind_TwitterAnomalyDetection.csv")

#VWind
anomaly_store=NULL
grid_name =NULL
for (i in 1:851) {
  res = AnomalyDetectionVec(vwind_data_2d[,i],direction = 'both',max_anoms = 0.05,period = 7,longterm_period = 365)
  cnt = nrow(res$anoms)
  if(cnt != 0){
    grid_name = c(grid_name,rep(i,cnt))
    anomaly_store = rbind(anomaly_store,res$anoms)
  }
}

vwind_anomaly = cbind(grid_name,anomaly_store)
write.csv(vwind_anomaly,file="VWind_TwitterAnomalyDetection.csv")

#Relative Humidity
anomaly_store=NULL
grid_name =NULL
for (i in 1:851) {
  res = AnomalyDetectionVec(rel_humid_data_2d[,i],direction = 'both',max_anoms = 0.05,period = 7,longterm_period = 365)
  cnt = nrow(res$anoms)
  if(cnt != 0){
    grid_name = c(grid_name,rep(i,cnt))
    anomaly_store = rbind(anomaly_store,res$anoms)
  }
}

rel_humid_anomaly = cbind(grid_name,anomaly_store)
write.csv(rel_humid_anomaly,file="Rel_Humid_TwitterAnomalyDetection.csv")

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

