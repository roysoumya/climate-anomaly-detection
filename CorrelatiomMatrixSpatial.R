#Loading required libraries
library(RNetCDF)

#Removing all elements from workspace
rm(list=ls())

#Reading values from air_data_2d and uwind_data_2d
air_data_2d = read.csv("Air_Land.csv",header=TRUE,row.names=1)
uwind_data_2d = read.csv("Uwind_Land.csv",header=TRUE,row.names=1)
rel_humid_data_2d = read.csv("Rel_Humid_Land.csv",header=TRUE,row.names=1)
sp_humid_data_2d = read.csv("Sp_Humid_Land.csv",header=TRUE,row.names=1)
vwind_data_2d = read.csv("Vwind_Land.csv",header=TRUE,row.names=1)

class(air_data_2d)

air_data_2d=as.data.frame(t(air_data_2d[1:369]))
uwind_data_2d=as.data.frame(t(uwind_data_2d[1:369]))
rel_humid_data_2d=as.data.frame(t(rel_humid_data_2d[1:369]))
sp_humid_data_2d=as.data.frame(t(sp_humid_data_2d[1:369]))
vwind_data_2d=as.data.frame(t(vwind_data_2d[1:369]))

nrow(air_data_2d)
#Set 1:Air
#Generating all the correlation values of each pair of air_temp and U-Wind
corr_air_uwnd=0
for(i in 1:15340)
{
  corr_air_uwnd[i] = cor(air_data_2d[i],uwind_data_2d[i]) 
}

#Generating all the correlation values of each pair of air_temp and V-Wind
corr_air_vwnd=0
for(i in 1:15340)
{
  corr_air_vwnd[i] = cor(air_data_2d[i],vwind_data_2d[i]) 
}

#Generating all the correlation values of each pair of air_temp and Relative Humidity
corr_air_rel_humid=0
for(i in 1:15340)
{
  corr_air_rel_humid[i] = cor(air_data_2d[i],rel_humid_data_2d[i]) 
}

#Generating all the correlation values of each pair of air_temp and spific Humidity
corr_air_sp_humid=0
for(i in 1:15340)
{
  corr_air_sp_humid[i] = cor(air_data_2d[i],sp_humid_data_2d[i]) 
}

#Set 2: Relative Humidity
#Generating all the correlation values of each pair of air_temp and U-Wind
corr_rel_humid_uwnd=0
for(i in 1:15340)
{
  corr_rel_humid_uwnd[i] = cor(rel_humid_data_2d[i],uwind_data_2d[i]) 
}

#Generating all the correlation values of each pair of air_temp and V-Wind
corr_rel_humid_vwnd=0
for(i in 1:15340)
{
  corr_rel_humid_vwnd[i] = cor(rel_humid_data_2d[i],vwind_data_2d[i]) 
}

#Generating all the correlation values of each pair of air_temp and spific Humidity
corr_rel_humid_sp_humid=0
for(i in 1:15340)
{
  corr_rel_humid_sp_humid[i] = cor(rel_humid_data_2d[i],sp_humid_data_2d[i]) 
}

#Set 3: spific Humidity
#Generating all the correlation values of each pair of air_temp and U-Wind
corr_sp_humid_uwnd=0
for(i in 1:15340)
{
  corr_sp_humid_uwnd[i] = cor(sp_humid_data_2d[i],uwind_data_2d[i]) 
}

#Generating all the correlation values of each pair of air_temp and V-Wind
corr_sp_humid_vwnd=0
for(i in 1:15340)
{
  corr_sp_humid_vwnd[i] = cor(sp_humid_data_2d[i],vwind_data_2d[i]) 
}

#Set 4: U-Wind
corr_uwnd_vwnd=0
for(i in 1:15340)
{
  corr_uwnd_vwnd[i] = cor(uwind_data_2d[i],vwind_data_2d[i]) 
}

#Generating the frames
#Set 1: Air 
corr_air_sp_humid_frame = as.data.frame(corr_air_sp_humid)
corr_air_rel_humid_frame = as.data.frame(corr_air_rel_humid)
corr_air_vwnd_frame = as.data.frame(corr_air_vwnd)
corr_air_uwnd_frame = as.data.frame(corr_air_uwnd)

#Set 2: Relative Humidity
corr_rel_humid_sp_humid_frame = as.data.frame(corr_rel_humid_sp_humid)
corr_rel_humid_vwnd_frame = as.data.frame(corr_rel_humid_vwnd)
corr_rel_humid_uwnd_frame = as.data.frame(corr_rel_humid_uwnd)

#Set 3: Specific Humidity
corr_sp_humid_vwnd_frame = as.data.frame(corr_sp_humid_vwnd)
corr_sp_humid_uwnd_frame = as.data.frame(corr_sp_humid_uwnd)

#Set 4: U-Wind
corr_uwnd_vwnd_frame = as.data.frame(corr_uwnd_vwnd)



#Combining the frames into one table
corr_air_sp_humid_frame2=data.frame(corr_air_sp_humid_frame,corr_air_rel_humid_frame,corr_air_vwnd_frame,corr_air_uwnd_frame,corr_rel_humid_sp_humid_frame,corr_rel_humid_vwnd_frame,corr_rel_humid_uwnd_frame,corr_sp_humid_vwnd_frame,corr_sp_humid_uwnd_frame,corr_uwnd_vwnd_frame)

#Write all the results in a single .csv file
write.csv(corr_air_sp_humid_frame2,"Corr_Land_Combined_Spatial.csv")

#Removing all the elements from the workspace
rm(list = ls())


