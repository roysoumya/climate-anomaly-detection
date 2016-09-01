#Loading the necessary libraries
library(RNetCDF)

#Clearing the workspace
rm(list=ls())


#Reading values from air_data_2d and uwind_data_2d
air_data_2d = read.csv("Air_Data_2D.csv",header=TRUE,row.names=1)
uwind_data_2d = read.csv("Uwind_Data_2D.csv",header=TRUE,row.names=1)
rel_humid_data_2d = read.csv("Rel_Humid_Data_2D.csv",header=TRUE,row.names=1)
sp_humid_data_2d = read.csv("Sp_Humid_Data_2D.csv",header=TRUE,row.names=1)
vwind_data_2d = read.csv("Vwind_Data_2D.csv",header=TRUE,row.names=1)

#Preprocessing for Spatial characteristics mining
#Air
l2_norm_air_spatial=0
for(i in 1:925)
{
  total_sum=sum(air_data_2d[i]**2)
  l2_norm_air_spatial[i]=sqrt(total_sum)
}

#Relative Humidity
l2_norm_rel_humid_spatial=0
for(i in 1:925)
{
  total_sum=sum(rel_humid_data_2d[i]**2)
  l2_norm_rel_humid_spatial[i]=sqrt(total_sum)
}

#Specific Humidity
l2_norm_sp_humid_spatial=0
for(i in 1:925)
{
  total_sum=sum(sp_humid_data_2d[i]**2)
  l2_norm_sp_humid_spatial[i]=sqrt(total_sum)
}

#U-Wind
l2_norm_uwind_spatial=0
for(i in 1:925)
{
  total_sum=sum(uwind_data_2d[i]**2)
  l2_norm_uwind_spatial[i]=sqrt(total_sum)
}

#V-Wind
l2_norm_vwind_spatial=0
for(i in 1:925)
{
  total_sum=sum(vwind_data_2d[i]**2)
  l2_norm_vwind_spatial[i]=sqrt(total_sum)
}

#Generating the frames
l2_norm_vwind_spatial_frame = as.data.frame(l2_norm_vwind_spatial)
l2_norm_uwind_spatial_frame = as.data.frame(l2_norm_uwind_spatial)
l2_norm_sp_humid_spatial_frame = as.data.frame(l2_norm_sp_humid_spatial)
l2_norm_rel_humid_spatial_frame = as.data.frame(l2_norm_rel_humid_spatial)
l2_norm_air_spatial_frame = as.data.frame(l2_norm_air_spatial)

#Binding all spatial characteristics in one file
SpatialData = data.frame(l2_norm_air_spatial_frame,l2_norm_rel_humid_spatial_frame,l2_norm_sp_humid_spatial_frame,l2_norm_uwind_spatial_frame,l2_norm_vwind_spatial_frame)
write.csv(SpatialData,"Spatial_Data_LNorm2.csv")

#Preprocessing for Temporal characteristics mining

#Creating transpose
air_data_2d_tr = t(air_data_2d)
uwind_data_2d_tr = t(uwind_data_2d)
rel_humid_data_2d_tr = t(rel_humid_data_2d)
sp_humid_data_2d_tr = t(sp_humid_data_2d)
vwind_data_2d_tr = t(vwind_data_2d)

#Air
l2_norm_air_temporal=0
for(i in 1:15340)
{
  total_sum=sum(air_data_2d_tr[i]**2)
  l2_norm_air_temporal[i]=sqrt(total_sum)
}

#Relative Humidity
l2_norm_rel_humid_temporal=0
for(i in 1:15340)
{
  total_sum=sum(rel_humid_data_2d_tr[i]**2)
  l2_norm_rel_humid_temporal[i]=sqrt(total_sum)
}

#Specific Humidity
l2_norm_sp_humid_temporal=0
for(i in 1:15340)
{
  total_sum=sum(sp_humid_data_2d_tr[i]**2)
  l2_norm_sp_humid_temporal[i]=sqrt(total_sum)
}

#U-Wind
l2_norm_uwind_temporal=0
for(i in 1:15340)
{
  total_sum=sum(uwind_data_2d_tr[i]**2)
  l2_norm_uwind_temporal[i]=sqrt(total_sum)
}

#V-Wind
l2_norm_vwind_temporal=0
for(i in 1:15340)
{
  total_sum=sum(vwind_data_2d_tr[i]**2)
  l2_norm_vwind_temporal[i]=sqrt(total_sum)
}

#Generating the frames
l2_norm_vwind_temporal_frame = as.data.frame(l2_norm_vwind_temporal)
l2_norm_uwind_temporal_frame = as.data.frame(l2_norm_uwind_temporal)
l2_norm_sp_humid_temporal_frame = as.data.frame(l2_norm_sp_humid_temporal)
l2_norm_rel_humid_temporal_frame = as.data.frame(l2_norm_rel_humid_temporal)
l2_norm_air_temporal_frame = as.data.frame(l2_norm_air_temporal)

#Binding all spatial characteristics in one file
TemporalData = data.frame(l2_norm_air_temporal_frame,l2_norm_rel_humid_temporal_frame,l2_norm_sp_humid_temporal_frame,l2_norm_uwind_temporal_frame,l2_norm_vwind_temporal_frame)
write.csv(TemporalData,"Temporal_Data_LNorm2_final.csv")

#Creating seperate file storing individual parameteres accordiing to lat-long coordinates
air_lnorm2_geo = as.data.frame(t(matrix(l2_norm_air_spatial,nrow = 37,ncol=25)))
rel_humid_lnorm2_geo = as.data.frame(t(matrix(l2_norm_rel_humid_spatial,nrow = 37,ncol=25)))
sp_humid_lnorm2_geo = as.data.frame(t(matrix(l2_norm_sp_humid_spatial,nrow = 37,ncol=25)))
uwind_lnorm2_geo = as.data.frame(t(matrix(l2_norm_uwind_spatial,nrow = 37,ncol=25)))
vwind_lnorm2_geo = as.data.frame(t(matrix(l2_norm_vwind_spatial,nrow = 37,ncol=25)))

#Creating .csv files for each
write.csv(air_lnorm2_geo,file = "Processed_Air_Data_2D.csv")
write.csv(rel_humid_lnorm2_geo,file = "Processed_Rel_Humid_Data_2D.csv")
write.csv(sp_humid_lnorm2_geo,file = "Processed_Sp_Humid_Data_2D.csv")
write.csv(uwind_lnorm2_geo,file = "Processed_UWind_Data_2D.csv")
write.csv(vwind_lnorm2_geo,file = "Processed_Vwind_Data_2D.csv")

#Clearing the workspace
rm(list=ls())
