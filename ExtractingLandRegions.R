#Approach: Dealing eith one dataset and making necessary changes

air_land = read.csv("Air_Land.csv",header=TRUE,row.names=1)
land_grids=names(air_land)
#Reading all datasets and extracting the Land grids data only

uwind_data_2d = read.csv("Uwind_Data_2D.csv",header=TRUE,row.names=1)
rel_humid_data_2d = read.csv("Rel_Humid_Data_2D.csv",header=TRUE,row.names=1)
sp_humid_data_2d = read.csv("Sp_Humid_Data_2D.csv",header=TRUE,row.names=1)
vwind_data_2d = read.csv("Vwind_Data_2D.csv",header=TRUE,row.names=1)

vwind_data_land = vwind_data_2d[land_grids]
vwind_data_land_frame = as.data.frame(vwind_data_land)

uwind_data_land = uwind_data_2d[land_grids]
uwind_data_land_frame = as.data.frame(uwind_data_land)

rel_humid_data_land = rel_humid_data_2d[land_grids]
rel_humid_data_land_frame = as.data.frame(rel_humid_data_land)

sp_humid_data_land = sp_humid_data_2d[land_grids]
sp_humid_data_land_frame = as.data.frame(sp_humid_data_land)

#Creating .csv files for each
write.csv(vwind_data_land,file = "Vwind_Land.csv")
write.csv(uwind_data_land,file = "Uwind_Land.csv")
write.csv(rel_humid_data_land,file = "Rel_Humid_Land.csv")
write.csv(sp_humid_data_land,file = "Sp_Humid_Land.csv")

#Clearing the workspace
rm(list=ls())
