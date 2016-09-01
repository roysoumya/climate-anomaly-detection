#Clearing the workspace
rm(list=ls())

#Renaming column headers for VWind_Data_2D
vwind_data_2d = read.csv("Vwind_Data_2D.csv",header=TRUE,row.names=1)

new_names=NULL
for (i in 0:924) {
  row_no=floor(i/37)+1
  col_no=floor(i%%37)+1
  new_names[i+1]=paste('Row',row_no,'Col',col_no,sep = "")
}

new_names_frame=as.data.frame(new_names)

names(vwind_data_2d)=new_names
names(vwind_data_2d)

#Writing the matrix
write.csv(vwind_data_2d,file = "Vwind_Data_2D_final.csv")
rm(vwind_data_2d)
rm(old_names)


#Renaming column headers for UWind_Data_2D
uwind_data_2d = read.csv("Uwind_Data_2D.csv",header=TRUE,row.names=1)

names(uwind_data_2d)=new_names
names(uwind_data_2d)

#Writing the matrix
write.csv(uwind_data_2d,file = "Uwind_Data_2D_final.csv")
rm(uwind_data_2d)

#Renaming column headers for Air_Data_2D
air_data_2d = read.csv("Air_Data_2D.csv",header=TRUE,row.names=1)

names(air_data_2d)=new_names
names(air_data_2d)

#Writing the matrix
write.csv(air_data_2d,file = "Air_Data_2D_final.csv")
rm(air_data_2d)

#Renaming column headers for Rel_Humid_Data_2D
rel_humid_data_2d = read.csv("Rel_Humid_Data_2D.csv",header=TRUE,row.names=1)

names(rel_humid_data_2d)=new_names
names(rel_humid_data_2d)

#Writing the matrix
write.csv(rel_humid_data_2d,file = "Rel_Humid_Data_2D_final.csv")
rm(rel_humid_data_2d)

#Renaming column headers for Sp_Humid_Data_2D
sp_humid_data_2d = read.csv("Sp_Humid_Data_2D.csv",header=TRUE,row.names=1)

names(sp_humid_data_2d)=new_names
names(sp_humid_data_2d)

#Writing the matrix
write.csv(sp_humid_data_2d,file = "Sp_Humid_Data_2D_final.csv")
rm(sp_humid_data_2d)

#Renaming complete for all 5 datasets

#Generating list of Water Grids
land_grids=NULL
for (i in c(75:160,166:182,186:197,203:207,212:219,223:234,240:244,248:256,260:271,277:281,286:293,297:308,314:318,323:330,334:345,351:355,360:378)) {
  land_grids=c(land_grids,new_names[i])
}

for (i in c(398:415,435:452,472:489,509:526,546:563,583:601,630:638,667:675,704:715,721:725,730:748,775:785,812:822,849:859,886:900,906:910,915:925)) {
  land_grids=c(land_grids,new_names[i])
}

land_grids_frame=as.data.frame(land_grids)

#Reading all datasets and extracting the Land grids data only

air_data_2d = read.csv("Air_Data_2D.csv",header=TRUE,row.names=1)
uwind_data_2d = read.csv("Uwind_Data_2D.csv",header=TRUE,row.names=1)
rel_humid_data_2d = read.csv("Rel_Humid_Data_2D.csv",header=TRUE,row.names=1)
sp_humid_data_2d = read.csv("Sp_Humid_Data_2D.csv",header=TRUE,row.names=1)
vwind_data_2d = read.csv("Vwind_Data_2D.csv",header=TRUE,row.names=1)

vwind_data_land = vwind_data_2d[land_grids]
vwind_data_land_frame = as.data.frame(vwind_data_land)

air_data_land = air_data_2d[land_grids]
air_data_land_frame = as.data.frame(air_data_land)

uwind_data_land = uwind_data_2d[land_grids]
uwind_data_land_frame = as.data.frame(uwind_data_land)

rel_humid_data_land = rel_humid_data_2d[land_grids]
rel_humid_data_land_frame = as.data.frame(rel_humid_data_land)

sp_humid_data_land = sp_humid_data_2d[land_grids]
sp_humid_data_land_frame = as.data.frame(sp_humid_data_land)

#Creating .csv files for each
write.csv(vwind_data_land,file = "Vwind_Land.csv")
write.csv(air_data_land,file = "Air_Land.csv")
write.csv(uwind_data_land,file = "Uwind_Land.csv")
write.csv(rel_humid_data_land,file = "Rel_Humid_Land.csv")
write.csv(sp_humid_data_land,file = "Sp_Humid_Land.csv")

#Clearing the workspace
rm(list=ls())


#Approach 2 
