#Loading the necessary libraries
library(RNetCDF)

#Clearing the workspace
rm(list=ls())

new_names=NULL
for (i in 0:224) {
  row_no=floor(i/15)+1
  col_no=floor(i%%15)+1
  new_names[i+1]=paste('Row',row_no,'Col',col_no,sep = "")
}



#Opening the .nc file
air_data_open = open.nc("vwind_1951_1990.nc")
uwind_data_open = open.nc("vwind_1991_2014.nc")

#Printing the summary of it
print.nc(air_data_open)
print.nc(uwind_data_open)

#Reading the summary
air_data_read = read.nc(air_data_open)
uwind_data_read = read.nc(uwind_data_open)

#Converting to 2-d
air_data_2d=matrix(air_data_read$vwnd,nrow = 225,ncol=58440)
x=t(air_data_2d)

air_data_2d=as.data.frame(t(air_data_2d))

uwind_data_2d=matrix(uwind_data_read$vwnd,nrow = 225,ncol=35064)
y=t(uwind_data_2d)



uwind_data_2d=as.data.frame(t(uwind_data_2d))

#Printing the structure of air_data_2d
str(air_data_2d)

names(air_data_2d)=new_names
names(uwind_data_2d)=new_names

air_data_2d = rbind.data.frame(air_data_2d,uwind_data_2d)




uwind_data_open = open.nc("surface_prec_wtr_1969.nc")

#Printing the summary of it
#print.nc(uwind_data_open)

#Reading the summary
uwind_data_read = read.nc(uwind_data_open)

#Converting to 2-d

uwind_data_2d=matrix(uwind_data_read$pr_wtr,nrow = 225,ncol=488)
uwind_data_2d=as.data.frame(t(uwind_data_2d))

names(uwind_data_2d)=new_names
air_data_2d = rbind.data.frame(air_data_2d,uwind_data_2d)












write.csv(air_data_2d,file = "Vwind_925_1_Data_2D.csv")
write.csv(uwind_data_2d,file = "Vwind_925_2_Data_2D.csv")

write.csv(new_x,file = "Temp_925_1981_2014_Data_2D.csv")


air_data_open = open.nc("vwind_1991_2014.nc")

#Printing the summary of it
print.nc(air_data_open)

#Reading the summary
air_data_read = read.nc(air_data_open)

#Converting to 2-d
air_data_2d=matrix(air_data_read$uwnd,nrow = 225,ncol=35064)
air_data_2d=as.data.frame(t(air_data_2d))

#Printing the structure of air_data_2d
str(air_data_2d)

names(air_data_2d)=new_names

write.csv(air_data_2d,file = "Vwind_925_2_Data_2D.csv")

air_data_open = open.nc("temp_1951_1980.nc")











#Removing the top 2 lattitudes to get 15340 * 851
#air_data_2d = air_data_2d[,75:925]

#Writing the matrix
#write.table(mat,file="test.txt") # keeps the rownames


#Reading from the table
#air_data_2d = read.table("Air_Data_2D.txt",header=TRUE,row.names=1)


#Starting with U-Wind data

#Clearing the workspace
#rm(list=ls())

#Opening the .nc file
uwind_data_open = open.nc("vwind_1981_2000.nc")

#Printing the summary of it
print.nc(uwind_data_open)

#Reading the summary
uwind_data_read = read.nc(uwind_data_open)

#Converting to 2-d
uwind_data_2d=matrix(uwind_data_read$vwnd,nrow = 225,ncol=29220)
uwind_data_2d=as.data.frame(t(uwind_data_2d))

#Printing the structure of uwind_data_2d
str(uwind_data_2d)
names(uwind_data_2d)=new_names

com_data = rbind.data.frame(air_data_2d,uwind_data_2d)



#Removing the top 2 lattitudes to get 15340 * 851
#uwind_data_2d = uwind_data_2d[,75:925]

#Writing the matrix
write.csv(com_data,file = "VWind_850_1_Data_2D.csv")

#Reading from the table
#uwind_data_2d = read.table("Uwind_Data_2D.txt",header=TRUE,row.names=1)

#Starting with the V-Wind Data
#Clearing the workspace
#rm(list=ls())

#Opening the .nc file
vwind_data_open = open.nc("data1969to2010vwind.nc")

#Printing the summary of it
print.nc(vwind_data_open)

#Reading the summary
vwind_data_read = read.nc(vwind_data_open)

#Converting to 2-d
vwind_data_2d=matrix(vwind_data_read$vwnd,nrow = 925,ncol=15340)
vwind_data_2d=as.data.frame(t(vwind_data_2d))

#Printing the structure of air_data_2d
str(vwind_data_2d)

names(vwind_data_2d)=new_names

#Removing the top 2 lattitudes to get 15340 * 851
vwind_data_2d = vwind_data_2d[,75:925]

#Writing the matrix
write.csv(vwind_data_2d,file = "Vwind_Data_2D.csv")

#Starting with the Specific Humidity data

#Clearing the workspace
#rm(list=ls())

#Opening the .nc file
Sp_Humid_data_open = open.nc("data1969to2010spechum.nc")

#Printing the summary of it
print.nc(Sp_Humid_data_open)

#Reading the summary
Sp_Humid_data_read = read.nc(Sp_Humid_data_open)

#Converting to 2-d
Sp_Humid_data_2d=matrix(Sp_Humid_data_read$shum,nrow = 925,ncol=15340)
Sp_Humid_data_2d=as.data.frame(t(Sp_Humid_data_2d))

#Printing the structure of uwind_data_2d
str(Sp_Humid_data_2d)

names(Sp_Humid_data_2d)=new_names

#Removing the top 2 lattitudes to get 15340 * 851
Sp_Humid_data_2d = Sp_Humid_data_2d[,75:925]

#Writing the matrix
write.csv(Sp_Humid_data_2d,file = "Sp_Humid_Data_2D.csv")

#Starting with the Relative Humidity data

#Clearing the workspace
#rm(list=ls())

#Opening the .nc file
Rel_Humid_data_open = open.nc("data1969to2010relhum.nc")

#Printing the summary of it
print.nc(Rel_Humid_data_open)

#Reading the summary
Rel_Humid_data_read = read.nc(Rel_Humid_data_open)

#Converting to 2-d
Rel_Humid_data_2d=matrix(Rel_Humid_data_read$rhum,nrow = 925,ncol=15340)
Rel_Humid_data_2d=as.data.frame(t(Rel_Humid_data_2d))

#Printing the structure of uwind_data_2d
str(Rel_Humid_data_2d)

names(Rel_Humid_data_2d)=new_names

#Removing the top 2 lattitudes to get 15340 * 851
Rel_Humid_data_2d = Rel_Humid_data_2d[,75:925]

#Writing the matrix
write.csv(Rel_Humid_data_2d,file = "Rel_Humid_Data_2D.csv")

#Closing the open .nc files
close.nc(air_data_open)
close.nc(Rel_Humid_data_open)
close.nc(Sp_Humid_data_open)
close.nc(uwind_data_open)
close.nc(vwind_data_open)

#Clearing the workspace
#rm(list=ls())

#Converting to 2-d
#air_data_2d_coordinates=matrix(l2_norm_air_spatial,nrow =25 ,ncol=37)
#air_data_2d_coordinates_frame = as.data.frame(air_data_2d_coordinates)
