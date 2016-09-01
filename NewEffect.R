#Reading from the data csv files
air_data_2d = read.csv("Air_Data_2D.csv",header=TRUE,row.names=1)
uwind_data_2d = read.csv("Uwind_Data_2D.csv",header=TRUE,row.names=1)
vwind_data_2d = read.csv("Vwind_Data_2D.csv",header=TRUE,row.names=1)
sp_humid_data_2d = read.csv("Sp_Humid_Data_2D.csv",header=TRUE,row.names=1)
rel_humid_data_2d = read.csv("Rel_Humid_Data_2D.csv",header=TRUE,row.names=1)

air_data_2d_new = as.matrix(air_data_2d)
uwind_data_2d_new = as.matrix(uwind_data_2d)
vwind_data_2d_new = as.matrix(vwind_data_2d)
sp_humid_data_2d_new = as.matrix(sp_humid_data_2d)
rel_humid_data_2d_new = as.matrix(rel_humid_data_2d)

grid_sd_air = NULL
for (i in 1:851) {
  grid_sd_air=c(grid_sd_air,sd(air_data_2d_new[,i]))
}

grid_sd_uwind = NULL
for (i in 1:851) {
  grid_sd_uwind=c(grid_sd_uwind,sd(uwind_data_2d_new[,i]))
}
grid_sd_vwind = NULL
for (i in 1:851) {
  grid_sd_vwind=c(grid_sd_vwind,sd(vwind_data_2d_new[,i]))
}
grid_sd_rel_humid = NULL
for (i in 1:851) {
  grid_sd_rel_humid=c(grid_sd_rel_humid,sd(rel_humid_data_2d_new[,i]))
}
grid_sd_sp_humid = NULL
for (i in 1:851) {
  grid_sd_sp_humid=c(grid_sd_sp_humid,sd(sp_humid_data_2d_new[,i]))
}
summary(grid_sd_air)
col_gen=NULL
for (i in 1:23) {
  col_gen = c(col_gen,seq(1,37,1))
}

row_gen=NULL
for (i in 1:23) {
  for (j in 1:37) {
    row_gen = c(row_gen,i)
  }
}
row_gen_fr = as.data.frame(row_gen)
col_gen_fr =as.data.frame(col_gen)
rm(plot4d)

plot4d = data.frame(row_gen_fr,col_gen_fr,as.data.frame(grid_sd_sp_humid))
scatter2D(plot4d$col_gen,plot4d$row_gen,colvar = plot4d$grid_sd_sp_humid,pch=15,main="Std. Dev Sp Humid")


#Previously done
air_data_2d_new= air_data_2d[,75:925]
uwind_data_2d_new= uwind_data_2d[,75:925]
vwind_data_2d_new= vwind_data_2d[,75:925]
rel_humid_data_2d_new= rel_humid_data_2d[,75:925]
sp_humid_data_2d_new= sp_humid_data_2d[,75:925]

write.csv(rel_humid_data_2d_new,file = "Rel_Humid_Data_2D.csv")
write.csv(sp_humid_data_2d_new,file = "Sp_Humid_Data_2D.csv")
write.csv(air_data_2d_new,file = "Air_Data_2D.csv")
write.csv(uwind_data_2d_new,file = "Uwind_Data_2D.csv")
write.csv(vwind_data_2d_new,file = "Vwind_Data_2D.csv")

plot(density(air_data_2d_new[,149:703]))

