#Create Change file
#Reading values from air_data_2d
air_data_2d = read.csv("Air_Data_2D.csv",header=TRUE,row.names=1)
air_tr=t(air_data_2d)
air_tr_f=as.data.frame(air_tr)

air_diff=air_tr_f[2] - air_tr_f[1]

for (i in 2:15339) {
  store = air_tr_f[i+1] - air_tr_f[i]
  air_diff=data.frame(air_diff,store)
}

air_work = t(air_diff)
air_work_fr = as.data.frame(air_work)

#Writing to csv files
write.csv(air_work_fr,file = "Air_Change_Time.csv")

#Reading values from uwind_data_2d
air_data_2d = read.csv("Uwind_Data_2D.csv",header=TRUE,row.names=1)
air_tr=t(air_data_2d)
air_tr_f=as.data.frame(air_tr)

air_diff=air_tr_f[2] - air_tr_f[1]

for (i in 2:15339) {
  store = air_tr_f[i+1] - air_tr_f[i]
  air_diff=data.frame(air_diff,store)
}

air_work = t(air_diff)
air_work_fr = as.data.frame(air_work)

#Writing to csv files
write.csv(air_work_fr,file = "Uwind_Change_Time.csv")

#Reading values from vwind_data_2d
air_data_2d = read.csv("Vwind_Data_2D.csv",header=TRUE,row.names=1)
air_tr=t(air_data_2d)
air_tr_f=as.data.frame(air_tr)

air_diff=air_tr_f[2] - air_tr_f[1]

for (i in 2:15339) {
  store = air_tr_f[i+1] - air_tr_f[i]
  air_diff=data.frame(air_diff,store)
}

air_work = t(air_diff)
air_work_fr = as.data.frame(air_work)

#Writing to csv files
write.csv(air_work_fr,file = "Vwind_Change_Time.csv")

#Reading values from rel_humid_data_2d
air_data_2d = read.csv("Rel_Humid_Data_2D.csv",header=TRUE,row.names=1)
air_tr=t(air_data_2d)
air_tr_f=as.data.frame(air_tr)

air_diff=air_tr_f[2] - air_tr_f[1]

for (i in 2:15339) {
  store = air_tr_f[i+1] - air_tr_f[i]
  air_diff=data.frame(air_diff,store)
}

air_work = t(air_diff)
air_work_fr = as.data.frame(air_work)

#Writing to csv files
write.csv(air_work_fr,file = "Rel_Humid_Change_Time.csv")

#Reading the Time Change files
air_change = read.csv("Air_Change_Time.csv",header=TRUE,row.names=1)
uwind_change = read.csv("Uwind_Change_Time.csv",header=TRUE,row.names=1)
vwind_change = read.csv("Vwind_Change_Time.csv",header=TRUE,row.names=1)
rel_humid_change = read.csv("Rel_Humid_Change_Time.csv",header=TRUE,row.names=1)

#Generating the correlation Matrix
#Set 1:Air
#Generating all the correlation values of each pair of air_temp and U-Wind
corr_air_uwnd=0
for(i in 1:925)
{
  corr_air_uwnd[i] = cor(air_change[i],uwind_change[i]) 
}

#Generating all the correlation values of each pair of air_temp and V-Wind
corr_air_vwnd=0
for(i in 1:925)
{
  corr_air_vwnd[i] = cor(air_change[i],vwind_change[i]) 
}

#Generating all the correlation values of each pair of air_temp and Relative Humidity
corr_air_rel_humid=0
for(i in 1:925)
{
  corr_air_rel_humid[i] = cor(air_change[i],rel_humid_change[i]) 
}

#Generating all the correlation values of each pair of air_temp and spific Humidity
corr_air_sp_humid=0
for(i in 1:925)
{
  corr_air_sp_humid[i] = cor(air_change[i],sp_humid_change[i]) 
}

#Set 2: Relative Humidity
#Generating all the correlation values of each pair of air_temp and U-Wind
corr_rel_humid_uwnd=0
for(i in 1:925)
{
  corr_rel_humid_uwnd[i] = cor(rel_humid_change[i],uwind_change[i]) 
}

#Generating all the correlation values of each pair of air_temp and V-Wind
corr_rel_humid_vwnd=0
for(i in 1:925)
{
  corr_rel_humid_vwnd[i] = cor(rel_humid_change[i],vwind_change[i]) 
}

#Generating all the correlation values of each pair of air_temp and spific Humidity
corr_rel_humid_sp_humid=0
for(i in 1:925)
{
  corr_rel_humid_sp_humid[i] = cor(rel_humid_change[i],sp_humid_change[i]) 
}

#Set 3: spific Humidity
#Generating all the correlation values of each pair of air_temp and U-Wind
corr_sp_humid_uwnd=0
for(i in 1:925)
{
  corr_sp_humid_uwnd[i] = cor(sp_humid_change[i],uwind_change[i]) 
}

#Generating all the correlation values of each pair of air_temp and V-Wind
corr_sp_humid_vwnd=0
for(i in 1:925)
{
  corr_sp_humid_vwnd[i] = cor(sp_humid_change[i],vwind_change[i]) 
}

#Set 4: U-Wind
corr_uwnd_vwnd=0
for(i in 1:925)
{
  corr_uwnd_vwnd[i] = cor(uwind_change[i],vwind_change[i]) 
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
write.csv(corr_air_sp_humid_frame2,"Change_Corr_Combined.csv")

#Generating colored 2-d plots for correlation between Air Temp and UWind
col_gen=NULL
for (i in 1:25) {
  col_gen = c(col_gen,seq(1,37,1))
}

row_gen=NULL
for (i in 1:25) {
  for (j in 1:37) {
    row_gen = c(row_gen,i)
  }
}
row_gen_fr = as.data.frame(row_gen)
col_gen_fr =as.data.frame(col_gen)

#cor_tot_first = read.csv("Corr_Last10_Combined.csv",header=TRUE,row.names=1)
#cor_tot_first=data.frame(row_gen_fr,col_gen_fr,cor_tot_first)
#Using scatter2d to generate 3d plots where x=latitude, y=longitude,color= correlation value between air temp and uwind
plot_frame=data.frame(corr_air_uwnd_frame,row_gen_fr,col_gen_fr)
scatter2D(plot_frame$col_gen,plot_frame$row_gen,colvar = plot_frame$corr_air_uwnd,pch=15,main="Air_Uwind_Change_Correlation")
