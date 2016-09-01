#Reading values from air_data_2d and uwind_data_2d
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

up3=0
down3 = 0
for (i in 1:925) {
  sto_mean = mean(air_work[,i])
  sto_sd = sd(air_work[,i])
  up3[i]=sto_mean+(3*sto_sd)
  down3[i]=sto_mean-(3*sto_sd)
}

#up25=0
#down25 = 0
#for (i in 1:925) {
 # sto_mean = mean(air_work[,i])
  #sto_sd = sd(air_work[,i])
  #up25[i]=sto_mean+(2.5*sto_sd)
  #down25[i]=sto_mean-(2.5*sto_sd)
#}

air_work = rbind(air_work,up3,down3,up25,down25)

air_outliers3_time=0
air_outliers3_loc=0
air_outliers_ind=0

for (i in 1:15339) {
  for (j in 1:925) {
    if((air_work[i,j] > up3[j])==TRUE){
      air_outliers3_time=c(air_outliers3_time,i)
      air_outliers3_loc=c(air_outliers3_loc,j)
      air_outliers_ind=c(air_outliers_ind,up3[j])
    }
    if((air_work[i,j] < down3[j])==TRUE){
      air_outliers3_time=c(air_outliers3_time,i)
      air_outliers3_loc=c(air_outliers3_loc,j)
      air_outliers_ind=c(air_outliers_ind,down3[j])
    }
  }
}

air_outliers=cbind(air_outliers3_time,air_outliers3_loc,air_outliers_ind)
air_outliers_fr=as.data.frame(air_outliers)
write.csv(air_outliers_fr,file="Air_Outliers.csv")

nrow(subset(air_outliers,abs(air_outliers_ind)>3 ))
nrow(subset(air_outliers,abs(air_outliers_ind)>3.5 ))/14188575
nrow(subset(air_outliers,abs(air_outliers_ind)>4.0 ))

#End Air_Outlier Detection

#Start UWind_Outlier Detection
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

up3=0
down3 = 0
for (i in 1:925) {
  sto_mean = mean(air_work[,i])
  sto_sd = sd(air_work[,i])
  up3[i]=sto_mean+(3*sto_sd)
  down3[i]=sto_mean-(3*sto_sd)
}

#up25=0
#down25 = 0
#for (i in 1:925) {
 # sto_mean = mean(air_work[,i])
  #sto_sd = sd(air_work[,i])
  #up25[i]=sto_mean+(2.5*sto_sd)
  #down25[i]=sto_mean-(2.5*sto_sd)
#}

#air_work = rbind(air_work,up3,down3,up25,down25)

air_outliers3_time=0
air_outliers3_loc=0
air_outliers_ind=0

for (i in 1:15339) {
  for (j in 1:925) {
    if((air_work[i,j] > up3[j])==TRUE){
      air_outliers3_time=c(air_outliers3_time,i)
      air_outliers3_loc=c(air_outliers3_loc,j)
      air_outliers_ind=c(air_outliers_ind,up3[j])
    }
    if((air_work[i,j] < down3[j])==TRUE){
      air_outliers3_time=c(air_outliers3_time,i)
      air_outliers3_loc=c(air_outliers3_loc,j)
      air_outliers_ind=c(air_outliers_ind,down3[j])
    }
  }
}

air_outliers=cbind(air_outliers3_time,air_outliers3_loc,air_outliers_ind)
air_outliers_fr=as.data.frame(air_outliers)
write.csv(air_outliers_fr,file="UWind_Outliers.csv")

nrow(subset(air_outliers,abs(air_outliers_ind)>3 ))
nrow(subset(air_outliers,abs(air_outliers_ind)>3.5 ))/14188575
nrow(subset(air_outliers,abs(air_outliers_ind)>4.0 ))

#End UWind_Outlier Detection

#Start UWind_Outlier Detection
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

up3=0
down3 = 0
for (i in 1:925) {
  sto_mean = mean(air_work[,i])
  sto_sd = sd(air_work[,i])
  up3[i]=sto_mean+(3*sto_sd)
  down3[i]=sto_mean-(3*sto_sd)
}

#up25=0
#down25 = 0
#for (i in 1:925) {
# sto_mean = mean(air_work[,i])
#sto_sd = sd(air_work[,i])
#up25[i]=sto_mean+(2.5*sto_sd)
#down25[i]=sto_mean-(2.5*sto_sd)
#}

#air_work = rbind(air_work,up3,down3,up25,down25)

air_outliers3_time=0
air_outliers3_loc=0
air_outliers_ind=0

for (i in 1:15339) {
  for (j in 1:925) {
    if((air_work[i,j] > up3[j])==TRUE){
      air_outliers3_time=c(air_outliers3_time,i)
      air_outliers3_loc=c(air_outliers3_loc,j)
      air_outliers_ind=c(air_outliers_ind,up3[j])
    }
    if((air_work[i,j] < down3[j])==TRUE){
      air_outliers3_time=c(air_outliers3_time,i)
      air_outliers3_loc=c(air_outliers3_loc,j)
      air_outliers_ind=c(air_outliers_ind,down3[j])
    }
  }
}

air_outliers=cbind(air_outliers3_time,air_outliers3_loc,air_outliers_ind)
air_outliers_fr=as.data.frame(air_outliers)
write.csv(air_outliers_fr,file="VWind_Outliers.csv")

nrow(subset(air_outliers,abs(air_outliers_ind)>3 ))
nrow(subset(air_outliers,abs(air_outliers_ind)>3.5 ))/14188575
nrow(subset(air_outliers,abs(air_outliers_ind)>4.0 ))

#End VWind_Outlier Detection

#Removing first 2 rows falling out of Tropical regions
#Reading the outliers dataset
air_data_2d = read.csv("Air_Outliers.csv",header=TRUE,row.names=1)
uwind_data_2d = read.csv("UWind_Outliers.csv",header=TRUE,row.names=1)
vwind_data_2d = read.csv("VWind_Outliers.csv",header=TRUE,row.names=1)
rel_humid_data_2d = read.csv("Rel_Humidity_Outliers.csv",header=TRUE,row.names=1)

hist(air_data_2d$air_outliers3_loc)
hist(uwind_data_2d$air_outliers3_loc)
hist(vwind_data_2d$air_outliers3_loc)
hist(rel_humid_data_2d$air_outliers3_loc)

air_data_2d_after=subset(air_data_2d,air_outliers3_loc>74)
uwind_data_2d_after=subset(uwind_data_2d,air_outliers3_loc>74)
vwind_data_2d_after=subset(vwind_data_2d,air_outliers3_loc>74)
rel_humid_data_2d_after=subset(rel_humid_data_2d,air_outliers3_loc>74)

hist(air_data_2d_after$air_outliers3_loc)
hist(uwind_data_2d_after$air_outliers3_loc)
hist(vwind_data_2d_after$air_outliers3_loc)
hist(rel_humid_data_2d_after$air_outliers3_loc)

nrow(subset(air_data_2d_after,abs(air_outliers_ind)>3 ))
nrow(subset(air_data_2d_after,abs(air_outliers_ind)>3.5 ))/14188575
nrow(subset(air_data_2d_after,abs(air_outliers_ind)>4.0 ))


write.csv(air_outliers_fr,file="VWind_Outliers.csv")