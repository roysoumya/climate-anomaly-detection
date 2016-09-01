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


#Generating vector for mean and standard deviation
grid_sd_air = NULL
grid_mean_air =NULL
for (i in 1:925) {
  grid_mean_air[i]=mean(air_change[,i])
  grid_sd_air[i]=sd(air_change[,i])
}

grid_sd_uwind = NULL
grid_mean_uwind =NULL
for (i in 1:925) {
  grid_mean_uwind[i]=mean(uwind_change[,i])
  grid_sd_uwind[i]=sd(uwind_change[,i])
}
grid_sd_vwind = NULL
grid_mean_vwind =NULL
for (i in 1:925) {
  grid_mean_vwind[i]=mean(vwind_change[,i])
  grid_sd_vwind[i]=sd(vwind_change[,i])
}
grid_sd_rel_humid = NULL
grid_mean_rel_humid =NULL
for (i in 1:925) {
  grid_mean_rel_humid[i]=mean(rel_humid_change[,i])
  grid_sd_rel_humid[i]=sd(rel_humid_change[,i])
}

#Generating multivariate score
score = matrix(,nrow = 15339,ncol = 925)
for (i in 13001:15339) {
  for (j in 1:925) {
    air = ((air_change[i,j] - grid_mean_air[j])**2)/(2*grid_sd_air[j]*grid_sd_air[j])
    uwind= ((uwind_change[i,j] - grid_mean_uwind[j])**2)/(2*grid_sd_uwind[j]*grid_sd_uwind[j])
    vwind= ((vwind_change[i,j] - grid_mean_vwind[j])**2)/(2*grid_sd_vwind[j]*grid_sd_vwind[j])
    rel_humid = ((rel_humid_change[i,j] - grid_mean_rel_humid[j])**2)/(2*grid_sd_rel_humid[j]*grid_sd_rel_humid[j])
    score[i,j]=air*uwind*vwind*rel_humid
  }
}

#Generating frame
score_fr=as.data.frame(score)

#Storing the score matrix
write.csv(scor,file="Multivariate_Score.csv")

#Generating mean and threshold for each region
up3=0
down3 = 0
for (i in 1:925) {
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

all_outliers3_time=0
all_outliers3_loc=0
all_outliers3_ind=0

for (i in 1:5000) {
  for (j in 1:925) {
    if((score_fr[i,j] > up3[j])==TRUE){
      all_outliers3_time=c(all_outliers3_time,i)
      all_outliers3_loc=c(all_outliers3_loc,j)
      all_outliers3_ind=c(all_outliers_ind,up3[j])
    }
    if((score_fr[i,j] < down3[j])==TRUE){
      all_outliers3_time=c(all_outliers3_time,i)
      all_outliers3_loc=c(all_outliers3_loc,j)
      all_outliers3_ind=c(all_outliers3_ind,down3[j])
    }
  }
}

all_outliers3=cbind(all_outliers3_time,all_outliers3_loc,all_outliers_ind)
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


AnomalyDetectionVec(air_data_2d[,363],direction = 'both',max_anoms = 0.02,alpha = 0.05,plot = TRUE,period = 365)
