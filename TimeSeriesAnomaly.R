#Reading values from air_data_2d and uwind_data_2d
air_data_2d = read.csv("Air_Data_2D.csv",header=TRUE,row.names=1)

air_first10 = air_data_2d[1:3652,]


air_tr=t(air_data_2d)
air_tr_f=as.data.frame(air_tr)

air_diff=air_tr_f[2] - air_tr_f[1]

for (i in 2:15339) {
  store = air_tr_f[i+1] - air_tr_f[i]
  air_diff=data.frame(air_diff,store)
}

air_work = t(air_diff)
air_work_fr = as.data.frame(air_work)

air_work[1,1]

up3=0
down3 = 0
for (i in 1:925) {
  sto_mean = mean(air_work[,i])
  sto_sd = sd(air_work[,i])
  up3[i]=sto_mean+(3*sto_sd)
  down3[i]=sto_mean-(3*sto_sd)
}

up25=0
down25 = 0
for (i in 1:925) {
  sto_mean = mean(air_work[,i])
  sto_sd = sd(air_work[,i])
  up25[i]=sto_mean+(2.5*sto_sd)
  down25[i]=sto_mean-(2.5*sto_sd)
}

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

new_days=seq(1,15339,1)
new_days=as.data.frame(new_days)
air_work_fr = data.frame(air_work_fr,new_days)
air_work_first10 = air_work_fr[1:3652,]
plot(air_work_fr$new_days,air_work_fr$Row3Col1,type = "l",color="blue")
plot(air_work_first10$new_days,air_work_first10$Row3Col1,type = "l")
plot(air_work_1stYear$new_days,air_work_1stYear$Row3Col1,type = "l",col="blue")
lines(air_work_1stYear$new_days,air_work_1stYear$Row3Col1,type = "l",col="red")
plot(air_work_first5$new_days,air_work_first5$Row3Col1,type = "l")

air_work_1stYear = air_work_fr[1:365,]
air_work_first5 = air_work_fr[1:1826,]

air_work_frNorm = air_data_2d - 238
air_data_2d_date = data.frame(air_data_2d,days_fr)
air_work_frNorm = data.frame(air_work_frNorm,days_fr)
air_work_frNorm_1stYear = air_work_frNorm[1:365,]
air_work_frNorm_first5 = air_work_frNorm[1:1826,]

plot(air_work_frNorm_1stYear$days,air_work_frNorm_1stYear$Row3Col1,type = "l",col="red")
lines(air_work_1stYear$new_days,air_work_1stYear$Row3Col1,type = "l",col="blue")

plot(air_work_frNorm_first5$days,air_work_frNorm_first5$Row3Col1,type = "l",col="red")
lines(air_work_first5$new_days,air_work_first5$Row3Col1,type = "l",col="blue")

write.csv(air_work_fr,file="ChangeAirTime.csv")

summary(air_work_fr)
summary(air_work_first10)

air_data_2d[i]


#Calculating the upper and lower threshold for each sensor
higher=0
lower=0
for (i in 1:925) {
  higher[i]=mean(air_data_2d[i])+sd(air_data_2d[i])
  lower[i]=mean(air_data_2d[i]-sd(air_data_2d[i]))
}
