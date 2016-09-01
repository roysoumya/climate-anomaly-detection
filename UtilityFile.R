library(dplyr)

#Utility Script to convert 15340 days(i.e from 1969 to 2010) to (i.e, from 1951 to 2014)
cal_data = read.csv('~/R/Twitter India/Utility Files/Calendar_Daily_15340.csv',header=TRUE,row.names = 1)

#Selecting portion for 2011 to 2014(2003 to 2006)
select_yr = cal_data %>% filter(year >= 2003)
select_yr = select_yr %>% filter(year <= 2006)

select_yr1 = select_yr %>% mutate(year1 = year+8)
select_yr1 = select_yr1 %>% select(-year)
colnames(select_yr1)[5] = 'year'

#Adding at the end
cal_data = rbind(cal_data,select_yr1)

#Selecting portion for 1951 to 1968
select_yr = cal_data %>% filter(year >= 1979)
select_yr = select_yr %>% filter(year <= 1996)

select_yr1 = select_yr %>% mutate(year1 = year-28)
select_yr1 = select_yr1 %>% select(-year)
colnames(select_yr1)[5] = 'year'

#Adding at the beginning
cal_data = rbind(select_yr1,cal_data)
cal_data = select(cal_data,-X,-index)
index = seq(1,nrow(cal_data),1)
cal_data = cbind(index,cal_data)

cal_data = rbind(cal_data,cal_data,cal_data,cal_data)
cal_arr = cal_data %>% arrange(index)

#Writing seq to understand which quarter
d = seq(1,4,1)
quarter = rep(d,23376)

cal_arr = cbind(quarter,cal_arr)

index = seq(1,nrow(cal_arr),1)
cal_arr = select(cal_arr,-index)
cal_data = cbind(index,cal_arr)

#Writing the information to .csv files
write.csv(cal_data,file = 'Calendar_1951_to_2014.csv')
