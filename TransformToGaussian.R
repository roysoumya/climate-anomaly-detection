#Reading values from air_data_2d
air_data_2d = read.csv("Air_Data_2D.csv",header=TRUE,row.names=1)

str(air_data_2d)

hist(air_data_2d$Row3Col1,breaks = seq(225,250,1))
