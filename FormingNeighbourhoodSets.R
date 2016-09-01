library(dplyr)

#Loading the anomaly database
complete_data = read.csv('Twitter_Anomaly_300hpa_Database.csv', header = TRUE,row.names = 1)

#Starting Temporal Clustering

#Setting the initial cluster number as 1
temp_clus_cnt = 1
cluster_no=NULL
mod_data1 = complete_data %>% arrange(grid_name,index)

#The first row
cluster_no[1] = temp_clus_cnt

for (i in 2:144278) {
  #Checking if the grid names are the same
  if(mod_data1[i,1] == mod_data1[i-1,1]){
    
    #Checking if the spatio-temporal point is temporally related
    if(mod_data1[i,2] == (mod_data1[i-1,2]+1)){
      cluster_no[i] = temp_clus_cnt
    }
    else{
      temp_clus_cnt = temp_clus_cnt+1
      cluster_no[i] = temp_clus_cnt
    }
  }
  else{
    temp_clus_cnt = temp_clus_cnt+1
    cluster_no[i] = temp_clus_cnt
  }
}
temp_clus_data = cbind(mod_data1,temp_cluster_no)

#Writing the results in a file
write.csv(temp_clus_data , file = '300_hpa_Twitter_database_temporal_clustering.csv')


#Starting spatial clustering
complete_data = read.csv('300_hpa_Twitter_database_temporal_clustering.csv', header = TRUE,row.names = 1)

#Phase 1: Form clusters for only adjacent grid numbers. In phase 2: we will also include the row anomalies.
#Setting the initial cluster number as 1
sp_clus_cnt = 1
cluster_no=NULL
mod_data2 = complete_data %>% arrange(index,grid_name)

#The first row
cluster_no[1] = sp_clus_cnt

for (i in 2:144278) {
  #Checking if the time-periods are the same
  if(mod_data2[i,2] == mod_data2[i-1,2]){
    
    #Checking if the spatio-temporal point is spatially related
    if(mod_data2[i,1] == (mod_data2[i-1,1]+1)){
      cluster_no[i] = sp_clus_cnt
    }
    else{
      sp_clus_cnt = sp_clus_cnt+1
      cluster_no[i] = sp_clus_cnt
    }
  }
  else{
    sp_clus_cnt = sp_clus_cnt+1
    cluster_no[i] = sp_clus_cnt
  }
}
sp_cluster_no = cluster_no
sp_clus_data = cbind(mod_data2,sp_cluster_no)

#Writing the results in a file
write.csv(sp_clus_data , file = 'Spatial_Phase_1_300_hpa_Twitter_database_temporal_clustering.csv')

#Starting the phase-2 of spatial clustering
mod_data3 = sp_clus_data %>% arrange(index,col_no,row_no)

for (i in 1:144277) {
  
  curr_clust_no = mod_data3[i,20] #Storing spatial cluster no. of current spatio-temporal point
  curr_row = mod_data3[i,17]
  curr_col = mod_data3[i,18]
  curr_index = mod_data3[i,2]
  
  
  #Searching for adjacent squares in the next row only
  #Because adjacent in the same row has been clustered in phase-1
  #For a given point(row_i,col_i), we have to check 3 points: (row_i +1,col_i -1), (row_i +1,col_i) , (row_i +1,col_i +1)
  #If we get one point satisfied, we do need to check for the remaining points, as they were covered in phase-1
  
  for (j in i+1:144278) {
    next_index = mod_data3[j,2]
    next_row = mod_data3[j,17]
    
    #Checking if it is not on the same day or the next row is not adjacent to the current row
    if(curr_index != next_index || next_row > (curr_row + 1)){
      break
    }
    
    #Skip the records on the same row as the curr_row
    if(next_row == curr_row){
      next
    }
    
    next_col = mod_data3[j,18]
    #Present in the adjacent row of curr_row
    if(next_col > (curr_col+1)){
      break
    }
    else if(abs(next_col - curr_col) < 2){
      #We have a match
      next_clust_no = mod_data3[j,20] #Storing the cluster no of the new adjacent spatio-temporal point
     # mod_data3[j,20] = curr_clust_no  #Marking it to be the part of the same cluster
      
      #Converting all the old cluster no, in all the places where next_clust_no occurs
      for (k in j:144278) {
        if(mod_data3[k,20] == next_clust_no){
          mod_data3[k,20] = curr_clust_no
        }
        else{
          break
        }
      }
      break
    }
    else{
      next
    }
  }
}


#Code for compaction of spatial anomalies

#Sorting according to the sp_cluster_no
sp_arrangement = mod_data3 %>% arrange(sp_cluster_no)
spatial_cluster1 = sp_arrangement$sp_cluster_no

sp_clus = NULL
curr_cnt = 1

#Initial step
sp_clus[1] = curr_cnt

for (i in 2:144278) {
  if(spatial_cluster1[i] == spatial_cluster1[i-1]){
    sp_clus[i] = curr_cnt
  }
  else{
    curr_cnt = curr_cnt + 1
    sp_clus[i] = curr_cnt
  }
}

spatial_clus_no = sp_clus
spatial_arrangement = cbind(sp_arrangement,spatial_clus_no)

write.csv(spatial_arrangement,file = '300_hpa_Spatial_Anomaly_Twitter_Database.csv')

#Completed Spatial Clustering phase-2

#Starting Spatio-temporal clustering
total_data = read.csv('300_hpa_Spatial_Anomaly_Twitter_Database.csv', header = TRUE,row.names=1)
sp_temp_clus_no = total_data$spatial_clus_no

mod_data4 = cbind(total_data,sp_temp_clus_no)

#Adding another column known as anomaly id
anomaly_id  = seq(1,144278,1)
mod_data4 = cbind(anomaly_id,mod_data4)

#Creating a list of vectors where the size of the list = 92159 and each element of the list is a vector containing 
#all the anomaly nos

seperate_tab = select(mod_data4, anomaly_id,grid_name,index,temp_cluster_no,spatial_clus_no)
seperate_tab = seperate_tab %>% arrange(temp_cluster_no)

#List of vectors created and is complete
temp_hash = vector("list",92159)

create_list = seperate_tab[1,1]
prev = seperate_tab[1,4]

for (i in 2:144278) {
  #Generating list of vectors 
  if(seperate_tab[i,4] == prev){
    create_list = c(create_list,seperate_tab[i,1])
    prev = seperate_tab[i,4]
  }
  else{
    temp_hash[[prev]] = create_list
    create_list = seperate_tab[i,1]
    prev = seperate_tab[i,4]
  }
}

temp_sp_clus_no = mod_data4$spatial_clus_no
mod_data4 = cbind(mod_data4,temp_sp_clus_no)

#Using the list of vectors generated and stored in temp_hash, we perform spatio-temporal clustering
flag_cnt = rep(0,144278)

for (i in 1:144278) {
  if(flag_cnt[i] == 0){
    temp_list = temp_hash[[mod_data4[i,20]]]
    curr_temp_sp_clus = mod_data4[i,22]
    for (j in 1:length(temp_list)) {
      loc = temp_list[j]
      mod_data4[loc,22] = curr_temp_sp_clus
      flag_cnt[loc] = 1
    }
  }
}

#Compaction required for the spatio-temporal cluster no

#Sorting according to the sp_cluster_no
sp_arrangement = complete_data %>% arrange(temp_sp_clus_no)
temp_sp_cluster1 = sp_arrangement$temp_sp_clus_no

temp_sp_clus = NULL
curr_cnt = 1

#Initial step
temp_sp_clus[1] = curr_cnt

for (i in 2:144278) {
  if(temp_sp_cluster1[i] == temp_sp_cluster1[i-1]){
    temp_sp_clus[i] = curr_cnt
  }
  else{
    curr_cnt = curr_cnt + 1
    temp_sp_clus[i] = curr_cnt
  }
}

temp_sp_cluster_no = temp_sp_clus
sp_arrangement = select(sp_arrangement, -temp_sp_clus_no)
spatial_arrangement = cbind(sp_arrangement,temp_sp_cluster_no)



#On applying spatio-temporal clustering it reduced to 20432 clusters
freq_store = as.data.frame(sort(table(mod_data4$temp_sp_clus_no),descending=TRUE)[1:100])
write.csv(spatial_arrangement,file = 'Complete_Twitter_anomaly_database.csv')

write.csv(freq_store,file = 'Spatio-Temporal Anomaly Clusters.csv')

