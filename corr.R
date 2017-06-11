##created by Kimberley Kirk 06-05-2017

##calculates correlation of completely observed cases for each pollutant variable across various air pollution monitors
corr <- function(directory, threshold = 0) {
  
  ##use returned data table from complete() function
  data_table_result <- complete(directory)
  ##retrieve values where N > threshold and save in variable
  values_greater_than_threshold <- data_table_result[data_table_result$nobs > threshold,]
  
  
  ##conditional: if ID > 0
  ##subset ID column to return a vector of those ID numbers
  if(nrow(values_greater_than_threshold) != 0 ) {
    
    ##get all ID numbers from values_greater_than_threshold
    monitor_ids <- values_greater_than_threshold[ ,1]
    ##get data frame that has all csv files
    csv_files_list_all = list.files(pattern="*.csv")
    ##read all csv files
    all_data_df <- do.call(rbind, lapply(csv_files_list_all, function(x) read.csv(x, colClasses= c("Date", "numeric", "numeric", "integer"), comment.char="" )))
    
    ##get data for specific monitor ID
    ##all_data_subset_IDs <- all_data_df[all_data_df$ID %in% monitor_ids, 2:4 ]
    all_data_subset_IDs <- all_data_df[all_data_df$ID %in% monitor_ids,2:4]
    ##remove NA values
    subset_IDs_no_NA <- all_data_subset_IDs[complete.cases(all_data_subset_IDs), ]
    
    ##holds final vector of correlations
    corr_vector <- c()
    
    ##go through each monitor id and find correlation between sulfate,nitrate
    for(id in monitor_ids) {
      
      ##get dataframe of sulfate and nitrate columns only from current monitor id
      current_monitor_id_df <- subset_IDs_no_NA[subset_IDs_no_NA$ID == id, 1:2]
      
      ##find correlation between sulfate and nitrate values
      correlation_result <- cor(current_monitor_id_df[,1], current_monitor_id_df[,2])
      
      ##apply cor() function to the dataframe you got in previous step
      ##corr_vector <- append(correlation_result, correlation_result, ) }
      corr_vector <- c(corr_vector,correlation_result)
      
    }
    ##conditional: else run next lines of code
    ##if monitor ID does not meet greater than threshold value, spit out vector with length 0
  } else {
      corr_vector <- 0
    }
  return(corr_vector)
  }
