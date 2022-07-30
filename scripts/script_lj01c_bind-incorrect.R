# Libraries and data ------------------------------------------------------

# get large threshold file
load("C:/Users/liina/OneDrive/PhD/EMO2021/EMO2021/ERP-package/output/data_threshold.RData")
# rename large threshold file
data_threshold -> data_threshold2

# 3 Trials: Threshold rule ----------------------------------------------------------

fi <- data_inspec[data_inspec$Trials == 3, ]$File # dat files with 3 trials. This object will be used in the following steps

# Applying the threshold rule to clean the data

Threshold <- 100 # enter the threshold (micro volts) used to clean the data

nTrials <- 3 # enter the number of trials

data_threshold <- data.frame()
miss_data <- data.frame()


level_key = c("019" = "Anger", "115" = "Anger", 
              "031" = "Happy", "127" = "Happy",
              "015" = "Sad", "111" = "Sad",
              "027" = "Neutral", "123" = "Neutral",
              "007" = "Disgust", "103" = "Disgust",
              "023" = "Fear", "119" = "Fear",
              "011" = "Surprise", "107" = "Surprise")

t0 <- Sys.time()

for (j in 1:length(fi)) {
  
  raw.data <- read.table(file = fi[j], header = T, sep = ';')
  
  # Attributing some info
  nSamplesTrial <- (nrow(raw.data)/nTrials) # this calculates the number of samples in each trial based on the number of rows present in the raw data
  vTrials <- unlist(lapply((1:nTrials), FUN = rep, nSamplesTrial)) # this will generate a vector containing the trials. Each trial is repeated "nSampleTrial" times. The vector length is equal to the number of rows in the raw data
  ElectNames <- colnames(raw.data) # store the electrode names. This will be used to assembly the final table
  
  data_01 <- data.frame('Trial' = vTrials, raw.data) # raw data with assigned trials
  
  # Step 1: Wide table
  # In this step, we will generate a wide table that will be used to average the 
  # electrodes respecting the row order in each trial. This is just an intermediary 
  # step, you don't need to pay attention to the structure of the 
  # objects produced in this step.
  
  u <- unique(vTrials) # a simple vector containing the trial ids
  d <- matrix(nrow = nSamplesTrial) # this is just a backbone matrix used to concatenate the following matrices in a single object (don't pay attention to this object)
  
  for (i in 1:length(u)) {
    
    a <- data_01[data_01$Trial == u[i], ]
    
    d <- cbind(d, a)
  }
  
  # Step 2: Averaging trials
  # Averaging the trials, respecting the row order. For example, if there are two trials with two rows each, 
  # then the code below will calculate the the average between: 1) the values present in the first cell of
  # each trial; and 2) the values present in the second cell of each trial. 
  
  v <- paste(paste('\\b', ElectNames, sep = ''), '\\b', sep = '') # vector with electrode names in a regular expression format. Don't change this. This will make sure that the electrode names will be correctly called during the loop.
  f <- matrix(nrow = nSamplesTrial)
  u <- unique(vTrials) # a simple vector containing the trial ids

  
  for (i in 1:length(v)) {
    
    a <- d[, grep(colnames(d), pattern = v[i], ignore.case = F)]
    #s <- (apply(a, MARGIN = 2, FUN = sd) * 2) + abs(apply(a, MARGIN = 2, FUN = mean)) # two standard deviations of the mean
    
    for (k in 1:ncol(a)) {
      
      a[, k][a[, k] > Threshold] <- NA # assigning NA to values above the user-specified threshold
      a[, k][a[, k] < -Threshold] <- NA # assigning NA to values below (more negatives than) the user-specified threshold
      a[, k][a[, k] == 0] <- NA # assigning NA to zero values
      
      # storing missing data
      m <- length(a[is.na(a)]) # amount of NAs attributed to each electrode for each participant across the trials
      p <- substr(fi[j], 6, 8)
      e <- ElectNames[i]
      
      # missing data table
      miss_data <- rbind(miss_data, data.frame('Electrode' = e, 'Participant' = p, 'Trial' = u[k], 'Sel_Threshold' = Threshold, 'Count_data' = nrow(a)*ncol(a), 'Missing_count' = m, 'Missing_perct' = round(m/(nrow(a)*ncol(a))*100, digits = 4)))
    }
    
    # Electrode average table (average among trials, respecting the row order, of course)
    b <- data.frame(apply(a, MARGIN = 1, FUN = mean, na.rm = T))
    colnames(b) <- ElectNames[i]
    f <- cbind(f, b)
  }
  
  data_avgTrials <- f[, -1] # this is the table with the electrodes averaged by trials (averaged respecting the row order)
  #head(data_avgTrials)
  #dim(data_avgTrials)
  
  
  # Step 3: Flip the table
  
  data_02 <- data_avgTrials[1:615, ] # subset rows (-199 to 1000)
  
  data_02 <- as.data.frame(t(data_02)) # flip the table
  
  column.names <- paste(rep('T_', ncol(data_02)), seq(1, ncol(data_02), by = 1), sep = '')
  
  colnames(data_02) <- column.names
  
  data_02 <- data.frame('Electrode' = rownames(data_02), data_02)
  
  rownames(data_02) <- NULL
  
  # Step 4: Adding metadata

  # Extracting the meta info from the name of the file and add the info into the data frame
  #tmp$stdv <- substr(fi[j],nchar(fi[j])-8,nchar(fi[j])-7)
  data_02$Subj <- substr(fi[j], 6, 8)
  data_02$Group <- substr(fi[j], 12, 13)
  data_02$Age <- substr(fi[j], 10, 11)
  data_02$Sex <- substr(fi[j],9 ,9)
  data_02$Marker <- substr(fi[j], nchar(fi[j])-6, nchar(fi[j])-4)
  data_02$Condition <- ifelse(data_02$Marker %in% c("007", "011", "015", "019", "023", "027", "031"), "Ekman", "Under")
  data_02$Emotion <- data_02$Marker
  data_02$Emotion <- recode(data_02$Emotion, !!!level_key)
  #tmp$order <- substr(fi[j], 12, 12)
  #tmp$series <- substr(fi[j], 14, nchar(fi[j])-29)
  #tmp$eventType <- substr(fi[j], nchar(fi[j])-8, nchar(fi[j])-4)
  
  data_02 <- data_02[, c('Electrode', 'Subj', 'Group', 'Age', 'Sex', 'Marker', 'Condition', 'Emotion', column.names)]
  
  data_threshold <- rbind(data_threshold, data_02)
  
  print(paste(fi[j], 'Done!', sep = ' '))
}

data_threshold3 = data_threshold

(Sys.time()-t0)


# 4 Trials: Threshold rule ----------------------------------------------------------

fi <- data_inspec[data_inspec$Trials == 4, ]$File # dat files with 3 trials. This object will be used in the following steps

# Applying the threshold rule to clean the data

Threshold <- 100 # enter the threshold (micro volts) used to clean the data

nTrials <- 4 # enter the number of trials

data_threshold <- data.frame()
miss_data <- data.frame()

t0 <- Sys.time()

for (j in 1:length(fi)) {
  
  raw.data <- read.table(file = fi[j], header = T, sep = ';')
  
  # Attributing some info
  nSamplesTrial <- (nrow(raw.data)/nTrials) # this calculates the number of samples in each trial based on the number of rows present in the raw data
  vTrials <- unlist(lapply((1:nTrials), FUN = rep, nSamplesTrial)) # this will generate a vector containing the trials. Each trial is repeated "nSampleTrial" times. The vector length is equal to the number of rows in the raw data
  ElectNames <- colnames(raw.data) # store the electrode names. This will be used to assembly the final table
  
  data_01 <- data.frame('Trial' = vTrials, raw.data) # raw data with assigned trials
  
  # Step 1: Wide table
  # In this step, we will generate a wide table that will be used to average the 
  # electrodes respecting the row order in each trial. This is just an intermediary 
  # step, you don't need to pay attention to the structure of the 
  # objects produced in this step.
  
  u <- unique(vTrials) # a simple vector containing the trial ids
  d <- matrix(nrow = nSamplesTrial) # this is just a backbone matrix used to concatenate the following matrices in a single object (don't pay attention to this object)
  
  for (i in 1:length(u)) {
    
    a <- data_01[data_01$Trial == u[i], ]
    
    d <- cbind(d, a)
  }
  
  # Step 2: Averaging trials
  # Averaging the trials, respecting the row order. For example, if there are two trials with two rows each, 
  # then the code below will calculate the the average between: 1) the values present in the first cell of
  # each trial; and 2) the values present in the second cell of each trial. 
  
  v <- paste(paste('\\b', ElectNames, sep = ''), '\\b', sep = '') # vector with electrode names in a regular expression format. Don't change this. This will make sure that the electrode names will be correctly called during the loop.
  f <- matrix(nrow = nSamplesTrial)
  u <- unique(vTrials) # a simple vector containing the trial ids

  
  for (i in 1:length(v)) {
    
    a <- d[, grep(colnames(d), pattern = v[i], ignore.case = F)]
    #s <- (apply(a, MARGIN = 2, FUN = sd) * 2) + abs(apply(a, MARGIN = 2, FUN = mean)) # two standard deviations of the mean
    
    for (k in 1:ncol(a)) {
      
      a[, k][a[, k] > Threshold] <- NA # assigning NA to values above the user-specified threshold
      a[, k][a[, k] < -Threshold] <- NA # assigning NA to values below (more negatives than) the user-specified threshold
      a[, k][a[, k] == 0] <- NA # assigning NA to zero values
      
      # storing missing data
      m <- length(a[is.na(a)]) # amount of NAs attributed to each electrode for each participant across the trials
      p <- substr(fi[j], 6, 8)
      e <- ElectNames[i]
      
      # missing data table
      miss_data <- rbind(miss_data, data.frame('Electrode' = e, 'Participant' = p, 'Trial' = u[k], 'Sel_Threshold' = Threshold, 'Count_data' = nrow(a)*ncol(a), 'Missing_count' = m, 'Missing_perct' = round(m/(nrow(a)*ncol(a))*100, digits = 4)))
    }
    
    # Electrode average table (average among trials, respecting the row order, of course)
    b <- data.frame(apply(a, MARGIN = 1, FUN = mean, na.rm = T))
    colnames(b) <- ElectNames[i]
    f <- cbind(f, b)
  }
  
  data_avgTrials <- f[, -1] # this is the table with the electrodes averaged by trials (averaged respecting the row order)
  #head(data_avgTrials)
  #dim(data_avgTrials)
  
  
  # Step 3: Flip the table
  
  data_02 <- data_avgTrials[1:615, ] # subset rows (-199 to 1000)
  
  data_02 <- as.data.frame(t(data_02)) # flip the table
  
  column.names <- paste(rep('T_', ncol(data_02)), seq(1, ncol(data_02), by = 1), sep = '')
  
  colnames(data_02) <- column.names
  
  data_02 <- data.frame('Electrode' = rownames(data_02), data_02)
  
  rownames(data_02) <- NULL
  
  # Step 4: Adding metadata

  # Extracting the meta info from the name of the file and add the info into the data frame
  #tmp$stdv <- substr(fi[j],nchar(fi[j])-8,nchar(fi[j])-7)
  data_02$Subj <- substr(fi[j], 6, 8)
  data_02$Group <- substr(fi[j], 12, 13)
  data_02$Age <- substr(fi[j], 10, 11)
  data_02$Sex <- substr(fi[j],9 ,9)
  data_02$Marker <- substr(fi[j], nchar(fi[j])-6, nchar(fi[j])-4)
  data_02$Condition <- ifelse(data_02$Marker %in% c("007", "011", "015", "019", "023", "027", "031"), "Ekman", "Under")
  data_02$Emotion <- data_02$Marker
  data_02$Emotion <- recode(data_02$Emotion, !!!level_key)
  #tmp$order <- substr(fi[j], 12, 12)
  #tmp$series <- substr(fi[j], 14, nchar(fi[j])-29)
  #tmp$eventType <- substr(fi[j], nchar(fi[j])-8, nchar(fi[j])-4)
  
  data_02 <- data_02[, c('Electrode', 'Subj', 'Group', 'Age', 'Sex', 'Marker', 'Condition', 'Emotion', column.names)]
  
  data_threshold <- rbind(data_threshold, data_02)
  
  print(paste(fi[j], 'Done!', sep = ' '))
}

data_threshold4 = data_threshold

# 5 Trials: Threshold rule ----------------------------------------------------------

fi <- data_inspec[data_inspec$Trials == 5, ]$File # dat files with 3 trials. This object will be used in the following steps

# Applying the threshold rule to clean the data

Threshold <- 100 # enter the threshold (micro volts) used to clean the data

nTrials <- 5 # enter the number of trials

data_threshold <- data.frame()
miss_data <- data.frame()

t0 <- Sys.time()

for (j in 1:length(fi)) {
  
  raw.data <- read.table(file = fi[j], header = T, sep = ';')
  
  # Attributing some info
  nSamplesTrial <- (nrow(raw.data)/nTrials) # this calculates the number of samples in each trial based on the number of rows present in the raw data
  vTrials <- unlist(lapply((1:nTrials), FUN = rep, nSamplesTrial)) # this will generate a vector containing the trials. Each trial is repeated "nSampleTrial" times. The vector length is equal to the number of rows in the raw data
  ElectNames <- colnames(raw.data) # store the electrode names. This will be used to assembly the final table
  
  data_01 <- data.frame('Trial' = vTrials, raw.data) # raw data with assigned trials
  
  # Step 1: Wide table
  # In this step, we will generate a wide table that will be used to average the 
  # electrodes respecting the row order in each trial. This is just an intermediary 
  # step, you don't need to pay attention to the structure of the 
  # objects produced in this step.
  
  u <- unique(vTrials) # a simple vector containing the trial ids
  d <- matrix(nrow = nSamplesTrial) # this is just a backbone matrix used to concatenate the following matrices in a single object (don't pay attention to this object)
  
  for (i in 1:length(u)) {
    
    a <- data_01[data_01$Trial == u[i], ]
    
    d <- cbind(d, a)
  }
  
  # Step 2: Averaging trials
  # Averaging the trials, respecting the row order. For example, if there are two trials with two rows each, 
  # then the code below will calculate the the average between: 1) the values present in the first cell of
  # each trial; and 2) the values present in the second cell of each trial. 
  
  v <- paste(paste('\\b', ElectNames, sep = ''), '\\b', sep = '') # vector with electrode names in a regular expression format. Don't change this. This will make sure that the electrode names will be correctly called during the loop.
  f <- matrix(nrow = nSamplesTrial)
  u <- unique(vTrials) # a simple vector containing the trial ids
  
  for (i in 1:length(v)) {
    
    a <- d[, grep(colnames(d), pattern = v[i], ignore.case = F)]
    #s <- (apply(a, MARGIN = 2, FUN = sd) * 2) + abs(apply(a, MARGIN = 2, FUN = mean)) # two standard deviations of the mean
    
    for (k in 1:ncol(a)) {
      
      a[, k][a[, k] > Threshold] <- NA # assigning NA to values above the user-specified threshold
      a[, k][a[, k] < -Threshold] <- NA # assigning NA to values below (more negatives than) the user-specified threshold
      a[, k][a[, k] == 0] <- NA # assigning NA to zero values
      
      # storing missing data
      m <- length(a[is.na(a)]) # amount of NAs attributed to each electrode for each participant across the trials
      p <- substr(fi[j], 6, 8)
      e <- ElectNames[i]
      
      # missing data table
      miss_data <- rbind(miss_data, data.frame('Electrode' = e, 'Participant' = p, 'Trial' = u[k], 'Sel_Threshold' = Threshold, 'Count_data' = nrow(a)*ncol(a), 'Missing_count' = m, 'Missing_perct' = round(m/(nrow(a)*ncol(a))*100, digits = 4)))
    }
    
    # Electrode average table (average among trials, respecting the row order, of course)
    b <- data.frame(apply(a, MARGIN = 1, FUN = mean, na.rm = T))
    colnames(b) <- ElectNames[i]
    f <- cbind(f, b)
  }
  
  data_avgTrials <- f[, -1] # this is the table with the electrodes averaged by trials (averaged respecting the row order)
  #head(data_avgTrials)
  #dim(data_avgTrials)
  
  
  # Step 3: Flip the table
  
  data_02 <- data_avgTrials[1:615, ] # subset rows (-199 to 1000)
  
  data_02 <- as.data.frame(t(data_02)) # flip the table
  
  column.names <- paste(rep('T_', ncol(data_02)), seq(1, ncol(data_02), by = 1), sep = '')
  
  colnames(data_02) <- column.names
  
  data_02 <- data.frame('Electrode' = rownames(data_02), data_02)
  
  rownames(data_02) <- NULL
  
  # Step 4: Adding metadata

  # Extracting the meta info from the name of the file and add the info into the data frame
  #tmp$stdv <- substr(fi[j],nchar(fi[j])-8,nchar(fi[j])-7)
  data_02$Subj <- substr(fi[j], 6, 8)
  data_02$Group <- substr(fi[j], 12, 13)
  data_02$Age <- substr(fi[j], 10, 11)
  data_02$Sex <- substr(fi[j],9 ,9)
  data_02$Marker <- substr(fi[j], nchar(fi[j])-6, nchar(fi[j])-4)
  data_02$Condition <- ifelse(data_02$Marker %in% c("007", "011", "015", "019", "023", "027", "031"), "Ekman", "Under")
  data_02$Emotion <- data_02$Marker
  data_02$Emotion <- recode(data_02$Emotion, !!!level_key)
  #tmp$order <- substr(fi[j], 12, 12)
  #tmp$series <- substr(fi[j], 14, nchar(fi[j])-29)
  #tmp$eventType <- substr(fi[j], nchar(fi[j])-8, nchar(fi[j])-4)
  
  data_02 <- data_02[, c('Electrode', 'Subj', 'Group', 'Age', 'Sex', 'Marker', 'Condition', 'Emotion', column.names)]
  
  data_threshold <- rbind(data_threshold, data_02)
  
  print(paste(fi[j], 'Done!', sep = ' '))
}

data_threshold5 = data_threshold

# combine all dataframes
data_threshold6 = rbind(data_threshold2, data_threshold3, data_threshold4, data_threshold5)

#save combined data frame
save(data_threshold6, file="data-threshold6.RData")
