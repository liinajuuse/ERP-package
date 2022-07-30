# Data manipulation for significance analysis of event-related potentials, i.e
# https://cran.r-project.org/web/packages/ERP/vignettes/ERP.html

# Based on Stenio's script "script_sf01.R", but currently leaving out averaging
# steps. Will check if R averaged and analyzer exports produce similar results

# Edited by Liina Juuse, 28.05.2022
# liina.juuse@ut.ee

# Libraries and data ------------------------------------------------------
library(tidyverse)

setwd("C:/Users/liina/OneDrive/PhD/EMO2021/EMO2021/ERP-package/")
input = "C:/Users/liina/OneDrive/PhD/EMO2021/EMO2021/ERP-package/input/"
output = "C:/Users/liina/OneDrive/PhD/EMO2021/EMO2021/ERP-package/output/"

level_key = c("019" = "Anger", "115" = "Anger", 
              "031" = "Happy", "127" = "Happy",
              "015" = "Sad", "111" = "Sad",
              "027" = "Neutral", "123" = "Neutral",
              "007" = "Disgust", "103" = "Disgust",
              "023" = "Fear", "119" = "Fear",
              "011" = "Surprise", "107" = "Surprise")

data.ERP = NULL

for (f in dir(input, pattern = "(.*)dat$")) {
  raw.data <- read.table(paste(input, f, sep="/"), header = T, dec = ".", sep = ";")
  ElectNames <- colnames(raw.data) # store the electrode names. This will be used to assembly the final table
  
  #4 mark bad trials (where +-75 ?V values): sets the values for given electrode to NA
  for (l in unlist(ROId)){
    excludeTrials <- unique(trial[abs(data[,l])>300])
    data[trial %in% excludeTrials, l]<-NA
  }
  
  #8 delete bad trials (where all electrodes in a ROI were set to NA)
  tmp<-tmp[!is.na(tmp$amplitude),]
  
  # Step 3: Flip the table --------------------------------------------------
  
  data.ok <- raw.data[1:615, ] # subset rows (-199 to 1000)
  
  data.ok <- as.data.frame(t(data.ok)) # flip the table
  
  column.names <- paste(rep('T_', ncol(data.ok)), seq(1, ncol(data.ok), by = 1), sep = '')
  
  colnames(data.ok) <- column.names
  
  data.ok <- data.frame('Electrode' = rownames(data.ok), data.ok)
  
  rownames(data.ok) <- NULL
  
  # Step 4: Adding metadata -------------------------------------------------
  
  #f <- list.files(path = "input/", pattern = '\\.dat')[1]
  
  #6 extract the meta info from the name of the file and add the info into the data frame
  data.ok$Subj <- substr(f,6,8)
  data.ok$Group <- substr(f,12,13)
  data.ok$Age <- substr(f,10,11)
  data.ok$Sex <- substr(f,9,9)
  data.ok$Marker <- substr(f, nchar(f)-6,nchar(f)-4)
  data.ok$Condition <- ifelse(data.ok$Marker %in% c("007", "011", "015", "019", "023", "027", "031"), "Ekman", "Verbal") #idk if this will work
  data.ok$Emotion <- data.ok$Marker 
  data.ok$Emotion <- recode(data.ok$Emotion, !!!level_key)
  
  data.ok <- data.ok[, c('Electrode', 'Subj', 'Group', 'Age', 'Sex', 'Marker', 'Condition', 'Emotion', column.names)]

  timestamp()
  print(paste(f, ": Done!"))
  
  data.ERP = rbind(data.ERP, data.ok)
  }

save(data.ERP, file=paste(output,"data-ERP.RData", sep = ''))
write.csv(data.ERP, file = paste(output,"data-ERP.csv", sep = ''))
