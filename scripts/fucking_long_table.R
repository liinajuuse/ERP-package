# R script - fucking long table
# Stenio F. 

library(tidyverse)

setwd("C:/Users/liina/OneDrive/PhD/EMO2021/EMO2021/ERP-package/output")

load('data-long6.rdata')

d <- data_long6[which(data_long6$Time <= 600), ]

d <- subset(d, select = c(Subj, Group, Age, Sex, Marker, Condition, Emotion, Time, T7, T8, C5, C6, F7, F8, CP3, CP4, O1, Oz, O2, P3, Pz, P4, PO3, PO4))

rownames(d) <- NULL

d %>% pivot_longer(cols = c('T7', 'T8', 'C5', 'C6', 'F7', 'F8', 'CP3', 'CP4', 'O1', 'Oz', 'O2', 'P3', 'Pz', 'P4', 'PO3', 'PO4'), names_to = 'Electrode', values_to = 'Amplitude') -> e

e %>% mutate(ROI = recode(Electrode, 
                          'O1' = 'Visual',
                          'O2' = 'Visual',
                          'Oz' = 'Visual',
                          'P3' = 'Visual',
                          'Pz' = 'Visual',
                          'P4' = 'Visual',
                          'PO4' = 'Visual',
                          'PO3' = 'Visual',
                          .default = 'Verbal')) -> e


rm(data_long6, d)
e$Sex <- gsub(pattern = 'F', replacement = 'f', x = e$Sex)
e$Group <- gsub(pattern = 'es', replacement = 'ee', x = e$Group)

e = e %>% mutate(Subj = as.factor(Subj), 
                 Group = as.factor(Group),
                 Sex = as.factor(Sex),
                 Condition = factor(Condition, levels = c('Ekman', 'Under')),
                 Emotion = factor(Emotion, levels = c('Neutral', 'Anger', 'Disgust', 'Fear', 'Happy', 'Sad', 'Surprise')),
                 Electrode = as.factor(Electrode),
                 ROI = factor(ROI, levels = c('Visual', 'Verbal')))

save(e, file="f-long-table.RData")


# END
