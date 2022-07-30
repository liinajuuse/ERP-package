# Averaging the data to plot the topographical things
# Stenio Foerster (stenio.foerster@ut.ee)
# June 2022

library(tidyverse)
library(ggsci)
library(Rmisc)
library(patchwork)
library(ERP)

load(file = 'data-long6.rdata')

# Subtracting each emotion by Neutral

# Neutral
ne <- data_long6[data_long6$Emotion == 'Neutral', ]
ne <- subset(ne, select = -c(Subj, Group, Age, Sex, Marker, Condition, Emotion, Time))

# Disgust - Neutral
em <- data_long6[data_long6$Emotion == 'Disgust', ]
em[, colnames(ne)] <- em[, colnames(ne)] - ne
emo.dif <- em

# Surprise - Neutral
em <- data_long6[data_long6$Emotion == 'Surprise', ]
em[, colnames(ne)] <- em[, colnames(ne)] - ne
emo.dif <- rbind(emo.dif, em)

# Sad - Neutral
em <- data_long6[data_long6$Emotion == 'Sad', ]
em[, colnames(ne)] <- em[, colnames(ne)] - ne
emo.dif <- rbind(emo.dif, em)

# Anger - Neutral
em <- data_long6[data_long6$Emotion == 'Anger', ]
em[, colnames(ne)] <- em[, colnames(ne)] - ne
emo.dif <- rbind(emo.dif, em)

# Fear - Neutral
em <- data_long6[data_long6$Emotion == 'Fear', ]
em[, colnames(ne)] <- em[, colnames(ne)] - ne
emo.dif <- rbind(emo.dif, em)

# Happy - Neutral
em <- data_long6[data_long6$Emotion == 'Happy', ]
em[, colnames(ne)] <- em[, colnames(ne)] - ne
emo.dif <- rbind(emo.dif, em)
rm(em, ne)

# Condensing the data into 100 ms intervals
emo.int <- emo.dif[emo.dif$Time %in% emo.dif$Time[100:399], ]
te <- c(rep('a', 50), rep('b', 50), rep('c', 50), rep('d', 50), rep('e', 50), rep('f', 50))
emo.int <- data.frame('Interval' = te, emo.int)

# Ekman Disgust
emo.int %>% filter(Condition == 'Ekman') %>% filter(Emotion == 'Disgust') -> emo.tem
emo.tem <- subset(emo.tem, select = -c(Time, Subj, Group, Age, Sex, Marker, Condition, Emotion))
emo.tem %>% group_by(Interval) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> emo.tem
emo.tem$Interval <- c('0-100', '100-200', '200-300', '300-400', '400-500', '500-600')
emo.tem <- data.frame('Condition' = rep('Ekman', nrow(emo.tem)), 'Emotion' = rep('Disgust', nrow(emo.tem)), emo.tem)

# Ekman Surprise
emo.int %>% filter(Condition == 'Ekman') %>% filter(Emotion == 'Surprise') -> emo
emo <- subset(emo, select = -c(Time, Subj, Group, Age, Sex, Marker, Condition, Emotion))
emo %>% group_by(Interval) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> emo
emo$Interval <- c('0-100', '100-200', '200-300', '300-400', '400-500', '500-600')
emo <- data.frame('Condition' = rep('Ekman', nrow(emo)), 'Emotion' = rep('Surprise', nrow(emo)), emo)
emo.tem <- rbind(emo.tem, emo)

# Ekman Sad
emo.int %>% filter(Condition == 'Ekman') %>% filter(Emotion == 'Sad') -> emo
emo <- subset(emo, select = -c(Time, Subj, Group, Age, Sex, Marker, Condition, Emotion))
emo %>% group_by(Interval) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> emo
emo$Interval <- c('0-100', '100-200', '200-300', '300-400', '400-500', '500-600')
emo <- data.frame('Condition' = rep('Ekman', nrow(emo)), 'Emotion' = rep('Sad', nrow(emo)), emo)
emo.tem <- rbind(emo.tem, emo)

# Ekman Anger
emo.int %>% filter(Condition == 'Ekman') %>% filter(Emotion == 'Anger') -> emo
emo <- subset(emo, select = -c(Time, Subj, Group, Age, Sex, Marker, Condition, Emotion))
emo %>% group_by(Interval) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> emo
emo$Interval <- c('0-100', '100-200', '200-300', '300-400', '400-500', '500-600')
emo <- data.frame('Condition' = rep('Ekman', nrow(emo)), 'Emotion' = rep('Anger', nrow(emo)), emo)
emo.tem <- rbind(emo.tem, emo)

# Ekman Fear
emo.int %>% filter(Condition == 'Ekman') %>% filter(Emotion == 'Fear') -> emo
emo <- subset(emo, select = -c(Time, Subj, Group, Age, Sex, Marker, Condition, Emotion))
emo %>% group_by(Interval) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> emo
emo$Interval <- c('0-100', '100-200', '200-300', '300-400', '400-500', '500-600')
emo <- data.frame('Condition' = rep('Ekman', nrow(emo)), 'Emotion' = rep('Fear', nrow(emo)), emo)
emo.tem <- rbind(emo.tem, emo)

# Ekman Happy
emo.int %>% filter(Condition == 'Ekman') %>% filter(Emotion == 'Happy') -> emo
emo <- subset(emo, select = -c(Time, Subj, Group, Age, Sex, Marker, Condition, Emotion))
emo %>% group_by(Interval) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> emo
emo$Interval <- c('0-100', '100-200', '200-300', '300-400', '400-500', '500-600')
emo <- data.frame('Condition' = rep('Ekman', nrow(emo)), 'Emotion' = rep('Happy', nrow(emo)), emo)
emo.tem <- rbind(emo.tem, emo)

# Under Disgust
emo.int %>% filter(Condition == 'Under') %>% filter(Emotion == 'Disgust') -> emo
emo <- subset(emo, select = -c(Time, Subj, Group, Age, Sex, Marker, Condition, Emotion))
emo %>% group_by(Interval) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> emo
emo$Interval <- c('0-100', '100-200', '200-300', '300-400', '400-500', '500-600')
emo <- data.frame('Condition' = rep('Under', nrow(emo)), 'Emotion' = rep('Disgust', nrow(emo)), emo)
emo.tem <- rbind(emo.tem, emo)

# Under Surprise
emo.int %>% filter(Condition == 'Under') %>% filter(Emotion == 'Surprise') -> emo
emo <- subset(emo, select = -c(Time, Subj, Group, Age, Sex, Marker, Condition, Emotion))
emo %>% group_by(Interval) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> emo
emo$Interval <- c('0-100', '100-200', '200-300', '300-400', '400-500', '500-600')
emo <- data.frame('Condition' = rep('Under', nrow(emo)), 'Emotion' = rep('Surprise', nrow(emo)), emo)
emo.tem <- rbind(emo.tem, emo)

# Under Sad
emo.int %>% filter(Condition == 'Under') %>% filter(Emotion == 'Sad') -> emo
emo <- subset(emo, select = -c(Time, Subj, Group, Age, Sex, Marker, Condition, Emotion))
emo %>% group_by(Interval) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> emo
emo$Interval <- c('0-100', '100-200', '200-300', '300-400', '400-500', '500-600')
emo <- data.frame('Condition' = rep('Under', nrow(emo)), 'Emotion' = rep('Sad', nrow(emo)), emo)
emo.tem <- rbind(emo.tem, emo)

# Under Anger
emo.int %>% filter(Condition == 'Under') %>% filter(Emotion == 'Anger') -> emo
emo <- subset(emo, select = -c(Time, Subj, Group, Age, Sex, Marker, Condition, Emotion))
emo %>% group_by(Interval) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> emo
emo$Interval <- c('0-100', '100-200', '200-300', '300-400', '400-500', '500-600')
emo <- data.frame('Condition' = rep('Under', nrow(emo)), 'Emotion' = rep('Anger', nrow(emo)), emo)
emo.tem <- rbind(emo.tem, emo)

# Under Fear
emo.int %>% filter(Condition == 'Under') %>% filter(Emotion == 'Fear') -> emo
emo <- subset(emo, select = -c(Time, Subj, Group, Age, Sex, Marker, Condition, Emotion))
emo %>% group_by(Interval) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> emo
emo$Interval <- c('0-100', '100-200', '200-300', '300-400', '400-500', '500-600')
emo <- data.frame('Condition' = rep('Under', nrow(emo)), 'Emotion' = rep('Fear', nrow(emo)), emo)
emo.tem <- rbind(emo.tem, emo)

# Under Happy
emo.int %>% filter(Condition == 'Under') %>% filter(Emotion == 'Happy') -> emo
emo <- subset(emo, select = -c(Time, Subj, Group, Age, Sex, Marker, Condition, Emotion))
emo %>% group_by(Interval) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> emo
emo$Interval <- c('0-100', '100-200', '200-300', '300-400', '400-500', '500-600')
emo <- data.frame('Condition' = rep('Under', nrow(emo)), 'Emotion' = rep('Happy', nrow(emo)), emo)
emo.tem <- rbind(emo.tem, emo)

#write.csv(x = emo.tem, file = 'input_topo.csv')

# END

