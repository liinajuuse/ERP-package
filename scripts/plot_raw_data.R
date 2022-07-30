# Plotting curves - R script 
# Stenio Foerster (stenio.foerster@ut.ee)
# June 2022

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggsci)
library(Rmisc)
library(patchwork)
library(ERP)

setwd("C:/Users/liina/OneDrive/PhD/EMO2021/EMO2021/ERP-package/output")

load(file = 'data-long6.rdata')
load(file = 'data-threshold6-ROI.rdata')

oz = na.omit(oz)
# Organizing the data for plotting purposes

# Ekman -------------------------------------------------------------------

data_long6 = data_long6 %>% subset(Time > -1 & Time < 601) #use this or scale_y_continuous(limits = 0, 600) in the actural graph

data_long6 %>% filter(Condition == 'Ekman') -> ek
unique(ek$Emotion) # Emotions, ok

# Disgust
ek %>% filter(Emotion == 'Disgust') %>% subset(select = -c(Subj, Group, Age, Sex, Marker, Condition, Emotion)) %>% group_by(Time) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> di
di$Emotion <- rep('Disgust', nrow(di))

# Surprise
ek %>% filter(Emotion == 'Surprise') %>% subset(select = -c(Subj, Group, Age, Sex, Marker, Condition, Emotion)) %>% group_by(Time) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> su
su$Emotion <- rep('Surprise', nrow(su))

# Sad
ek %>% filter(Emotion == 'Sad') %>% subset(select = -c(Subj, Group, Age, Sex, Marker, Condition, Emotion)) %>% group_by(Time) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> sa
sa$Emotion <- rep('Sad', nrow(sa))

# Anger
ek %>% filter(Emotion == 'Anger') %>% subset(select = -c(Subj, Group, Age, Sex, Marker, Condition, Emotion)) %>% group_by(Time) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> an
an$Emotion <- rep('Anger', nrow(an))

# Fear
ek %>% filter(Emotion == 'Fear') %>% subset(select = -c(Subj, Group, Age, Sex, Marker, Condition, Emotion)) %>% group_by(Time) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> fe
fe$Emotion <- rep('Fear', nrow(fe))

# Neutral
ek %>% filter(Emotion == 'Neutral') %>% subset(select = -c(Subj, Group, Age, Sex, Marker, Condition, Emotion)) %>% group_by(Time) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> ne
ne$Emotion <- rep('Neutral', nrow(ne))

# Happy
ek %>% filter(Emotion == 'Happy') %>% subset(select = -c(Subj, Group, Age, Sex, Marker, Condition, Emotion)) %>% group_by(Time) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> ha
ha$Emotion <- rep('Happy', nrow(ha))

plot_ek <- rbind(di, su, sa, an, fe, ne, ha) # plot data for Ekman condition (average across participants by time)


# Under -------------------------------------------------------------------

data_long6 %>% filter(Condition == 'Under') -> un
unique(un$Emotion) # Emotions, ok

# Disgust
un %>% filter(Emotion == 'Disgust') %>% subset(select = -c(Subj, Group, Age, Sex, Marker, Condition, Emotion)) %>% group_by(Time) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> di
di$Emotion <- rep('Disgust', nrow(di))

# Surprise
un %>% filter(Emotion == 'Surprise') %>% subset(select = -c(Subj, Group, Age, Sex, Marker, Condition, Emotion)) %>% group_by(Time) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> su
su$Emotion <- rep('Surprise', nrow(su))

# Sad
un %>% filter(Emotion == 'Sad') %>% subset(select = -c(Subj, Group, Age, Sex, Marker, Condition, Emotion)) %>% group_by(Time) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> sa
sa$Emotion <- rep('Sad', nrow(sa))

# Anger
un %>% filter(Emotion == 'Anger') %>% subset(select = -c(Subj, Group, Age, Sex, Marker, Condition, Emotion)) %>% group_by(Time) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> an
an$Emotion <- rep('Anger', nrow(an))

# Fear
un %>% filter(Emotion == 'Fear') %>% subset(select = -c(Subj, Group, Age, Sex, Marker, Condition, Emotion)) %>% group_by(Time) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> fe
fe$Emotion <- rep('Fear', nrow(fe))

# Neutral
un %>% filter(Emotion == 'Neutral') %>% subset(select = -c(Subj, Group, Age, Sex, Marker, Condition, Emotion)) %>% group_by(Time) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> ne
ne$Emotion <- rep('Neutral', nrow(ne))

# Happy
un %>% filter(Emotion == 'Happy') %>% subset(select = -c(Subj, Group, Age, Sex, Marker, Condition, Emotion)) %>% group_by(Time) %>% summarise_all(mean, na.rm = T) %>% as.data.frame() -> ha
ha$Emotion <- rep('Happy', nrow(ha))

plot_un <- rbind(di, su, sa, an, fe, ne, ha) # plot data for Under condition (average across participants by time)

rm(di, su, sa, an, fe, ne, ha, ek, un, data_long6) # we don't need these objects anymore

# Update 15.06.2022 -------------------------------------------------------

## GAM curve plots -----

# Calculating ROIs Ekman condition

roi_visu_ek <- subset(plot_ek, select = c(Time, O1, Oz, O2, P3, Pz, P4, PO3, PO4)) # electrodes visual

roi_verb_ek <- subset(plot_ek, select = c(Time, T7, T8, C5, C6, F7, F8, CP3, CP4)) # electrodes verbal

# Mean and 95% CI (visual)

b <- data.frame()

for (i in 1:nrow(roi_visu_ek)) {
  
  roi_visu_ek[i, 2:9] %>% as.numeric() %>% CI(ci = 0.95) %>% as.data.frame() %>% t() -> a
  b <- rbind(b, a)
}

roi_visu_ek$ROI_visual <- b$mean
roi_visu_ek$ROI_upper95 <- b$upper
roi_visu_ek$ROI_lower95 <- b$lower
roi_visu_ek$Emotion <- plot_ek$Emotion

# Plot
p2 <- ggplot(data = roi_visu_ek, mapping = aes(x = Time, y = ROI_visual, colour = Emotion), alpha = 0.5)
p2 <- p2 + stat_smooth(method = 'gam', se = F, size = 0.3, formula = y ~ s(x, k = 25), n = nrow(roi_visu_ek)) + theme_bw() + scale_color_locuszoom() + geom_hline(yintercept = 0, size = 0.3, colour = 'black', linetype = 'dashed') + geom_vline(xintercept = 0, size = 0.3, colour = 'black', linetype = 'dashed')
p2 <- p2 + ggtitle(label = 'Visual ROI ☓ Visual condition'); p2
p2 <- p2 + scale_y_continuous('Amplitude (μV)', breaks=seq(-2, 7, 2), limits = c(-1.5, 7)) 
p2 <- p2 + scale_x_continuous('Time (ms)') 
p2 <- p2 + theme(panel.grid = element_blank(), legend.position = 'none', panel.border = element_blank(), axis.line.x = element_line(size = 0.3))
# p2 + scale_x_continuous(limits = c(0,600)) #cut dimensions of graph without having to change original data
p2
# Mean and 95% CI (verbal)

b <- data.frame()

for (i in 1:nrow(roi_verb_ek)) {
  
  roi_verb_ek[i, 2:9] %>% as.numeric() %>% CI(ci = 0.95) %>% as.data.frame() %>% t() -> a
  b <- rbind(b, a)
}

roi_verb_ek$ROI_verbal <- b$mean
roi_verb_ek$ROI_upper95 <- b$upper
roi_verb_ek$ROI_lower95 <- b$lower
roi_verb_ek$Emotion <- plot_ek$Emotion

# Plot
p3 <- ggplot(data = roi_verb_ek, mapping = aes(x = Time, y = ROI_verbal, colour = Emotion), alpha = 0.5)
p3 <- p3 + stat_smooth(method = 'gam', se = F, size = 0.3, formula = y ~ s(x, k = 25), n = nrow(roi_verb_ek)) + theme_bw() + scale_color_locuszoom() + geom_hline(yintercept = 0, size = 0.3, colour = 'black', linetype = 'dashed') + geom_vline(xintercept = 0, size = 0.3, colour = 'black', linetype = 'dashed')
p3 <- p3 + ggtitle(label = 'Verbal ROI ☓ Visual Condition'); p3
p3 <- p3 + scale_y_continuous('Amplitude (μV)', breaks=seq(-4, 6, 2), limits = c(-4,5)) 
p3 <- p3 + scale_x_continuous('Time (ms)') 
p3 <- p3 + theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line.x = element_line(size = 0.3))
p3 <- p3 + theme(legend.position = 'none')

p3

p2/p3


# Calculating ROIs Under condition

roi_visu_un <- subset(plot_un, select = c(Time, O1, Oz, O2, P3, Pz, P4, PO3, PO4)) # electrodes visual

roi_verb_un <- subset(plot_un, select = c(Time, T7, T8, C5, C6, F7, F8, CP3, CP4)) # electrodes verbal

# Mean and 95% CI (visual)

b <- data.frame()

for (i in 1:nrow(roi_visu_un)) {
  
  roi_visu_un[i, 2:9] %>% as.numeric() %>% CI(ci = 0.95) %>% as.data.frame() %>% t() -> a
  b <- rbind(b, a)
}

roi_visu_un$ROI_visual <- b$mean
roi_visu_un$ROI_upper95 <- b$upper
roi_visu_un$ROI_lower95 <- b$lower
roi_visu_un$Emotion <- plot_un$Emotion

# Plot
p4 <- ggplot(data = roi_visu_un, mapping = aes(x = Time, y = ROI_visual, colour = Emotion), alpha = 0.5)
p4 <- p4 + stat_smooth(method = 'gam', se = F, size = 0.3, formula = y ~ s(x, k = 25), n = nrow(roi_visu_un)) + theme_bw() + scale_color_locuszoom() + geom_hline(yintercept = 0, size = 0.3, colour = 'black', linetype = 'dashed') + geom_vline(xintercept = 0, size = 0.3, colour = 'black', linetype = 'dashed')
p4 <- p4 + ggtitle(label = 'Visual ROI ☓ Verbal Condition'); p4
p4 <- p4 + theme(legend.position = 'none')
p4 <- p4 + scale_y_continuous('Amplitude (μV)', breaks=seq(-2, 7, 2), limits = c(-1.5, 7)) 
p4 <- p4 + scale_x_continuous('Time (ms)') 
p4 <- p4 + theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line.x = element_line(size = 0.3))
# Mean and 95% CI (verbal)

p7 = p2 / p4
#ggsave(filename = 'ROI-vis-diff-waves.png', plot = p7, device = 'png', width = 16, height = 12, units = 'cm', dpi = 300)


b <- data.frame()

for (i in 1:nrow(roi_verb_un)) {
  
  roi_verb_un[i, 2:9] %>% as.numeric() %>% CI(ci = 0.95) %>% as.data.frame() %>% t() -> a
  b <- rbind(b, a)
}

roi_verb_un$ROI_verbal <- b$mean
roi_verb_un$ROI_upper95 <- b$upper
roi_verb_un$ROI_lower95 <- b$lower
roi_verb_un$Emotion <- plot_un$Emotion

rm(a, b, i) # we don't need these objects anymore

# Plot
p5 <- ggplot(data = roi_verb_un, mapping = aes(x = Time, y = ROI_verbal, colour = Emotion), alpha = 0.5)
p5 <- p5 + stat_smooth(method = 'gam', se = F, size = 0.3, formula = y ~ s(x, k = 25), n = nrow(roi_verb_un)) + theme_bw() + scale_color_locuszoom() + geom_hline(yintercept = 0, size = 0.3, colour = 'black', linetype = 'dashed') + geom_vline(xintercept = 0, size = 0.3, colour = 'black', linetype = 'dashed')
p5 <- p5 + ggtitle(label = 'Verbal ROI ☓ Verbal Condition'); p5
p5 <- p5 + scale_y_continuous('Amplitude (μV)', breaks=seq(-4, 6, 2), limits = c(-4,5)) 
p5 <- p5 + scale_x_continuous('Time (ms)') 
p5 <- p5 + theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line.x = element_line(size = 0.3))
p5

p8 = p3/p5
p8
#ggsave(filename = 'ROI-ver-diff-waves.png', plot = p8, device = 'png', width = 16, height = 12, units = 'cm', dpi = 300)

p6 <- (p2+p4)/(p3+p5)
p6
ggsave(filename = 'ROI-7-vis-ver-4.no-border-2.png', plot = p6, device = 'png', width = 18, height = 10.5, units = 'cm', dpi = 300)


# Update 16.06.2022 -------------------------------------------------------
## erptest plots ----
# Exploring the source code of the function ERP::erpplot. The idea here is to 
# reproduce the plots from the above function in ggplot. 
# There will be a chunk of code for every emotion and condition. 

#load data

# First, some objects that will be used for plotting all emotions
time.pt <- seq(from = -199.22, to = 1033.5187, by = 2.0077) # changed time sequence to match number of columns for ERP data
time.pt = time.pt[100:400]
alpha <- 0.05

# Visual: Happy
test.emo <- na.omit(subset(oz, c(Emotion == 'Happy' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Visual') & (test.emo$Condition == 'Ekman'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Happy', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

data.plot <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Happy', length(time.pt)), 'Condition' = rep('Ekman', length(time.pt)), 'Stimulus' = rep('Visual', length(time.pt)))

test.emo <- na.omit(subset(oz, c(Emotion == 'Happy' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Visual') & (test.emo$Condition == 'Under'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Happy', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Happy', length(time.pt)), 'Condition' = rep('Under', length(time.pt)), 'Stimulus' = rep('Visual', length(time.pt)))
data.plot <- rbind(data.plot, d)

# Visual: Disgust
test.emo <- na.omit(subset(oz, c(Emotion == 'Disgust' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Visual') & (test.emo$Condition == 'Ekman'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Disgust', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Disgust', length(time.pt)), 'Condition' = rep('Ekman', length(time.pt)), 'Stimulus' = rep('Visual', length(time.pt)))
data.plot <- rbind(data.plot, d) # Disgust added

test.emo <- na.omit(subset(oz, c(Emotion == 'Disgust' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Visual') & (test.emo$Condition == 'Under'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Disgust', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Disgust', length(time.pt)), 'Condition' = rep('Under', length(time.pt)), 'Stimulus' = rep('Visual', length(time.pt)))
data.plot <- rbind(data.plot, d) # Disgust added

# Visual: Surprise
test.emo <- na.omit(subset(oz, c(Emotion == 'Surprise' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Visual') & (test.emo$Condition == 'Ekman'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Surprise', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Surprise', length(time.pt)), 'Condition' = rep('Ekman', length(time.pt)), 'Stimulus' = rep('Visual', length(time.pt)))
data.plot <- rbind(data.plot, d) # Surprise added

test.emo <- na.omit(subset(oz, c(Emotion == 'Surprise' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Visual') & (test.emo$Condition == 'Under'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Surprise', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Surprise', length(time.pt)), 'Condition' = rep('Under', length(time.pt)), 'Stimulus' = rep('Visual', length(time.pt)))
data.plot <- rbind(data.plot, d) # Surprise added

# Visual: Sad
test.emo <- na.omit(subset(oz, c(Emotion == 'Sad' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Visual') & (test.emo$Condition == 'Ekman'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Sad', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Sad', length(time.pt)), 'Condition' = rep('Ekman', length(time.pt)), 'Stimulus' = rep('Visual', length(time.pt)))
data.plot <- rbind(data.plot, d) # Sad added

test.emo <- na.omit(subset(oz, c(Emotion == 'Sad' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Visual') & (test.emo$Condition == 'Under'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Sad', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Sad', length(time.pt)), 'Condition' = rep('Under', length(time.pt)), 'Stimulus' = rep('Visual', length(time.pt)))
data.plot <- rbind(data.plot, d) # Sad added

# Visual: Anger
test.emo <- na.omit(subset(oz, c(Emotion == 'Anger' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Visual') & (test.emo$Condition == 'Ekman'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Anger', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Anger', length(time.pt)), 'Condition' = rep('Ekman', length(time.pt)), 'Stimulus' = rep('Visual', length(time.pt)))
data.plot <- rbind(data.plot, d) # Anger added

test.emo <- na.omit(subset(oz, c(Emotion == 'Anger' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Visual') & (test.emo$Condition == 'Under'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Anger', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Anger', length(time.pt)), 'Condition' = rep('Under', length(time.pt)), 'Stimulus' = rep('Visual', length(time.pt)))
data.plot <- rbind(data.plot, d) # Anger added

# Visual: Fear
test.emo <- na.omit(subset(oz, c(Emotion == 'Fear' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Visual') & (test.emo$Condition == 'Ekman'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Fear', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Fear', length(time.pt)), 'Condition' = rep('Ekman', length(time.pt)), 'Stimulus' = rep('Visual', length(time.pt)))
data.plot <- rbind(data.plot, d) # Fear added

test.emo <- na.omit(subset(oz, c(Emotion == 'Fear' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Visual') & (test.emo$Condition == 'Under'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Fear', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Fear', length(time.pt)), 'Condition' = rep('Under', length(time.pt)), 'Stimulus' = rep('Visual', length(time.pt)))
data.plot <- rbind(data.plot, d) # Fear added

# Verbal: Happy
test.emo <- na.omit(subset(oz, c(Emotion == 'Happy' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Verbal') & (test.emo$Condition == 'Under'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Happy', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Happy', length(time.pt)), 'Condition' = rep('Under', length(time.pt)), 'Stimulus' = rep('Verbal', length(time.pt)))
data.plot <- rbind(data.plot, d) # Happy added

test.emo <- na.omit(subset(oz, c(Emotion == 'Happy' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Verbal') & (test.emo$Condition == 'Ekman'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Happy', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Happy', length(time.pt)), 'Condition' = rep('Ekman', length(time.pt)), 'Stimulus' = rep('Verbal', length(time.pt)))
data.plot <- rbind(data.plot, d) # Happy added

# Verbal: Disgust
test.emo <- na.omit(subset(oz, c(Emotion == 'Disgust' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Verbal') & (test.emo$Condition == 'Under'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Disgust', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Disgust', length(time.pt)), 'Condition' = rep('Under', length(time.pt)), 'Stimulus' = rep('Verbal', length(time.pt)))
data.plot <- rbind(data.plot, d) # Disgust added

test.emo <- na.omit(subset(oz, c(Emotion == 'Disgust' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Verbal') & (test.emo$Condition == 'Ekman'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Disgust', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Disgust', length(time.pt)), 'Condition' = rep('Ekman', length(time.pt)), 'Stimulus' = rep('Verbal', length(time.pt)))
data.plot <- rbind(data.plot, d) # Disgust added

# Verbal: Surprise
test.emo <- na.omit(subset(oz, c(Emotion == 'Surprise' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Verbal') & (test.emo$Condition == 'Under'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Surprise', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Surprise', length(time.pt)), 'Condition' = rep('Under', length(time.pt)), 'Stimulus' = rep('Verbal', length(time.pt)))
data.plot <- rbind(data.plot, d) # Surprise added

test.emo <- na.omit(subset(oz, c(Emotion == 'Surprise' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Verbal') & (test.emo$Condition == 'Ekman'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Surprise', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Surprise', length(time.pt)), 'Condition' = rep('Ekman', length(time.pt)), 'Stimulus' = rep('Verbal', length(time.pt)))
data.plot <- rbind(data.plot, d) # Surprise added

# Verbal: Sad
test.emo <- na.omit(subset(oz, c(Emotion == 'Sad' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Verbal') & (test.emo$Condition == 'Under'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Sad', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Sad', length(time.pt)), 'Condition' = rep('Under', length(time.pt)), 'Stimulus' = rep('Verbal', length(time.pt)))
data.plot <- rbind(data.plot, d) # Sad added

test.emo <- na.omit(subset(oz, c(Emotion == 'Sad' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Verbal') & (test.emo$Condition == 'Ekman'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Sad', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Sad', length(time.pt)), 'Condition' = rep('Ekman', length(time.pt)), 'Stimulus' = rep('Verbal', length(time.pt)))
data.plot <- rbind(data.plot, d) # Sad added

# Verbal: Anger
test.emo <- na.omit(subset(oz, c(Emotion == 'Anger' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Verbal') & (test.emo$Condition == 'Under'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Anger', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Anger', length(time.pt)), 'Condition' = rep('Under', length(time.pt)), 'Stimulus' = rep('Verbal', length(time.pt)))
data.plot <- rbind(data.plot, d) # Anger added

test.emo <- na.omit(subset(oz, c(Emotion == 'Anger' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Verbal') & (test.emo$Condition == 'Ekman'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Anger', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Anger', length(time.pt)), 'Condition' = rep('Ekman', length(time.pt)), 'Stimulus' = rep('Verbal', length(time.pt)))
data.plot <- rbind(data.plot, d) # Anger added

# Verbal: Fear
test.emo <- na.omit(subset(oz, c(Emotion == 'Fear' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Verbal') & (test.emo$Condition == 'Under'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Fear', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Fear', length(time.pt)), 'Condition' = rep('Under', length(time.pt)), 'Stimulus' = rep('Verbal', length(time.pt)))
data.plot <- rbind(data.plot, d) # Fear added

test.emo <- na.omit(subset(oz, c(Emotion == 'Fear' | Emotion == 'Neutral')))
test.emo <- test.emo[, c(1:9, 109:409)] # 0ms to 600ms
test.emo <- subset(test.emo, (test.emo$ROI == 'Verbal') & (test.emo$Condition == 'Ekman'))
test.emo <- droplevels(test.emo)
data.emo <- subset(test.emo, select = -c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # amplitude data
cova.emo <- subset(test.emo, select = c(Electrode, Subj, Group, Age, Sex, Marker, Condition, Emotion, ROI)) # descriptive data
cova.emo$Subj <- as.factor(cova.emo$Subj) # required to generate the model matrix
cova.emo$Emotion <- factor(cova.emo$Emotion, c('Fear', 'Neutral')) # required to generate the model matrix
mat1.emo <- model.matrix(~ C(Subj, sum) + Emotion, data = cova.emo) # model matrix
mat0.emo <- model.matrix(~ C(Subj, sum), data = cova.emo) # null model matrix

# plotting data
fit.emo <- erptest(dta = data.emo, design = mat1.emo, design0 = mat0.emo, nbs = 20, method = 'BH')
sig.emo <- as.numeric(fit.emo$signal)
sds.emo <- as.numeric(fit.emo$sdsignal)
lwr.emo <- sig.emo-qnorm(1-alpha/2)*sds.emo
upr.emo <- sig.emo+qnorm(1-alpha/2)*sds.emo

d <- data.frame('Time' = time.pt, 'Signal' = sig.emo, 'Lower' = lwr.emo, 'Upper' = upr.emo, 'FStat' = fit.emo$test, 'pval' = fit.emo$correctedpval, 'R2' = fit.emo$r2, 'Emotion' = rep('Fear', length(time.pt)), 'Condition' = rep('Ekman', length(time.pt)), 'Stimulus' = rep('Verbal', length(time.pt)))
data.plot <- rbind(data.plot, d) # Fear added

write.csv(x = data.plot, file = 'data_plot.csv')


# Graphs ------------------------------------------------------------------
setwd("C:/Users/liina/OneDrive/PhD/EMO2021/EMO2021/ERP-package/output")
data_plot <- read.csv('data_plot.csv', header = T, row.names = 1)

data_plot$Condition <- gsub(pattern = 'Ekman', replacement = 'Face', x = data_plot$Condition)
data_plot$Condition <- gsub(pattern = 'Under', replacement = 'Word', x = data_plot$Condition)

data_plot %>% filter(Stimulus == 'Visual') -> data_vis
data_plot %>% filter(Stimulus == 'Verbal') -> data_ver


# Visual panel
data_plot %>% filter(Stimulus == 'Visual') -> data_vis2
data_plot %>% filter(Stimulus == 'Verbal') -> data_ver2
data_vis2$y1 <- rep(-2.5, nrow(data_vis2))
data_vis2$y2 <- rep(-2.4, nrow(data_vis2))
data_ver2$y1 <- rep(-2.5, nrow(data_ver2))
data_ver2$y2 <- rep(-2.3, nrow(data_ver2))

data_vis2$Emotion <- gsub(pattern = 'Anger', replacement = 'Anger - Neutral', x = data_vis2$Emotion)
data_vis2$Emotion <- gsub(pattern = 'Disgust', replacement = 'Disgust - Neutral', x = data_vis2$Emotion)
data_vis2$Emotion <- gsub(pattern = 'Fear', replacement = 'Fear - Neutral', x = data_vis2$Emotion)
data_vis2$Emotion <- gsub(pattern = 'Happy', replacement = 'Happy - Neutral', x = data_vis2$Emotion)
data_vis2$Emotion <- gsub(pattern = 'Sad', replacement = 'Sad - Neutral', x = data_vis2$Emotion)
data_vis2$Emotion <- gsub(pattern = 'Surprise', replacement = 'Surprise - Neutral', x = data_vis2$Emotion)
data_vis2$Condition <- gsub(pattern = 'Under', replacement = 'Verbal', x = data_vis2$Condition)

data_ver2$Emotion <- gsub(pattern = 'Anger', replacement = 'Anger - Neutral', x = data_ver2$Emotion)
data_ver2$Emotion <- gsub(pattern = 'Disgust', replacement = 'Disgust - Neutral', x = data_ver2$Emotion)
data_ver2$Emotion <- gsub(pattern = 'Fear', replacement = 'Fear - Neutral', x = data_ver2$Emotion)
data_ver2$Emotion <- gsub(pattern = 'Happy', replacement = 'Happy - Neutral', x = data_ver2$Emotion)
data_ver2$Emotion <- gsub(pattern = 'Sad', replacement = 'Sad - Neutral', x = data_ver2$Emotion)
data_ver2$Emotion <- gsub(pattern = 'Surprise', replacement = 'Surprise - Neutral', x = data_ver2$Emotion)
data_ver2$Condition <- gsub(pattern = 'Under', replacement = 'Verbal', x = data_ver2$Condition)

# Visual
p1 <- ggplot(data = data_vis2, mapping = aes(x = Time, y = Signal, colour = Condition)) + geom_line(size = 0.3)
p1 <- p1 + geom_ribbon(mapping = aes(ymin = Lower, ymax = Upper, fill = Condition), alpha = 0.2, colour = NA) + theme_bw()
p1 <- p1 + theme(panel.grid = element_blank(), strip.background = element_rect(colour="black", fill="grey93"), panel.border = element_blank(), axis.line.x = element_line(size = 0.3)) + ylab('Condition effect') + xlab("Time (ms)")
p1 <- p1 + geom_hline(yintercept = 0, size = 0.3, linetype = 'dashed')
p1 <- p1 + geom_point(data = filter(data_vis2, pval < 0.05 & Condition == 'Face'), mapping = aes(x = Time, y = y2), size = 0.7, shape = 15)
p1 <- p1 + geom_point(data = filter(data_vis2, pval < 0.05 & Condition == 'Word'), mapping = aes(x = Time, y = y1), size = 0.7, shape = 15)
p1 <- p1 + facet_wrap(~Emotion, ncol = 2) + ggtitle(label = '(A) Visual Cortex')
p1 <- p1 + scale_fill_manual(values = c('purple3', 'orange3')) + scale_color_manual(values = c('purple3', 'orange3'))
p1 <- p1 + scale_x_continuous(breaks=seq(0, 600, 100)) #cut dimensions of graph without having to change original data
p1
# Verbal
p2 <- ggplot(data = data_ver2, mapping = aes(x = Time, y = Signal, colour = Condition)) + geom_line(size = 0.3)
p2 <- p2 + geom_ribbon(mapping = aes(ymin = Lower, ymax = Upper, fill = Condition), alpha = 0.2, colour = NA) + theme_bw()
p2 <- p2 + theme(panel.grid = element_blank(), strip.background = element_rect(colour="black", fill="grey93"), panel.border = element_blank(), axis.line.x = element_line(size = 0.3)) + ylab('Condition effect') + xlab("Time (ms)")
p2 <- p2 + geom_hline(yintercept = 0, size = 0.3, linetype = 'dashed')
p2 <- p2 + geom_point(data = filter(data_ver2, pval < 0.05 & Condition == 'Face'), mapping = aes(x = Time, y = y2), size = 0.7, shape = 15)
p2 <- p2 + geom_point(data = filter(data_ver2, pval < 0.05 & Condition == 'Word'), mapping = aes(x = Time, y = y1), size = 0.7, shape = 15)
p2 <- p2 + facet_wrap(~Emotion, ncol = 2) + ggtitle(label = '(B) Auditory Cortex')
p2 <- p2 + scale_fill_manual(values = c('purple3', 'orange3')) + scale_color_manual(values = c('purple3', 'orange3'))
p2

setwd("C:/Users/liina/OneDrive/PhD/EMO2021/EMO2021/ERP-package/figures")
ggsave(filename = 'ROI-vis-diff-waves.png', plot = p1, device = 'png', width = 16, height = 19.5, units = 'cm', dpi = 300)
ggsave(filename = 'ROI-ver-diff-waves.png', plot = p2, device = 'png', width = 16, height = 19.5, units = 'cm', dpi = 300)