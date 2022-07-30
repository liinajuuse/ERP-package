# Significance Analysis of Event-Related Potentials, i.e
# https://cran.r-project.org/web/packages/ERP/vignettes/ERP.html
# Liina Juuse, 30.05.2022


# Steo 0: Libraries and data ------------------------------------------------------
library(tidyverse)
library(ERP)
load("C:/Users/liina/OneDrive/PhD/EMO2021/EMO2021/ERP-package/output/data-threshold6.RData")

data_thres = data_threshold
data_thres = na.omit(data_thres)
# selecting small sample for testing
ERP.test <- subset(data_threshold, (Electrode == "O2" |  Electrode == "Oz" |  Electrode == "PO8") &
                   (Emotion == "Happy" | Emotion == "Neutral"))
ERP.test = na.omit(ERP.test)
ERP.test = ERP.test[, 1:408]
rownames(ERP.test) <- NULL # I like to reset the row names every time I apply a filter to delete rows in a data frame, just to keep it more organised

# Step X: divide metadata and erpdata -------------------------------------

# To facilitate subsequent steps in ERP data analysis, some frequently used
# values such as the number of curves (n), the time points sampled in milliseconds
# (time_pt) and the number of time frames (T) are assigned and saved as R objects.
# The levels of subject and channel variables (both are of factor types) are 
# re-ordered along numerical sequence for the former and fronto-posterior axis 
# for the latter.

time_pt <- seq(from = -199.22, to = 1033.5187, by = 2.0077) #changed time sequence to match number of columns for ERP data
time_pt = time_pt[1:400]
T <- length(time_pt)                 
n <- nrow(ERP.test)

#löön lahku metadata ja erpdta

covariates = ERP.test[, 1:8]
erpdta = ERP.test[, -(1:8)] 
with(covariates, table(Electrode, Emotion, Condition))

# factorizing factors
covariates = covariates %>% mutate(Subj = as.factor(Subj), 
                               Condition = as.factor(Condition),
                               Emotion = as.factor(Emotion),
                               Electrode = as.factor(Electrode))
covariates = droplevels(covariates)

channels <- levels(covariates$Electrode)  
groups <- levels(covariates$Condition)      
colors <- ifelse(covariates$Emotion == "Happy", "orange", "slateblue") 

timeLine <- c(-100 , 100)

par(mfrow = c(3, 2)) 
for (i in 1:3) {
  for (j in 1:2) {
    select <- (covariates$Electrode == channels[i]) & 
      (covariates$Condition == groups[j]) 
    erpplot(erpdta[select, ], frames = time_pt, col = colors[select], 
            ylim = timeLine,
            lty = 1, lwd = 2, cex.lab = 1.25,
            xlab = "Time (ms)", ylab = "ERP (mV)",  
            main = paste("Electrode: ", channels[i], " - Condition: ", groups[j], 
                         sep = ""))
  }
}
legend("topright", bty = "n", lwd = 2, col = c("orange", "slateblue"),  
       legend = c("Happy", "Neutral")) 

par(mfrow = c(1, 1))

design <- model.matrix(~ C(Subj, sum) + Condition + Emotion + 
                         Electrode + Electrode:Emotion + Electrode:Condition +
                         Emotion:Condition + Electrode:Emotion:Condition, 
                       data = covariates)
colnames(design)[c(1, 110:120)]

effect = 117 # mark interesting difference from the design matrix

erpplot(erpdta, design, effect = effect, interval = "simultaneous", 
        nbs = 20, lwd = 2, frames = time_pt,
        xlab = "Time (ms)", ylab = "Condition effect")
title("Happy-Neutral difference curve \n Group Verbal, Channel PO8")

# To examine all difference curves over channels and groups, the above R statements 
# can be repeatedly applied after changing only the reference levels of the factors
# ‘Channel’ and ‘Group’:

par(mfrow = c(3, 2)) 
for (i in 1:3) {
  for (j in 1:2) {
    covariates$Electrode <- relevel(covariates$Electrode, ref = channels[i])
    covariates$Condition <- relevel(covariates$Condition, ref = groups[j])
    design <- model.matrix(~ C(Subj, sum)/Condition + Condition + Emotion + 
                             Electrode + Electrode:Emotion + Electrode:Condition +
                             Emotion:Condition + Electrode:Emotion:Condition, 
                           data = covariates)
    erpplot(erpdta, design, effect = effect, interval = "simultaneous", 
            nbs = 20, lwd = 2, frames = time_pt,
            xlab = "Time (ms)", ylab = "Condition effect")
    title(paste("Group ", groups[j], " Channel ", channels[i], sep = ""))
  }
}

par(mfrow = c(1, 1))

# Step 6 Signal detection: significance of effect curves ------
## 6.1 Functional Analysis of Variance of ERP curves ------

design0 <- model.matrix( ~ C(Subj, sum) + Condition + Emotion + 
                           Electrode + Electrode:Emotion + Electrode:Condition + 
                           Emotion:Condition, data = covariates)

# Variance Inflation Curve - Diagnostic plot to determine the number of factors 
# in correlation-adjusted testing procedures.
F <- erpFtest(erpdta, design, design0, pvalue = "none", nbf = NULL,
              wantplot = TRUE)

F$nbf

erpFtest(erpdta, design, design0, nbf = F$nbf)$pval 

# No significant third-order interaction effect is found. In other words, we can
# consider the spatial distribution of the condition effect to be the same for 
# the two impulsivity groups.

## 6.2 Selecting significant effects in an ANOVA design -----

# Each of the three second-order interaction effects can likewise 
# be tested, starting from the design matrix in which all the 
# interaction effects are present:

design <- model.matrix(~ C(Subj, sum) + Condition + Emotion + 
                         Electrode + Electrode:Emotion + Electrode:Condition +
                         Emotion:Condition, data = covariates)

# and deriving the design matrix of the null model by sequentially
# excluding one of the three interaction effects:

design0 <- model.matrix(~ C(Subj, sum) + Condition + Emotion + 
                          Electrode + Electrode:Condition + Emotion:Condition, 
                        data = covariates)
erpFtest(erpdta, design, design0, nbf = F$nbf)$pval

# The ‘Condition x Channel’ interaction effect is tested first, 
# followed by the ‘Group x Channel’ interaction effect, and then 
# the ‘Condition x Group’ interaction effect.

design0 <- model.matrix(~ C(Subj, sum) + Condition + Emotion + 
                          Electrode + Electrode:Emotion + Emotion:Condition, 
                        data = covariates)
erpFtest(erpdta, design, design0, nbf = F$nbf)$pval  
# [1] 0.982

design0 <- model.matrix(~ C(Subj, sum) + Condition + Emotion + 
                          Electrode + Electrode:Emotion + Electrode:Condition, 
                        data = covariates)
erpFtest(erpdta, design, design0, nbf = F$nbf)$pval 
# [1] 0.005268016

# Note that we have kept the same number of factors used previously
# for testing the third-order interaction effect. Since the residual 
# error of the model changes when the third-order interaction effect 
# is removed, the number of factors should have been updated by setting 
# the ‘nbf’ argument to ‘NULL’ in ‘erpFtest’. This also implies using 
# different numbers of factors in each of the three tests above. In 
# the present case, we have re-calculated the number of factors case 
# by case and have gotten the same conclusions reported above.

# From the results of the above functional F-tests only the ‘Condition 
# x Group’ interaction effect is found to be significant, which means 
# that, at each channel, the condition effect differs in the two 
# impulsivity groups. This is consistent with the condition effect 
# curves in panels of Figure 3, in which the peak around 300 ms 
# appears to be much more salient in the ‘Low’ impulsivity group.

# Likewise, the difference between ERP curves at the three channels can 
# be tested starting from the design matrix in which all the main effects
# and the ‘Condition x Group’ interaction effect are present:

design <- model.matrix(~ C(Subj, sum) + Condition + Emotion + 
                         Electrode + Emotion:Condition, data = covariates)

# and deriving the design matrix of the null model by removing the ‘Channel’ effect:

design0 <- model.matrix(~ C(Subj, sum) + Condition + Emotion +
                          Emotion:Condition, data = covariates)
erpFtest(erpdta, design, design0, nbf = F$nbf)$pval  

# [1] 0.0103747

# Likewise, the difference between ERP curves at the three channels
# can be tested starting from the design matrix in which all the 
# main effects and the ‘Condition x Group’ interaction effect are 
# present:

design <- model.matrix(~ C(Subj, sum) + Condition + Emotion + 
                         Electrode + Emotion:Condition, data = covariates)

# and deriving the design matrix of the null model by removing the ‘Channel’ effect:

design0 <- model.matrix(~ C(Subj, sum) + Condition + Emotion +
                          Emotion:Condition, data = covariates)
erpFtest(erpdta, design, design0, nbf = F$nbf)$pval  

# [1] 0.003092301

# Based on the p-value, the mean ERP curves are found to be significantly 
# different in the three channels.

## 6.3 Fitted effect curves with the optimal model -----

# The model corresponding to the last version of the design matrix,
# in which all the non-significant effects have been excluded, is 
# used to explore more thoroughly the condition effect and to 
# identify significant intervals. The effect curves, based on the 
# model resulting from the functional ANOVA selection of the 
# significant effects, is generated as follows:

par(mfrow = c(2,1)) 
for (i in 1:2) {
  covariates$Condition <- relevel(covariates$Condition, ref = groups[i])
  design <- model.matrix(~ C(Subj, sum) + Condition + Emotion + 
                           Electrode + Emotion:Condition, data = covariates)
  erpplot(erpdta, design, effect = effect, interval = "simultaneous", 
          lwd = 2, frames = time_pt, nbs = 20,
          xlab = "Time (ms)", ylab = "Emotion effect")
  title(paste("Happy-Neutral difference curve \n Condition ",
              groups[i], sep = ""))
}

# Success-Failure difference curve by group
par(mfrow = c(1, 1)) 

# 7 Signal identification: significant time intervals ----

# The analysis of ERP curves using functional ANOVA in the previous 
# section reveals a significantly different condition effect in the
# two impulsivity groups. One question remains: in which time 
# intervals is the condition effect curve in a given group at a 
# scalp location significant?

# Here, we address this signal identification issue by examining 
# ERPs at channel CPZ for subjects in high impulsivity group. First,
# we extract the corresponding ERP curves and covariates:

erp.o2e <- subset(ERP.test, 
                           (ERP.test$Electrode == "O2") &
                             (ERP.test$Condition == "Ekman"))
erp.o2e <- droplevels(erp.o2e)
covariates.o2e <- erp.o2e[, 1:8] 
erpdta.o2e <- erp.o2e[, -(1:8)]  

# Comparing the ERP curves in the two response inhibition conditions 
# at each time point is accomplished by pointwise t-tests in which the 
# null model is obtained by setting the ‘Condition’ effect parameter 
# to zero:

covariates.o2e = covariates.o2e %>% mutate(Subj = as.factor(Subj), 
                                   Condition = as.factor(Condition),
                                   Emotion = as.factor(Emotion),
                                   Electrode = as.factor(Electrode))
covariates.o2e = droplevels(covariates.o2e)


design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.o2e)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.o2e)

## 7.1 Averaging ERP curves in preselected time intervals ----

# The function ‘erpavetest’ provides a basic method to address the 
# simultaneous testing issue by partitioning the whole time frame into 
# a preselected number of intervals with same length and averaging the 
# ERPs of each single curve over the time points in each bin of the 
# partition. The number of simultaneous tests is reduced from T to the 
# number of bins in the partition. The default option in ‘erpavetest’ 
# introduces a partition of the whole time frame in ten intervals with 
# equal length:

avetest <- erpavetest(erpdta.o2e, design, design0, method = "BY")

erpplot(erpdta.o2e, design, effect = ncol(design), lwd = 2, 
        interval = "pointwise", frames = time_pt, nbs = 20,
        xlab = "Time (ms)", ylab = "Condition effect")
title("Happy-Neutral difference curve \n Ekman Condition, Channel O2")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")


## 7.2 Control of the False Discovery Rate ---------------------------------

bhtest <- erptest(erpdta.o2e, design, design0)

# The output ‘bhtest’ is used to plot the ‘Condition’ effect curve:
  
erpplot(erpdta.o2e, design, effect = ncol(design), lwd = 2, 
          interval = "simultaneous", frames = time_pt, ylim = c(-6, 6),
        nbs = 20,
          xlab = "Time (ms)", ylab = "Condition effect")
title("Happy-Neutral difference curve \n Ekman Condition, Channel O2")
points(time_pt[bhtest$significant], rep(0, length(bhtest$significant)),
       pch = 20, col = "goldenrod")


## 7.3 The Guthrie-Buchwald procedure --------------------------------------

gb <- gbtest(erpdta.o2e, design, design0)

# The output ‘gb’ is used to plot the ‘Condition’ effect curve:
  
erpplot(erpdta.o2e, design, effect = ncol(design), lwd = 2,
          interval = "simultaneous", frames = time_pt,  ylim = c(-6, 6),
        nbs = 20,
          xlab = "Time (ms)", ylab = "Condition effect")
title("Happy-Neutral difference curve \n Ekman Condition, Channel O2")
points(time_pt[gb$significant], rep(0, length(gb$significant)),
       pch = 20, col = "goldenrod")


## 7.4 The Adapative Factor-Adjustment method ------------------------------


fabh <- erpfatest(erpdta.o2e, design, nbf = NULL, 
                  wantplot = TRUE)
nbf <- fabh$nbf

fabh <- erpfatest(erpdta.o2e, design, nbf = nbf)

erpplot(erpdta.o2e, design, effect = ncol(design), lwd = 2, 
        interval = "pointwise", frames = time_pt, nbs = 20,
        xlab = "Time (ms)", ylab = "Condition effect curve")
title("Happy-Neutral difference curve \n Visual Condition, Channel O2")
points(time_pt[fabh$significant], rep(0, length(fabh$significant)),
       pch = 20, col = "goldenrod")

# Happy - Neutral Verbal --------------------------------------------------

erp.o2v <- subset(ERP.test, 
                  (ERP.test$Electrode == "O2") &
                    (ERP.test$Condition == "Under"))
erp.o2v <- droplevels(erp.o2v)
covariates.o2v <- erp.o2v[, 1:8] 
erpdta.o2v <- erp.o2v[, -(1:8)]  

# Comparing the ERP curves in the two response inhibition conditions 
# at each time point is accomplished by pointwise t-tests in which the 
# null model is obtained by setting the ‘Condition’ effect parameter 
# to zero:

covariates.o2v = covariates.o2v %>% mutate(Subj = as.factor(Subj), 
                                           Condition = as.factor(Condition),
                                           Emotion = as.factor(Emotion),
                                           Electrode = as.factor(Electrode))
covariates.o2v = droplevels(covariates.o2v)


design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.o2v)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.o2v)

## 7.1 Averaging ERP curves in preselected time intervals ----

# The function ‘erpavetest’ provides a basic method to address the 
# simultaneous testing issue by partitioning the whole time frame into 
# a preselected number of intervals with same length and averaging the 
# ERPs of each single curve over the time points in each bin of the 
# partition. The number of simultaneous tests is reduced from T to the 
# number of bins in the partition. The default option in ‘erpavetest’ 
# introduces a partition of the whole time frame in ten intervals with 
# equal length:

avetest <- erpavetest(erpdta.o2v, design, design0)

erpplot(erpdta.o2v, design, effect = ncol(design), lwd = 2,
        axis(side = 1, at = seq(0:600, by = 50)),
        interval = "pointwise", frames = time_pt, nbs = 20,
        xlab = "Time (ms)", ylab = "Condition effect")
title("Happy-Neutral difference curve \n Verbal Condition, Channel O2")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")


fabh <- erpfatest(erpdta.o2v, design, nbf = NULL, 
                  wantplot = F)
nbf <- fabh$nbf

fabh <- erpfatest(erpdta.o2v, design, nbf = nbf)

erpplot(erpdta.o2v, design, effect = ncol(design), lwd = 2, 
        interval = "pointwise", frames = time_pt, nbs = 20,
        xlab = "Time (ms)", ylab = "Condition effect curve")
title("Happy-Neutral difference curve \n Verbal Condition, Channel O2")
points(time_pt[fabh$significant], rep(0, length(fabh$significant)),
       pch = 20, col = "goldenrod")



par(mfrow = c(2, 1)) 
p1.o2e

