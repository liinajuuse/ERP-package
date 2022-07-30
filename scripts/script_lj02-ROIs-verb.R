# Liina Juuse, 14.06.2022

# Libraries and data ------------------------------------------------------
library(tidyverse)
library(ERP)

# Visual Happy - Neutral ----------------------------------------------------

## 7 Signal identification: significant time intervals
ERP.test <- subset(oz, (Emotion == "Happy" | Emotion == "Neutral" ) #& ROI == "Visual"
)
ERP.test = na.omit(ERP.test)
ERP.test = ERP.test[, c(1:9, 109:409)]
rownames(ERP.test) <- NULL # I like to reset the row names every time I apply a filter to delete rows in a data frame, just to keep it more organised

erp.fe <- subset(ERP.test, 
                  (ERP.test$ROI == "Verbal") &
                    (ERP.test$Condition == "Ekman"))
erp.fe <- droplevels(erp.fe)
covariates.fe <- erp.fe[, 1:9] 
erpdta.fe <- erp.fe[, -(1:9)]  

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.fe)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.fe)

## 7.1 Averaging ERP curves in preselected time intervals -

avetest <- erpavetest(erpdta.fe, design, design0, method = "bonferroni")

erpplot(erpdta.fe, design, effect = ncol(design), lwd = 2, 
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Happy-Neutral difference curve \n Visual condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

# Verbal Happy - Neutral --------------------------------------------------

erp.fv <- subset(ERP.test, (ERP.test$ROI == "Verbal") &
                   (ERP.test$Condition == "Under"))
erp.fv <- droplevels(erp.fv)
covariates.fv <- erp.fv[, 1:9] 
erpdta.fv <- erp.fv[, -(1:9)]  

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.fv)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.fv)

## 7.1 Averaging ERP curves in preselected time intervals ---

avetest <- erpavetest(erpdta.fv, design, design0, method = "bonferroni")

erpplot(erpdta.fv, design, effect = ncol(design), lwd = 2, ylim = c(-2,2),
        interval = "pointwise", frames = time_pt, nbs = 20,
        xlab = "Time (ms)", ylab = "Condition effect")
title("Happy-Neutral difference curve \n Verbal condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")


# Visual Anger - Neutral ---------------------------------------------------------
ERP.test <- subset(oz, (Emotion == "Anger" | Emotion == "Neutral" ) #& ROI == "Visual"
)
ERP.test = na.omit(ERP.test)
ERP.test = ERP.test[, c(1:9, 109:409)]
rownames(ERP.test) <- NULL # I like to reset the row names every time I apply a filter to delete rows in a data frame, just to keep it more organised

# 7 Signal identification: significant time intervals 

erp.ge <- subset(ERP.test, 
                (ERP.test$ROI == "Verbal") &
                  (ERP.test$Condition == "Ekman"))
erp.ge <- droplevels(erp.ge)
covariates.ge <- erp.ge[, 1:9] 
erpdta.ge <- erp.ge[, -(1:9)]  

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.ge)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.ge)

## 7.1 Averaging ERP curves in preselected time intervals --

avetest <- erpavetest(erpdta.ge, design, design0, method = "bonferroni")

erpplot(erpdta.ge, design, effect = ncol(design), lwd = 2, ylim = c(-2,2),
        interval = "pointwise", frames = time_pt, nbs = 20,
        xlab = "Time (ms)", ylab = "Condition effect")
title("Anger-Neutral difference curve \n Visual condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

# Verbal Anger - Neutral --------------------------------------------------

erp.gv <- subset(ERP.test, (ERP.test$ROI == "Verbal") & (ERP.test$Condition == "Under"))
erp.gv <- droplevels(erp.gv)
covariates.gv <- erp.gv[, 1:9] 
erpdta.gv <- erp.gv[, -(1:9)]  

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.gv)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.gv)

## 7.1 Averaging ERP curves in preselected time intervals --

avetest <- erpavetest(erpdta.gv, design, design0, method = "bonferroni")

erpplot(erpdta.gv, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Anger-Neutral difference curve \n Verbal condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

# Visual Fear - Neutral ---------------------------------------------------------

ERP.test <- subset(oz, (Emotion == "Fear" | Emotion == "Neutral") #& ROI == "Visual"
)
ERP.test = na.omit(ERP.test)
ERP.test = ERP.test[, c(1:9, 109:409)]
rownames(ERP.test) <- NULL # I like to reset the row names every time I apply a filter to delete rows in a data frame, just to keep it more organised
ERP.test = droplevels(ERP.test)
ERP.test$Emotion = factor(ERP.test$Emotion, c("Fear", "Neutral"))

# 7 Signal identification: significant time intervals ---

erp.he <- subset(ERP.test, 
                 (ERP.test$ROI == "Verbal") &
                   (ERP.test$Condition == "Ekman"))
erp.he <- droplevels(erp.he)
covariates.he <- erp.he[, 1:9] 
erpdta.he <- erp.he[, -(1:9)]  

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.he)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.he)

## 7.1 Averaging ERP curves in preselected time intervals ---

avetest <- erpavetest(erpdta.he, design, design0, method = "bonferroni")

erpplot(erpdta.he, design, effect = ncol(design), lwd = 2, 
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Fear-Neutral difference curve \n Visual condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

# Verbal Fear - Neutral --------------------------------------------------

erp.hv <- subset(ERP.test, (ERP.test$ROI == "Verbal") & 
                   (ERP.test$Condition == "Under"))
erp.hv <- droplevels(erp.hv)
covariates.hv <- erp.hv[, 1:9] 
erpdta.hv <- erp.hv[, -(1:9)]  

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.hv)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.hv)

## 7.1 Averaging ERP curves in preselected time intervals --

avetest <- erpavetest(erpdta.hv, design, design0, method = "bonferroni")

erpplot(erpdta.hv, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Fear-Neutral difference curve \n Verbal condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

# Visual Surprise - Neutral ---------------------------------------------------------
ERP.test <- subset(oz, (Emotion == "Surprise" | Emotion == "Neutral" ) #& ROI == "Visual"
)
ERP.test = na.omit(ERP.test)
ERP.test = ERP.test[, c(1:9, 109:409)]
rownames(ERP.test) <- NULL # I like to reset the row names every time I apply a filter to delete rows in a data frame, just to keep it more organised
ERP.test$Emotion = factor(ERP.test$Emotion, c("Surprise", "Neutral"))

erp.ie <- subset(ERP.test, 
                 (ERP.test$ROI == "Verbal") &
                   (ERP.test$Condition == "Ekman"))
erp.ie <- droplevels(erp.ie)
covariates.ie <- erp.ie[, 1:9] 
erpdta.ie <- erp.ie[, -(1:9)]  

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.ie)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.ie)

## 7.1 Averaging ERP curves in preselected time intervals 

avetest <- erpavetest(erpdta.ie, design, design0, method = "bonferroni")

erpplot(erpdta.ie, design, effect = ncol(design), lwd = 2, 
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Surprise-Neutral difference curve \n Visual condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

# Verbal Surprise - Neutral --------------------------------------------------

erp.iv <- subset(ERP.test, (ERP.test$ROI == "Verbal") & 
                   (ERP.test$Condition == "Under"))
erp.iv <- droplevels(erp.iv)
covariates.iv <- erp.iv[, 1:9] 
erpdta.iv <- erp.iv[, -(1:9)]  

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.iv)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.iv)

## 7.1 Averaging ERP curves in preselected time intervals 

avetest <- erpavetest(erpdta.iv, design, design0, method = "bonferroni")

erpplot(erpdta.iv, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Surprise-Neutral difference curve \n Verbal condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

# Visual Disgust - Neutral ---------------------------------------------------------

ERP.test <- subset(oz, (Emotion == "Disgust" | Emotion == "Neutral") #& ROI == "Visual"
)
ERP.test = na.omit(ERP.test)
ERP.test = ERP.test[, c(1:9, 109:409)]
rownames(ERP.test) <- NULL # I like to reset the row names every time I apply a filter to delete rows in a data frame, just to keep it more organised
ERP.test = droplevels(ERP.test)
ERP.test$Emotion = factor(ERP.test$Emotion, c("Disgust", "Neutral"))

erp.je <- subset(ERP.test, 
                 (ERP.test$ROI == "Verbal") &
                   (ERP.test$Condition == "Ekman"))
erp.je <- droplevels(erp.je)
covariates.je <- erp.je[, 1:9] 
erpdta.je <- erp.je[, -(1:9)]  

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.je)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.je)

## 7.1 Averaging ERP curves in preselected time intervals 

avetest <- erpavetest(erpdta.je, design, design0, method = "bonferroni")

erpplot(erpdta.je, design, effect = ncol(design), lwd = 2, 
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Disgust-Neutral difference curve \n Visual condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

# Verbal Disgust - Neutral --------------------------------------------------

erp.jv <- subset(ERP.test, (ERP.test$ROI == "Verbal") & 
                   (ERP.test$Condition == "Under"))
erp.jv <- droplevels(erp.jv)
covariates.jv <- erp.jv[, 1:9] 
erpdta.jv <- erp.jv[, -(1:9)]  

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.jv)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.jv)

avetest <- erpavetest(erpdta.jv, design, design0, method = "bonferroni")
#avetest2 = erpavetest(erpdta.jv, design, design0)
erpplot(erpdta.jv, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Disgust-Neutral difference curve \n Verbal condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

# Visual Sad - Neutral ---------------------------------------------------------

ERP.test <- subset(oz, (Emotion == "Sad" | Emotion == "Neutral") #& ROI == "Visual"
)
ERP.test = na.omit(ERP.test)
ERP.test = ERP.test[, c(1:9, 109:409)]
rownames(ERP.test) <- NULL # I like to reset the row names every time I apply a filter to delete rows in a data frame, just to keep it more organised
ERP.test = droplevels(ERP.test)
ERP.test$Emotion = factor(ERP.test$Emotion, c("Sad", "Neutral"))

erp.ke <- subset(ERP.test, 
                 (ERP.test$ROI == "Verbal") &
                   (ERP.test$Condition == "Ekman"))
erp.ke <- droplevels(erp.ke)
covariates.ke <- erp.ke[, 1:9] 
erpdta.ke <- erp.ke[, -(1:9)]  

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.ke)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.ke)

## 7.1 Averaging ERP curves in preselected time intervals 

avetest <- erpavetest(erpdta.ke, design, design0, method = "bonferroni")

erpplot(erpdta.ke, design, effect = ncol(design), lwd = 2, 
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Sad-Neutral difference curve \n Visual condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

# Verbal Sad - Neutral --------------------------------------------------

erp.kv <- subset(ERP.test, (ERP.test$ROI == "Verbal") & 
                   (ERP.test$Condition == "Under"))
erp.kv <- droplevels(erp.kv)
covariates.kv <- erp.kv[, 1:9] 
erpdta.kv <- erp.kv[, -(1:9)]  

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.kv)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.kv)

## 7.1 Averaging ERP curves in preselected time intervals

avetest <- erpavetest(erpdta.kv, design, design0, method = "bonferroni")

erpplot(erpdta.kv, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Sad-Neutral difference curve \n Verbal condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")
