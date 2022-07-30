par(mfrow = c(3, 2)) 
par(mfrow = c(1, 1)) 


# I graphs ----------------------------------------------------------------

## happy-neutral ROI:visual --------------
design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.o2e)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.o2e)

avetest <- erpavetest(erpdta.o2e, design, design0, method = "bonferroni"#, nintervals = 20
                      )

erpplot(erpdta.o2e, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 30, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Happy-Neutral difference curve \n Visual condition, visual ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.o2v)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.o2v)

avetest <- erpavetest(erpdta.o2v, design, design0, method = "bonferroni"# nintervals = 20
                      )

erpplot(erpdta.o2v, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 30, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Happy-Neutral difference curve \n Verbal condition, visual ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

## angry-neutral ROI:visual -------------
design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.abe)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.abe)

avetest <- erpavetest(erpdta.abe, design, design0, method = "bonferroni"#, nintervals = 20
                      )

erpplot(erpdta.abe, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 30, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Anger-Neutral difference curve \n Visual condition, visual ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.abv)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.abv)

avetest <- erpavetest(erpdta.abv, design, design0, method = "bonferroni"#, nintervals = 20
                      )

erpplot(erpdta.abv, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 30, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Anger-Neutral difference curve \n Verbal condition, visual ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

## fear-neutral ROI:visual ---------------------
design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.ee)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.ee)

avetest <- erpavetest(erpdta.ee, design, design0, method = "bonferroni"#, nintervals = 20
                      )

erpplot(erpdta.ee, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 30, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Fear-Neutral difference curve \n Visual condition, visual ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.ev)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.ev)

avetest <- erpavetest(erpdta.ev, design, design0, method = "bonferroni"#, nintervals = 20
                      )

erpplot(erpdta.ev, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 30, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Fear-Neutral difference curve \n Verbal condition, visual ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")


# II graph ----------------------------------------------------------------

## surprise-neutral ROI:visual --------------
design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.be)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.be)

avetest <- erpavetest(erpdta.be, design, design0, method = "bonferroni"#, nintervals = 20
                      )

erpplot(erpdta.be, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Surprise-Neutral difference curve \n Visual condition, visual ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.bv)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.bv)

avetest <- erpavetest(erpdta.bv, design, design0, method = "bonferroni"#, nintervals = 20
                      )

erpplot(erpdta.bv, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 20,ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Surprise-Neutral difference curve \n Verbal condition, visual ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

## disgust-neutral ROI:visual ---------------
design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.de)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.de)

avetest <- erpavetest(erpdta.de, design, design0, method = "bonferroni"#, nintervals = 20
                      )

erpplot(erpdta.de, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Disgust-Neutral difference curve \n Visual condition, visual ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.dv)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.dv)

avetest <- erpavetest(erpdta.dv, design, design0, method = "bonferroni"#, nintervals = 20
                      )

erpplot(erpdta.dv, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Disgust-Neutral difference curve \n Verbal condition, visual ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

## sad-neutral ROI:visual ---------
design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.ce)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.ce)

avetest <- erpavetest(erpdta.ce, design, design0, method = "bonferroni"#, nintervals = 20
                      )

erpplot(erpdta.ce, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Sad-Neutral difference curve \n Visual condition, visual ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.cv)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.cv)

avetest <- erpavetest(erpdta.cv, design, design0, method = "bonferroni"#, nintervals = 20
                      )

erpplot(erpdta.cv, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Sad-Neutral difference curve \n Verbal condition, visual ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")


# III graphs --------------------------------------------------------------
## happy-neutral ROI:verbal --------------
design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.fe)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.fe)

avetest <- erpavetest(erpdta.fe, design, design0, method = "bonferroni")

erpplot(erpdta.fe, design, effect = ncol(design), lwd = 2, 
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Happy-Neutral difference curve \n Visual condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.fv)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.fv)

avetest <- erpavetest(erpdta.fv, design, design0, method = "bonferroni")

erpplot(erpdta.fv, design, effect = ncol(design), lwd = 2, ylim = c(-2,2),
        interval = "pointwise", frames = time_pt, nbs = 20,
        xlab = "Time (ms)", ylab = "Condition effect")
title("Happy-Neutral difference curve \n Verbal condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

## angry-neutral ROI:verbal --------------
design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.ge)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.ge)

avetest <- erpavetest(erpdta.ge, design, design0, method = "bonferroni")

erpplot(erpdta.ge, design, effect = ncol(design), lwd = 2, ylim = c(-2,2),
        interval = "pointwise", frames = time_pt, nbs = 20,
        xlab = "Time (ms)", ylab = "Condition effect")
title("Anger-Neutral difference curve \n Visual condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.gv)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.gv)

avetest <- erpavetest(erpdta.gv, design, design0, method = "bonferroni")

erpplot(erpdta.gv, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Anger-Neutral difference curve \n Verbal condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

## fear-neutral ROI:verbal ---------------

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.he)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.he)

avetest <- erpavetest(erpdta.he, design, design0, method = "bonferroni")

erpplot(erpdta.he, design, effect = ncol(design), lwd = 2, 
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Fear-Neutral difference curve \n Visual condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.hv)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.hv)

avetest <- erpavetest(erpdta.hv, design, design0, method = "bonferroni")

erpplot(erpdta.hv, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Fear-Neutral difference curve \n Verbal condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

# IV graphs ---------------------------------------------------------------


## surprise-neutral ROI:verbal --------------

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.ie)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.ie)

avetest <- erpavetest(erpdta.ie, design, design0, method = "bonferroni")

erpplot(erpdta.ie, design, effect = ncol(design), lwd = 2, 
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Surprise-Neutral difference curve \n Visual condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.iv)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.iv)

avetest <- erpavetest(erpdta.iv, design, design0, method = "bonferroni")

erpplot(erpdta.iv, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Surprise-Neutral difference curve \n Verbal condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

## disgust-neutral ROI:verbal ---------------

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.je)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.je)

avetest <- erpavetest(erpdta.je, design, design0, method = "bonferroni")

erpplot(erpdta.je, design, effect = ncol(design), lwd = 2, 
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Disgust-Neutral difference curve \n Visual condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.jv)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.jv)

avetest <- erpavetest(erpdta.jv, design, design0, method = "bonferroni")

erpplot(erpdta.jv, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Disgust-Neutral difference curve \n Verbal condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

## sad-neutral ROI:verbal ---------
design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.ke)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.ke)

avetest <- erpavetest(erpdta.ke, design, design0, method = "bonferroni")

erpplot(erpdta.ke, design, effect = ncol(design), lwd = 2, 
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Sad-Neutral difference curve \n Visual condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")

design <- model.matrix(~ C(Subj, sum) + Emotion, data = covariates.kv)
design0 <- model.matrix(~ C(Subj, sum), data = covariates.kv)

avetest <- erpavetest(erpdta.kv, design, design0, method = "bonferroni")

erpplot(erpdta.kv, design, effect = ncol(design), lwd = 2,
        interval = "pointwise", frames = time_pt, nbs = 20, ylim = c(-2,2),
        xlab = "Time (ms)", ylab = "Condition effect")
title("Sad-Neutral difference curve \n Verbal condition, verbal ROI")
points(time_pt[avetest$significant], rep(0, length(avetest$significant)), 
       pch = 20, col = "goldenrod")
abline(v = time_pt[avetest$breaks], lty = 2, col = "darkgray")
