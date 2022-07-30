# Mixed linear model output
# 18.07.2022
# Liina Juuse

# Libraries and data ------------------------------------------------------
library(tidyverse)
library(lmerTest)
library(effectsize)

load("C:/Users/liina/OneDrive/PhD/EMO2021/EMO2021/ERP-package/output/f-long-table.RData")

f = na.omit(e)
rm(e)

#  mixed linear model with participant as random effect
fit3 <- lmer(Amplitude ~ Emotion + (1|Subj), data = f)
summary(fit3)
anova(fit3)
eta_squared(fit3)
cohens_f(fit3)
parameters::model_parameters(fit3)
parameters::model_parameters(anova(m))

# simpler model
m <- lm(Amplitude ~ Emotion, data = f)
parameters::model_parameters(m)
parameters::model_parameters(anova(m))
eta_squared(m, partial = FALSE)
eta_squared(m)
cohens_f(m)