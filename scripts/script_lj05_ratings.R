library(tidyverse)
library(scales)

setwd('C:/Users/liina/OneDrive/PhD/EMO2021/EMO2021/ERP-package/output')

# rescaling Kessler (2008) JACFEE facial emotion intensity ratings from 1-100 to 1-9
jacfee_m = c(1, NA, 76.7, 60.6, 78.15, 63.8, 78.2, 79.7, 100)
jacfee_m = rescale(jacfee_m, to = c(1,9))
nam = c('min', 'neutral', 'happiness', 'sadness', 'fear', 'disgust', 'surprise', 'anger', 'max')
jacfee_sd = c(1, NA, (12.3+11.4)/2, (15.0+14.8)/2, (14.2+13.4)/2, (10.9+11.6)/2, (15.3+15.5)/2, (13.2+13.7)/2, 100)
jacfee_sd = rescale(jacfee_sd, to = c(1,9))

jacfee = as.data.frame(cbind(jacfee_m, jacfee_sd, nam))

jacfee = jacfee %>% mutate(jacfee_m = as.numeric(jacfee_m),
                     jacfee_sd = as.numeric(jacfee_sd))

# vec = arrange(vec, vec) # arrange by lowest value

# facial expression arousal ratings from Karolinska institute facial stimuli (1-9)
karolinska_m = c(1, 3.72, 3.37, 3.58, 3.71, 3.51, 2.67, 3.58,  9)
nam = c('min', 'happiness', 'sadness', 'fear', 'disgust', 'surprise', 'neutral', 'anger', 'max')
karolinska_sd = c(1, 0.44, 0.44, 0.39, 0.47, 0.58, 0.32,  0.53, 9)
karolinska = as.data.frame(cbind(karolinska_m, karolinska_sd, nam))
karolinska = karolinska %>% mutate(karolinska_m = as.numeric(karolinska_m),
                           karolinska_sd = as.numeric(karolinska_sd))

# adding English lemmas ratings from Warriner et al (2013) (1-9)
lemmas_m = c(1, 2.81, 3.45, 5, 5.93, 6.14, 6.5, 6.57, 9)
nam = c('min', 'sadness', 'neutral', 'disgust', 'anger', 'fear', 'happiness', 'surprise', 'max')
lemmas_sd = c(1, 2.25, 2.39, 2.35, 2.77, 2.76, 2.63, 2.75, 9)
lemmas = as.data.frame(cbind(lemmas_m, lemmas_sd, nam))
lemmas = lemmas %>% mutate(lemmas_m = as.numeric(lemmas_m),
                     lemmas_sd = as.numeric(lemmas_sd))

ratings = merge(jacfee, lemmas, by = 'nam')
ratings = merge(ratings, karolinska, by = 'nam')

ratings = arrange(ratings, jacfee_m)
rm(list=setdiff(ls(), "ratings"))

write.csv(ratings, 'jacfee-lemmas-ratings.csv')
