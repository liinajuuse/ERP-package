rescale(1:100)
rescale(runif(50))
rescale(1)

rescale(76.7899, to = c(1,9)) #5
rescale(78.2999)


set.seed(9734798)                                       # Create example data
vec <- runif(100, - 5, 10)
head(vec)                
vec_range3 <- rescale(vec)                              # Scale to 0/1
head(vec_range3)           

vec_range4 <- rescale(vec, to = c(0, 5))                # Scale to 0/5
head(vec_range4)

vec = c(1, 76.7, 60.6, 78.15, 63.8, 78.2, 79.7, 100)
vec = rescale(vec, to = c(1,9))
nam = c('min', 'happiness', 'sadness', 'fear', 'disgust', 'surprise', 'anger', 'max')
sd = c(1, (12.3+11.4)/2, (15.0+14.8)/2, (14.2+13.4)/2, (10.9+11.6)/2, (15.3+15.5)/2, (13.2+13.7)/2, 100)
sd = rescale(sd, to = c(1,9))

vec = as.data.frame(cbind(vec, sd, nam))

vec = vec %>% mutate(vec = as.numeric(vec),
                     sd = as.numeric(sd))

arrange(vec, vec)

# w