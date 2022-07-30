# Topographic plots for EMO2018 visual and verbal conditions
# Liina Juuse
# 17.06.2022

# Libraries and data ------------------------------------------------------

library(tidyverse)
library(akima)
library(scales)
library(mgcv)
library(gridExtra)
library(png)
library(grid)
library(readr)
library(patchwork)

setwd("C:/Users/liina/OneDrive/PhD/EMO2021/EMO2021/ERP-package/output")

input_topo <- read_csv("input_topo.csv")

## Drawing the grid ------------------------------------------------------
electrodeLocs <- read_delim("https://raw.githubusercontent.com/craddm/ExploringERPs/master/biosemi70elecs.loc",
                            "\t",
                            escape_double = FALSE,
                            col_names = c("chanNo","theta","radius","electrode"),
                            trim_ws = TRUE)

electrodeLocs$radianTheta <- pi/180*electrodeLocs$theta

electrodeLocs <- electrodeLocs %>%
  mutate(x = .$radius*sin(.$radianTheta),
         y = .$radius*cos(.$radianTheta))

theme_topo <- function(base_size = 12)
{
  theme_bw(base_size = base_size) %+replace%
    theme(
      rect = element_blank(),
      line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100) {
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

headShape <- circleFun(c(0, 0), round(max(electrodeLocs$x)), npoints = 100) # 0
nose <- data.frame(x = c(-0.075,0,.075),y=c(.495,.575,.495))

ggplot(headShape,aes(x,y))+
  geom_path()+
  geom_text(data = electrodeLocs,
            aes(x, y, label = electrode))+
  geom_line(data = nose,
            aes(x, y, z = NULL))+
  theme_topo()+
  coord_equal()

gridRes <- 240 # Specify the number of points for each grid dimension i.e. the resolution/smoothness of the interpolation

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

maskRing <- circleFun(diameter = 1.42) #create a circle round the outside of the plotting area to mask the jagged edges of the interpolation

# Akima plot Ekman Anger 100 --------------------------------------------------------------

input_topo = dplyr::rename(input_topo, Fp1 = FP1, Fp2 = FP2, Fpz = FPz)
t = input_topo %>% filter(Condition == 'Ekman') %>% filter(Emotion == 'Anger')
t = t[,4:68]
t = t %>% gather(electrode, amplitude, -Interval)
t$amplitude = as.double(t$amplitude)

allData <- t %>% left_join(electrodeLocs, by = "electrode")
allData

#select a Timepoint
singleTimepoint <- filter(allData, Interval == '100-200')


tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 0.4)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal() +
  ggtitle(label = "VIS: 100-200 ms")

ek.an.100 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                 guide = "colourbar",
                                 oob = squish)



# Akima plot Ekman Anger 300 ----------------------------------------------


singleTimepoint <- filter(allData, Interval == '300-400')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 0.4)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal() +
  ggtitle(label = "VIS: 300-400 ms")

ek.an.300 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                             guide = "colourbar",
                                             oob = squish)

# Akima plot Ekman Anger 500 ----------------------------------------------


singleTimepoint <- filter(allData, Interval == '500-600')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 0.4)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal() +
  ggtitle(label = "VIS: 500-600 ms")

ek.an.500 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                             guide = "colourbar",
                                             oob = squish)

#ek.an.100 + ek.an.300 + ek.an.500

ggarrange(ek.an.100, ek.an.300, ek.an.500, ncol = 3, common.legend = T)
ggarrange(ek.an.100, ek.an.300, ek.an.500, un.an.100, un.an.300, un.an.500, ncol = 3, nrow = 2, common.legend = T)
# Akima plot Ekman Happy 100 --------------------------------------------------------------

t = input_topo %>% filter(Condition == 'Ekman') %>% filter(Emotion == 'Happy')
t = t[,4:68]
t = t %>% gather(electrode, amplitude, -Interval)
t$amplitude = as.double(t$amplitude)

allData <- t %>% left_join(electrodeLocs, by = "electrode")
allData

singleTimepoint <- filter(allData, Interval == '100-200')


tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

ek.hap.100 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                             guide = "colourbar",
                                             oob = squish)

# Akima plot Ekman Happy 300 ----------------------------------------------

singleTimepoint <- filter(allData, Interval == '300-400')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

ek.hap.300 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                             guide = "colourbar",
                                             oob = squish)

# Akima plot Ekman Happy 500 ----------------------------------------------


singleTimepoint <- filter(allData, Interval == '500-600')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

ek.hap.500 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                             guide = "colourbar",
                                             oob = squish)

# Akima plot Ekman Fear 100 --------------------------------------------------------------

t = input_topo %>% filter(Condition == 'Ekman') %>% filter(Emotion == 'Fear')
t = t[,4:68]
t = t %>% gather(electrode, amplitude, -Interval)
t$amplitude = as.double(t$amplitude)

allData <- t %>% left_join(electrodeLocs, by = "electrode")
allData

singleTimepoint <- filter(allData, Interval == '100-200')


tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

ek.fea.100 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Ekman Fear 300 ----------------------------------------------

singleTimepoint <- filter(allData, Interval == '300-400')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

ek.fea.300 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Ekman Fear 500 ----------------------------------------------


singleTimepoint <- filter(allData, Interval == '500-600')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

ek.fea.500 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Ekman Disgust 100 --------------------------------------------------------------

t = input_topo %>% filter(Condition == 'Ekman') %>% filter(Emotion == 'Disgust')
t = t[,4:68]
t = t %>% gather(electrode, amplitude, -Interval)
t$amplitude = as.double(t$amplitude)

allData <- t %>% left_join(electrodeLocs, by = "electrode")
allData

singleTimepoint <- filter(allData, Interval == '100-200')


tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

ek.dis.100 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Ekman Disgust 300 ----------------------------------------------

singleTimepoint <- filter(allData, Interval == '300-400')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

ek.dis.300 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Ekman Disgust 500 ----------------------------------------------


singleTimepoint <- filter(allData, Interval == '500-600')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

ek.dis.500 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)
#ek.dis.100 + ek.dis.300 + ek.dis.500

# Akima plot Ekman Sad 100 --------------------------------------------------------------

t = input_topo %>% filter(Condition == 'Ekman') %>% filter(Emotion == 'Sad')
t = t[,4:68]
t = t %>% gather(electrode, amplitude, -Interval)
t$amplitude = as.double(t$amplitude)

allData <- t %>% left_join(electrodeLocs, by = "electrode")
allData

singleTimepoint <- filter(allData, Interval == '100-200')


tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.5) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

ek.sad.100 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Ekman Sad 300 ----------------------------------------------

singleTimepoint <- filter(allData, Interval == '300-400')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

ek.sad.300 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Ekman Sad 500 ----------------------------------------------


singleTimepoint <- filter(allData, Interval == '500-600')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

ek.sad.500 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

ek.sad.100 + ek.sad.300 + ek.sad.500

# Akima plot Ekman Surprise 100 --------------------------------------------------------------

t = input_topo %>% filter(Condition == 'Ekman') %>% filter(Emotion == 'Surprise')
t = t[,4:68]
t = t %>% gather(electrode, amplitude, -Interval)
t$amplitude = as.double(t$amplitude)

allData <- t %>% left_join(electrodeLocs, by = "electrode")
allData

singleTimepoint <- filter(allData, Interval == '100-200')


tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

ek.sur.100 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Ekman Surprise 300 ----------------------------------------------

singleTimepoint <- filter(allData, Interval == '300-400')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

ek.sur.300 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Ekman Surprise 500 ----------------------------------------------


singleTimepoint <- filter(allData, Interval == '500-600')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

ek.sur.500 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

ek.sur.100 + ek.sur.300 + ek.sur.500

# Akima plot Under Anger 100 --------------------------------------------------------------

input_topo = dplyr::rename(input_topo, Fp1 = FP1, Fp2 = FP2, Fpz = FPz)
t = input_topo %>% filter(Condition == 'Under') %>% filter(Emotion == 'Anger')
t = t[,4:68]
t = t %>% gather(electrode, amplitude, -Interval)
t$amplitude = as.double(t$amplitude)

allData <- t %>% left_join(electrodeLocs, by = "electrode")
allData

#select a Timepoint
singleTimepoint <- filter(allData, Interval == '100-200')


tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 0.4)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

un.an.100 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                             guide = "colourbar",
                                             oob = squish)



# Akima plot Under Anger 300 ----------------------------------------------


singleTimepoint <- filter(allData, Interval == '300-400')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 0.4)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

un.an.300 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                             guide = "colourbar",
                                             oob = squish)

# Akima plot Under Anger 500 ----------------------------------------------


singleTimepoint <- filter(allData, Interval == '500-600')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 0.4)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

un.an.500 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                             guide = "colourbar",
                                             oob = squish)

un.an.100 + un.an.300 + un.an.500

# Akima plot Under Happy 100 --------------------------------------------------------------

t = input_topo %>% filter(Condition == 'Under') %>% filter(Emotion == 'Happy')
t = t[,4:68]
t = t %>% gather(electrode, amplitude, -Interval)
t$amplitude = as.double(t$amplitude)

allData <- t %>% left_join(electrodeLocs, by = "electrode")
allData

singleTimepoint <- filter(allData, Interval == '100-200')


tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

un.hap.100 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Under Happy 300 ----------------------------------------------

singleTimepoint <- filter(allData, Interval == '300-400')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

un.hap.300 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Under Happy 500 ----------------------------------------------


singleTimepoint <- filter(allData, Interval == '500-600')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

un.hap.500 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Under Fear 100 --------------------------------------------------------------

t = input_topo %>% filter(Condition == 'Under') %>% filter(Emotion == 'Fear')
t = t[,4:68]
t = t %>% gather(electrode, amplitude, -Interval)
t$amplitude = as.double(t$amplitude)

allData <- t %>% left_join(electrodeLocs, by = "electrode")
allData

singleTimepoint <- filter(allData, Interval == '100-200')


tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

un.fea.100 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Under Fear 300 ----------------------------------------------

singleTimepoint <- filter(allData, Interval == '300-400')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

un.fea.300 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Under Fear 500 ----------------------------------------------


singleTimepoint <- filter(allData, Interval == '500-600')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

un.fea.500 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Under Disgust 100 --------------------------------------------------------------

t = input_topo %>% filter(Condition == 'Under') %>% filter(Emotion == 'Disgust')
t = t[,4:68]
t = t %>% gather(electrode, amplitude, -Interval)
t$amplitude = as.double(t$amplitude)

allData <- t %>% left_join(electrodeLocs, by = "electrode")
allData

singleTimepoint <- filter(allData, Interval == '100-200')


tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

un.dis.100 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Under Disgust 300 ----------------------------------------------

singleTimepoint <- filter(allData, Interval == '300-400')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

un.dis.300 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Under Disgust 500 ----------------------------------------------


singleTimepoint <- filter(allData, Interval == '500-600')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

un.dis.500 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)
#un.dis.100 + un.dis.300 + un.dis.500

# Akima plot Under Sad 100 --------------------------------------------------------------

t = input_topo %>% filter(Condition == 'Under') %>% filter(Emotion == 'Sad')
t = t[,4:68]
t = t %>% gather(electrode, amplitude, -Interval)
t$amplitude = as.double(t$amplitude)

allData <- t %>% left_join(electrodeLocs, by = "electrode")
allData

singleTimepoint <- filter(allData, Interval == '100-200')


tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.5) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

un.sad.100 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Under Sad 300 ----------------------------------------------

singleTimepoint <- filter(allData, Interval == '300-400')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

un.sad.300 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Under Sad 500 ----------------------------------------------


singleTimepoint <- filter(allData, Interval == '500-600')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

un.sad.500 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

un.sad.100 + un.sad.300 + un.sad.500

# Akima plot Under Surprise 100 --------------------------------------------------------------

t = input_topo %>% filter(Condition == 'Under') %>% filter(Emotion == 'Surprise')
t = t[,4:68]
t = t %>% gather(electrode, amplitude, -Interval)
t$amplitude = as.double(t$amplitude)

allData <- t %>% left_join(electrodeLocs, by = "electrode")
allData

singleTimepoint <- filter(allData, Interval == '100-200')


tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

un.sur.100 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Under Surprise 300 ----------------------------------------------

singleTimepoint <- filter(allData, Interval == '300-400')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

un.sur.300 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

# Akima plot Under Surprise 500 ----------------------------------------------


singleTimepoint <- filter(allData, Interval == '500-600')

tmpTopo <- with(singleTimepoint,
                interp(x = x, y = y, z = amplitude,
                       xo = seq(min(x)*2,
                                max(x)*2,
                                length = gridRes),
                       yo = seq(min(y)*2,
                                max(y)*2,
                                length = gridRes),
                       linear = FALSE,
                       extrap = TRUE)
) 

interpTopo <- data.frame(x = tmpTopo$x, tmpTopo$z)

names(interpTopo)[1:length(tmpTopo$y)+1] <- tmpTopo$y

interpTopo <- gather(interpTopo,
                     key = y,
                     value = amplitude,
                     -x,
                     convert = TRUE)

interpTopo$incircle <- sqrt(interpTopo$x^2 + interpTopo$y^2) < .7 # mark grid elements that are outside of the plotting circle

interpTopo <- interpTopo[interpTopo$incircle,] #remove the elements outside the circle

akimaPlot <- ggplot(interpTopo,
                    aes(x = x, y = y, fill = amplitude)
) +
  geom_raster() +
  stat_contour(aes(z = amplitude),
               colour = "grey40",
               size = 0.2,
               binwidth = 0.5,
               alpha = 0.35) +
  theme_topo()+
  scale_fill_gradientn(colours = jet.colors(10),
                       limits = c(-2,2),
                       guide = "colourbar",
                       oob = squish) + 
  geom_path(data = maskRing,
            aes(x, y, z = NULL, fill =NULL),
            colour = "white",
            size = 6)+
  geom_point(data = singleTimepoint,
             aes(x, y),
             size = 1)+
  geom_path(data = headShape,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  geom_path(data = nose,
            aes(x, y, z = NULL, fill = NULL),
            size = 0.65)+
  coord_equal()

un.sur.500 = akimaPlot + scale_fill_distiller(type = "div",palette = "PuOr",limits = c(-2,2),
                                              guide = "colourbar",
                                              oob = squish)

un.sur.100 + un.sur.300 + un.sur.500