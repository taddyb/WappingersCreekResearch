# Author: Tadd Bindas
# Description: To produce graphs which plot the Wappingers Data
# install.packages("readxl")
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages('ggsn')
# devtools::install_github("oswaldosantos/ggsn")
library(readxl)
library(lubridate)
library(ggplot2)
library(config)
library(ggmap)
library(tidyverse)
library(ggsn)
library(legendMap)

# dataPrep
# takes a file location and returns data split on site number
dataPrep <- function(fileLocation){
  data <- read_excel(fileLocation, skip=1)
  # remove units row
  data <- data[-1,]
  # View(data)
  splitData <- split(data, f=data$`Site Number`)
  return(splitData)
}

normalize <- function(data){
  data$EC <- sapply(data$EC, as.numeric)
  meanEC <- mean(data$EC)
  data$NEC <- scale(data$EC)
  data$Temp <- sapply(data$Temp, as.numeric)
  meanEC <- mean(data$Temp)
  data$NTemp <- scale(data$Temp)
  return(data)
}

splitData<- dataPrep("data/salinity-dataset.xlsx")
siteOne = normalize(splitData$`1`)
siteTwo = normalize(splitData$`2`)
siteThree = normalize(splitData$`3`)
siteFour = normalize(splitData$`4`)
siteFive = normalize(splitData$`5`)

siteThreeMerged = rbind(siteFive,siteThree,siteFour)

mergedData = rbind(siteOne,siteTwo,siteThree)
# plot(x=siteTwo$Time, y=siteTwo$NEC)
#   

# ---------Maps--------- #

# Map stuff:
states <- map_data("state")
counties <- map_data("county")
new_york<- subset(states, region %in% c("new york"))
ny_county <- subset(counties, region == "new york")
dutchess <- subset(ny_county, subregion == "dutchess")


ny_base <- ggplot(data = new_york, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")

# Use this to create this to show where dutchess county is
gg1 <- ny_base + theme_nothing() +
  geom_polygon(data = ny_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_polygon(color = "black", fill = "black", data= dutchess)


  # get the state border back on top
ggplot(data = new_york) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") +
  coord_fixed(1.3)


Sys.setenv(R_CONFIG_ACTIVE = "config")
config <- config::get(file = "config.yml")
config[1]

# Please cite ggmap if you use it: see citation("ggmap") for details.

# save api key
register_google(key = config[1])
# check if key is saved
has_goog_key()

onePoints = data.frame(lon=as.numeric(siteOne$lon), lat=as.numeric(siteOne$lat), EC=as.numeric(siteOne$NEC), temp=as.numeric(siteOne$NTemp))
twoPoints = data.frame(lon=as.numeric(siteTwo$lon), lat=as.numeric(siteTwo$lat), EC=as.numeric(siteTwo$NEC), temp=as.numeric(siteTwo$NTemp))
threePoints = data.frame(lon=as.numeric(siteThreeMerged$lon), lat=as.numeric(siteThreeMerged$lat), EC=as.numeric(siteThreeMerged$NEC), temp=as.numeric(siteThreeMerged$NTemp))

colnames(onePoints)[3] <- "μS"
colnames(twoPoints)[3] <- "μS"
colnames(threePoints)[3] <- "μS"

# USE WITH GGPLOT
map <-  get_map("41.75420 -73.78561", zoom = 12)

overlay <- get_map("41.75420 -73.78561", zoom = 12) %>% ggmap() +
  geom_point(data = onePoints, aes(x = lon, y = lat, color=μS), size = 2 ) +
  geom_point(data = twoPoints, aes(x = lon, y = lat, color=μS), size = 2 ) +
  geom_point(data = threePoints, aes(x = lon, y = lat, color=μS), size = 2 ) +
  scale_colour_gradientn(colors=c("light blue","blue", "black", "orange", "yellow")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

bb <- attr(map, "bb")
bb2 <- data.frame(long = unlist(bb[c(2, 4)]), lat = unlist(bb[c(1,3)]))

north2(overlay, symbol = 16) 
  # scalebar(x.min=-73.67, x.max =-73.89558, y.min=41.67177 , y.max=41.83569,
  #          dist = 1, dist_unit = "km", location="topleft",
  #          transform = TRUE, model = "WGS84")


siteOnePlot <- get_map("41.80420 -73.78561", zoom = 16) %>% ggmap() +
  geom_point(data = onePoints, aes(x = lon, y = lat, color=μS), size = 2 ) +
  scale_colour_gradientn(colors=c("light blue","blue", "black", "orange", "yellow")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

north2(siteOnePlot, symbol = 16) 

siteTwoPlot <- get_map("41.71397 -73.84609", zoom = 13) %>% ggmap() +
  geom_point(data = twoPoints, aes(x = lon, y = lat, color=μS), size = 2 ) +
  scale_colour_gradientn(colors=c("light blue","blue", "black", "orange", "yellow")) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

north2(siteTwoPlot, symbol = 16) 

siteThreePlot <- get_map("41.79155 -73.72755", zoom = 14) %>% ggmap() +
  geom_point(data = threePoints, aes(x = lon, y = lat, color=μS), size = 2 ) +
  scale_colour_gradientn(colors=c("light blue","blue", "black", "orange", "yellow")) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

north2(siteThreePlot, symbol = 16) 

#  --------Plots------



dataFilter <- function(data, num, inFault){
  if(inFault){
    filteredData = data %>% 
      select(EC, Temp, Time, NEC, NTemp, InFault, `Site Number`) %>% 
      filter(InFault == "yes")
  } else {
    filteredData = data %>% 
      select(EC, Temp, Time, NEC, NTemp, InFault, `Site Number`) %>% 
      filter(InFault == "no")
  }
  if(num != 0){
    filteredData = filteredData %>% 
      select(EC, Temp, Time, NEC, NTemp, InFault, `Site Number`) %>% 
      filter(`Site Number` == num)
  }
  return(filteredData)
}

filteredData = dataFilter(mergedData, 0, F)
inFaultData = dataFilter(mergedData, 0, T)

filteredData %>% ggplot() + 
  geom_jitter(data = filteredData, aes(x=as.numeric(Time), y=as.numeric(EC)), color='black', size=2) +
  ggtitle("Stream 3 Non-Fault Points EC vs Time") +
  xlab("Time (Arbitrary Unit)") +
  ylab("EC (micro semens)")

mergedData %>% ggplot() + 
  geom_jitter(data = filteredData, aes(x=as.numeric(NEC), y=as.numeric(Temp)), color='black', size=2) +
  geom_jitter(data = inFaultData, aes(x=as.numeric(NEC), y=as.numeric(Temp)), color='red', size=2) +
  ggtitle("Normalized EC vs Temp") +
  xlab("Normalized Electroconductivity") +
  ylab("Temperture (Celcius)")

mergedData %>% ggplot(aes(group=`Site Number`,y=as.numeric(NEC))) + 
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  ggtitle("Box and Whisker Plot of NEC") +
  xlab("Sampling Site Number") +
  ylab("Normalized EC")

mergedData %>% ggplot(aes(`Site Number`,as.numeric(NEC))) + 
  geom_point(width = 0.2, aes(color=mergedData$InFault)) +
  coord_flip() +
  geom_boxplot(outlier.shape = NA) +   
  ggtitle("Box and Whisker Plot of Normalized EC") +
  xlab("Sampling Site Number") +
  ylab("Normalized EC")

mergedData %>% 
  ggplot(aes(x=as.numeric(NEC), color=InFault)) + 
  geom_density(color="darkblue", fill="lightblue") + 
  geom_vline(aes(xintercept=mean(as.numeric(NEC)),
                 color="Mean"), linetype="dashed", size=.5) +
  # geom_vline(aes(xintercept=mean(as.numeric(NEC) + me),
  #                color="CI_UB"), linetype="dashed", size=.5) +
  # geom_vline(aes(xintercept=mean(as.numeric(NEC) - me),
  #                color="CI_LB"), linetype="dashed", size=.5) +
  ggtitle("Distribution of Normalized EC") +
  xlab("Normalized EC") +
  scale_color_manual(name = "statistics", values = c(Mean = "blue", CI_UB = "red", CI_LB="dark red"))


# ----z test -----

siteOneZoneOne = onePoints[1:4,]
siteOneZoneTwo = onePoints[5:9,]

siteTwoZoneOne = twoPoints[1:10,]
siteTwoZoneTwo = twoPoints[11:18,]

siteThreeZoneOne = threePoints[4:20,]
siteThreeZoneTwo = rbind(threePoints[1:3,],threePoints[21:23,])
siteThreeZoneThree = threePoints[24:40,]
siteThreeZoneFour = threePoints[40:46,]

xBar1_1 = mean(as.numeric(siteOneZoneOne$EC))
xBar1_2 = mean(as.numeric(siteOneZoneTwo$EC))

xBar2_1 = mean(as.numeric(siteTwoZoneOne$EC))
xBar2_2 = mean(as.numeric(siteTwoZoneTwo$EC))

xBar3_1 = mean(as.numeric(siteThreeZoneOne$EC))
xBar3_2 = mean(as.numeric(siteThreeZoneTwo$EC))
xBar3_3 = mean(as.numeric(siteThreeZoneThree$EC))
xBar3_4 = mean(as.numeric(siteThreeZoneFour$EC))

s1_1 = sd(as.numeric(siteOneZoneOne$EC))
s1_2 = sd(as.numeric(siteOneZoneTwo$EC))

s2_1 = sd(as.numeric(siteTwoZoneOne$EC))
s2_2 = sd(as.numeric(siteTwoZoneTwo$EC))

s3_1 = sd(as.numeric(siteThreeZoneOne$EC))
s3_2 = sd(as.numeric(siteThreeZoneTwo$EC))
s3_3 = sd(as.numeric(siteThreeZoneThree$EC))
s3_4 = sd(as.numeric(siteThreeZoneFour$EC))

n1_1 = length(as.numeric(siteOneZoneOne$EC))
n1_2 = length(as.numeric(siteOneZoneTwo$EC))

n2_1 = length(as.numeric(siteTwoZoneOne$EC))
n2_2 = length(as.numeric(siteTwoZoneTwo$EC))

n3_1 = length(as.numeric(siteThreeZoneOne$EC))
n3_2 = length(as.numeric(siteThreeZoneTwo$EC))
n3_3 = length(as.numeric(siteThreeZoneThree$EC))
n3_4 = length(as.numeric(siteThreeZoneFour$EC))


tStat1_1 = (xBar1_2 - xBar1_1)/(s1_2 / sqrt(n1_2))

tStat2_2 = (xBar2_2 - xBar2_1)/(s2_2 / sqrt(n2_2))

tStat3_2 = (xBar3_2 - xBar3_1)/(s3_2 / sqrt(n3_2))

tStat3_3 = (xBar3_3 - xBar3_2)/(s3_3 / sqrt(n3_3))

tStat3_4 = (xBar3_4 - xBar3_3)/(s3_4 / sqrt(n3_4))


