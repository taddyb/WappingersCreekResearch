# Author: Tadd Bindas
# Description: To produce graphs which plot the Wappingers Data
# install.packages("readxl")
# install.packages("lubridate")
# install.packages("ggplot2")
library(readxl)
library(lubridate)
library(ggplot2)
library(config)
library(ggmap)
library(tidyverse)

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

mergedData = rbind(siteOne,siteTwo,siteThree,siteFour)
# plot(x=siteTwo$Time, y=siteTwo$NEC)
#   

# ---------Maps--------- #

# Map stuff:
# states <- map_data("state")
# counties <- map_data("county")
# new_york<- subset(states, region %in% c("new york"))
# ny_county <- subset(counties, region == "new york")
# 
# ny_base <- ggplot(data = new_york, mapping = aes(x = long, y = lat, group = group)) +
#   coord_fixed(1.3) +
#   geom_polygon(color = "black", fill = "gray")
# 
# gg1 <- ny_base + theme_nothing() +
#   geom_polygon(data = ny_county, fill = NA, color = "white") +
#   geom_polygon(color = "black", fill = NA)
#   # get the state border back on top
# ggplot(data = new_york) + 
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
#   coord_fixed(1.3)


Sys.setenv(R_CONFIG_ACTIVE = "config")
config <- config::get(file = "config.yml")
config[1]

# Please cite ggmap if you use it: see citation("ggmap") for details.

# save api key
register_google(key = config[1])
# check if key is saved
has_goog_key()

onePoints = data.frame(lon=as.numeric(siteOne$lon), lat=as.numeric(siteOne$lat))
twoPoints = data.frame(lon=as.numeric(siteTwo$lon), lat=as.numeric(siteTwo$lat))
threePoints = data.frame(lon=as.numeric(siteThree$lon), lat=as.numeric(siteThree$lat))
fourPoints = data.frame(lon=as.numeric(siteFour$lon), lat=as.numeric(siteFour$lat))
fivePoints = data.frame(lon=as.numeric(siteFive$lon), lat=as.numeric(siteFive$lat))


# USE WITH GGPLOT
get_map("Pleasent Valley, New York", zoom = 11) %>% ggmap() +
  geom_point(data = onePoints, aes(x = lon, y = lat), color = 'red', size = 2) +
  geom_point(data = twoPoints, aes(x = lon, y = lat), color = 'blue', size = 2) +
  geom_point(data = threePoints, aes(x = lon, y = lat), color = 'green', size = 2) +
  geom_point(data = fourPoints, aes(x = lon, y = lat), color = 'black', size = 2) +
  geom_point(data = fivePoints, aes(x = lon, y = lat), color = 'purple', size = 2) +
  theme(legend.position = c(0.06, 0.75))
  # scale_color_manual(name = "Site Number", # or name = element_blank()
  #                    labels = c("Site 1","Site 2", "Site 3", "Site 4", "Site 5" ),
  #                    values = c('red', 'blue', 'green', 'black', 'purple'))

get_map("Cary Institute", zoom = 14) %>% ggmap() +
  geom_point(data = threePoints, aes(x = lon, y = lat), color = 'green', size = 2) +
  geom_point(data = fourPoints, aes(x = lon, y = lat), color = 'black', size = 2) +
  geom_point(data = fivePoints, aes(x = lon, y = lat), color = 'purple', size = 2)


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

mergedData 

mergedData %>% ggplot(aes(`Site Number`,as.numeric(NEC))) + 
  geom_point(width = 0.2, aes(color=mergedData$InFault)) +
  coord_flip() +
  geom_boxplot(outlier.shape = NA) +   
  ggtitle("Box and Whisker Plot of Normalized EC") +
  xlab("Sampling Site Number") +
  ylab("Normalized EC")

filteredData %>% 
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

View(filteredData)

xBar = mean(as.numeric(filteredData$NEC))
s = sd(as.numeric(filteredData$NEC))
n = length(as.numeric(filteredData$NEC))

x = mean(as.numeric(inFaultData$NEC))
s1 = sd(as.numeric(inFaultData$NEC))
n2 = length(as.numeric(inFaultData$NEC))


n = n2             # sample size 
z = (x-xBar)/(s/sqrt(n2))

error = qnorm(.95)





