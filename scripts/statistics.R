# Author: Tadd Bindas
# Description: To produce the statistics for my research
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

# ----Data Prep -----

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
siteOneNormal = normalize(splitData$`1`)
siteTwoNormal = normalize(splitData$`2`)
siteThreeNormal = normalize(splitData$`3`)
siteFourNormal = normalize(splitData$`4`)
siteFiveNormal = normalize(splitData$`5`)

siteOne = splitData$`1`
siteTwo = splitData$`2`
siteThree = splitData$`3`
siteFour = splitData$`4`
siteFive = splitData$`5`

siteThreeMerged = rbind(siteFive,siteThree,siteFour)
siteThreeMergedNormal = rbind(siteFiveNormal,siteThreeNormal,siteFourNormal)

mergedData = rbind(siteOne,siteTwo,siteThree)
mergedDataNormal = rbind(siteOneNormal,siteTwoNormal,siteThreeNormal)

# ----z test -----

# ------------NORMALIZED VALUES -------------

siteOneZoneOne = onePoints[1:4,]
siteOneZoneTwo = onePoints[5:9,]

siteTwoZoneOne = twoPoints[1:10,]
siteTwoZoneTwo = twoPoints[11:18,]

siteThreeZoneOne = threePoints[4:20,]
siteThreeZoneTwo = rbind(threePoints[1:3,],threePoints[21:23,])
siteThreeZoneThree = threePoints[24:40,]
siteThreeZoneFour = threePoints[40:46,]

xBarNormal1_1 = mean(as.numeric(siteOneZoneOne$μS))
xBarNormal1_2 = mean(as.numeric(siteOneZoneTwo$μS))

xBarNormal2_1 = mean(as.numeric(siteTwoZoneOne$μS))
xBarNormal2_2 = mean(as.numeric(siteTwoZoneTwo$μS))

xBarNormal3_1 = mean(as.numeric(siteThreeZoneOne$μS))
xBarNormal3_2 = mean(as.numeric(siteThreeZoneTwo$μS))
xBarNormal3_3 = mean(as.numeric(siteThreeZoneThree$μS))
xBarNormal3_4 = mean(as.numeric(siteThreeZoneFour$μS))

sNormal1_1 = sd(as.numeric(siteOneZoneOne$μS))
sNormal1_2 = sd(as.numeric(siteOneZoneTwo$μS))

sNormal2_1 = sd(as.numeric(siteTwoZoneOne$μS))
sNormal2_2 = sd(as.numeric(siteTwoZoneTwo$μS))

sNormal3_1 = sd(as.numeric(siteThreeZoneOne$μS))
sNormal3_2 = sd(as.numeric(siteThreeZoneTwo$μS))
sNormal3_3 = sd(as.numeric(siteThreeZoneThree$μS))
sNormal3_4 = sd(as.numeric(siteThreeZoneFour$μS))


# ------------Non- normal VALUES -------------

siteOneZoneOne = siteOne[1:4,]
siteOneZoneTwo = siteOne[5:9,]

siteTwoZoneOne = siteTwo[1:10,]
siteTwoZoneTwo = siteTwo[11:18,]

siteThreeZoneOne = siteThreeMerged[4:20,]
siteThreeZoneTwo = rbind(siteThreeMerged[1:3,],siteThreeMerged[21:23,])
siteThreeZoneThree = siteThreeMerged[24:40,]
siteThreeZoneFour = siteThreeMerged[40:46,]


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

# ---------------------------------------

n1_1 = length(as.numeric(siteOneZoneOne$μS))
n1_2 = length(as.numeric(siteOneZoneTwo$μS))

n2_1 = length(as.numeric(siteTwoZoneOne$μS))
n2_2 = length(as.numeric(siteTwoZoneTwo$μS))

n3_1 = length(as.numeric(siteThreeZoneOne$μS))
n3_2 = length(as.numeric(siteThreeZoneTwo$μS))
n3_3 = length(as.numeric(siteThreeZoneThree$μS))
n3_4 = length(as.numeric(siteThreeZoneFour$μS))


tStat1_1 = (xBar1_2 - xBar1_1)/(s1_2 / sqrt(n1_2))

tStat2_2 = (xBar2_2 - xBar2_1)/(s2_2 / sqrt(n2_2))

tStat3_2 = (xBar3_2 - xBar3_1)/(s3_2 / sqrt(n3_2))

tStat3_3 = (xBar3_3 - xBar3_2)/(s3_3 / sqrt(n3_3))

tStat3_4 = (xBar3_4 - xBar3_3)/(s3_4 / sqrt(n3_4))

