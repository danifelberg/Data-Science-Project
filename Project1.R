#setwd() Meng Fei's WD  
setwd("C:/Users/18045/Documents/R/Data_Intro_Class/Project1")# Sean's WD
#setwd() Daniel's WD
library(readr)
library(ggplot2)
#install.packages("survey","ggmap","maps","mapdata","formattable")
library(survey)
library(dplyr)
library(ggmap)
library(maps)
library(mapdata)
library(formattable)

#----LoadData and Assign weights. 
#this is example code from the EIA weights doc:

RECS2015 <- read.csv("recs2015_public_v4.csv", header=TRUE, sep=",")

RECS2015$NG_MAINSPACEHEAT <- ifelse(RECS2015$FUELHEAT == 1, 1, 0)

RECS2015$NG_MAINSPACEHEAT <- as.factor(RECS2015$NG_MAINSPACEHEAT)

sampweights <- RECS2015$NWEIGHT

brrwts <- RECS2015[grepl("^BRRWT", names(RECS2015))]

des <- svrepdesign(weights = sampweights, 
                   repweights = brrwts, 
                   type = "Fay", 
                   rho = 0.5, 
                   mse = TRUE, 
                   data = RECS2015)
des
svytotal(~NG_MAINSPACEHEAT, des)

#Electricity costs for space heating (“DOLELSPH” variable),

RECS2015$DOLELSPH <- currency(RECS2015$DOLELSPH,
                              symbol = "$",
                              digits = 0L,
                              format = "f",
                              big.mark = ",",
                              sep = "")

#Electricity costs for air conditioning (“DOLELCOL” variable),

RECS2015$DOLELCOL <- currency(RECS2015$DOLELCOL,
                              symbol = "$",
                              digits = 0L,
                              format = "f",
                              big.mark = ",",
                              sep = "")

#Electricity costs for water heating (“DOLELWTH” variable),

RECS2015$DOLELWTH <- currency(RECS2015$DOLELWTH,
                              symbol = "$",
                              digits = 0L,
                              format = "f",
                              big.mark = ",",
                              sep = "")

#Electricity costs for all refrigerators (“DOLELRFG” variable),

RECS2015$DOLELRFG <- currency(RECS2015$DOLELRFG,
                              symbol = "$",
                              digits = 0L,
                              format = "f",
                              big.mark = ",",
                              sep = "")

#Annual gross household income for the last year (“MONEPY” variable),


#Electricity cost differences between urban and rural areas (“UATYP10” variable),


#What are the energy costs (“DOLLAREL” variable) for homeowners based on the number of rooms (“TOTROOMS” variable)?



#Is Electric heating and cooling costs respective to certain climates (“CLIMATE_REGION_PUB”)?

#----Mapping the data
#relevant code for us to get started

usa <- map_data("usa")

states <- map_data("state")

ggplot(data = states) + 
  geom_polygon(aes(x = long, 
                   y = lat, 
                   fill = region, 
                   group = group), 
               color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # do this to leave off the color legend
