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
svytotal(~DOLELSPH, des)

#Electricity costs for air conditioning (“DOLELCOL” variable),

RECS2015$DOLELCOL <- currency(RECS2015$DOLELCOL,
                              symbol = "$",
                              digits = 0L,
                              format = "f",
                              big.mark = ",",
                              sep = "")
svytotal(~DOLELCOL, des)

#Electricity costs for water heating (“DOLELWTH” variable),

RECS2015$DOLELWTH <- currency(RECS2015$DOLELWTH,
                              symbol = "$",
                              digits = 0L,
                              format = "f",
                              big.mark = ",",
                              sep = "")
svytotal(~DOLELWTH, des)
#Electricity costs for all refrigerators (“DOLELRFG” variable),

RECS2015$DOLELRFG <- currency(RECS2015$DOLELRFG,
                              symbol = "$",
                              digits = 0L,
                              format = "f",
                              big.mark = ",",
                              sep = "")
svytotal(~DOLELRFG, des)
#Annual gross household income for the last year (“MONEYPY” variable),

RECS2015$MONEYPY <- as.factor(RECS2015$MONEYPY)

#Electricity cost differences between urban and rural areas (“UATYP10” variable),

#rename levels of area 
RECS2015 <- RECS2015 %>% 
  mutate(UATYP10 = as.factor(recode(UATYP10,
                          U = "Urban Area",
                          R = "Rural",
                          C = "Urban Cluster")))

#dataframe of area and total energy
Tot_Energy_area_df <- data.frame(RECS2015$UATYP10, RECS2015$DOLLAREL, RECS2015$DIVISION)

#rough plot to see expenditure differences by density classification
plot(y = Tot_Energy_area_df$RECS2015.DOLLAREL,
     x = Tot_Energy_area_df$RECS2015.UATYP10)

#What are the energy costs (“DOLLAREL” variable) for homeowners based on the number of rooms (“TOTROOMS” variable)?

house_size <- data.frame(RECS2015$DOLLAREL, RECS2015$TOTROOMS, RECS2015$TOTSQFT_EN)

#rough plot for total squarefootage (x), and energy costs (y)
plot(x = house_size$RECS2015.TOTSQFT_EN,
     y = house_size$RECS2015.DOLLAREL)


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
