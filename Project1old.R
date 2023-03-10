#setwd() Meng Fei's WD  
setwd("C:/Users/18045/Documents/R/Data_Intro_Class/Project1")# Sean's WD
#setwd("C:/Users/danif/OneDrive/Documents/GWU - Data Science (Spring 2023)/DATS 6101/Project/Project1.R") Daniel's WD
library(readr)
library(ggplot2)
#install.packages("survey","ggmap","maps","mapdata","formattable", "forcats", "RColorBrewer", "reshape2")
library(survey)
library(dplyr)
library(ggmap)
library(maps)
library(mapdata)
library(formattable)
library(forcats)
library(RColorBrewer)
library(ezids)
library(reshape2)

#LoadData and Example Code for Assigning weights----- 
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

#Appliances info ----

app_cost <- data.frame(RECS2015$DOLLAREL,RECS2015$DOLELSPH,RECS2015$DOLELCOL,
                   RECS2015$DOLELWTH,RECS2015$DOLELRFG,RECS2015$DOLELFRZ,
                   RECS2015$DOLELCOK,RECS2015$DOLELMICRO,RECS2015$DOLELCW,
                   RECS2015$DOLELCDR,RECS2015$DOLELDWH,RECS2015$DOLELLGT,
                   RECS2015$DOLELTVREL,RECS2015$DOLELAHUHEAT,RECS2015$DOLELAHUCOL,
                   RECS2015$DOLELEVAPCOL,RECS2015$DOLELCFAN,RECS2015$DOLELDHUM,
                   RECS2015$DOLELHUM,RECS2015$DOLELPLPMP,RECS2015$DOLELHTBPMP,
                   RECS2015$DOLELHTBHEAT,RECS2015$DOLELNEC)

series <- data.frame(rep(1, nrow(app_cost)))
Spaceheating<- RECS2015$DOLELSPH
AC<- RECS2015$DOLELCOL
Waterheating<-RECS2015$DOLELWTH
Refrigerator<-RECS2015$DOLELRFG
Freezer<-RECS2015$DOLELFRZ
Cooking<-RECS2015$DOLELCOK
Microwave<-RECS2015$DOLELMICRO
Clothewasher<-RECS2015$DOLELCW
Clothedryer<-RECS2015$DOLELCDR
Dishwasher<-RECS2015$DOLELDWH
Light<-RECS2015$DOLELLGT
TV<-RECS2015$DOLELTVREL
Airhandlerheating<-RECS2015$DOLELAHUHEAT
Airhandlercooling<-RECS2015$DOLELAHUCOL
Fan<-RECS2015$DOLELCFAN


app_cost <- data.frame(
                       Spaceheating, 
                       AC, 
                       Waterheating,
                       Light, 
                       TV,
                       Dishwasher,
                       Refrigerator, 
                       Freezer, 
                       Cooking, 
                       Microwave, 
                       Clothewasher, 
                       Clothedryer,
                       Fan)

colnames(app_cost) <- c(
                        "Spaceheating", 
                         "AC", 
                         "Waterheating",
                         "Light", 
                         "TV",
                         "Dishwasher",
                         "Refrigerator", 
                         "Freezer", 
                         "Cooking", 
                         "Microwave", 
                         "Clothewasher", 
                         "Clothedryer",
                         "Fan")

app_cost <- outlierKD2(app_cost, (Spaceheating), rm =TRUE)
app_cost <- outlierKD2(app_cost, (AC), rm =TRUE) 
app_cost <- outlierKD2(app_cost, (Waterheating), rm =TRUE)
app_cost <- outlierKD2(app_cost, (Light), rm =TRUE)
app_cost <- outlierKD2(app_cost, (TV), rm =TRUE)  
app_cost <- outlierKD2(app_cost, (Dishwasher), rm =TRUE)
app_cost <- outlierKD2(app_cost, (Refrigerator), rm =TRUE)
app_cost <- outlierKD2(app_cost, (Freezer), rm =TRUE)
app_cost <- outlierKD2(app_cost, (Cooking), rm =TRUE)
app_cost <- outlierKD2(app_cost, (Microwave), rm =TRUE)
app_cost <- outlierKD2(app_cost, (Clothewasher), rm =TRUE)
app_cost <- outlierKD2(app_cost, (Clothedryer), rm =TRUE)
app_cost <- outlierKD2(app_cost, (Fan), rm =TRUE)

app_cost["series"] <- rep(1, nrow(app_cost))
                      
test <- melt(app_cost,  id.vars = 'series', variable.name = 'index')

test %>%
  arrange(index)%>%
  mutate(index = factor(index, levels = c("series",
                                         "Spaceheating", 
                                         "AC", 
                                         "Waterheating",
                                         "Light", 
                                         "TV",
                                         "Clothedryer",
                                         "Refrigerator", 
                                         "Fan",
                                         "Freezer", 
                                         "Cooking", 
                                         "Microwave", 
                                         "Dishwasher",
                                         "Clothewasher"))) %>%
ggplot(
       aes(x = series,
           y = value,
           fill = index))+ 
  geom_bar(stat = "identity", 
           position = "dodge") +
  labs(title = "Annual Electricity Cost of Different Appliances", 
       x = "Appliances",
       y = "Annual Electricity Cost")+  
  theme(axis.text.x = element_blank())
  
#Central Air------
#Electricity costs for space heating (???DOLELSPH??? variable),

RECS2015$DOLELSPH <- currency(RECS2015$DOLELSPH,
                              symbol = "$",
                              digits = 0L,
                              format = "f",
                              big.mark = ",",
                              sep = "")
svytotal(~DOLELSPH, des)

#Electricity costs for air conditioning (???DOLELCOL??? variable),

RECS2015$DOLELCOL <- currency(RECS2015$DOLELCOL,
                              symbol = "$",
                              digits = 0L,
                              format = "f",
                              big.mark = ",",
                              sep = "")
svytotal(~DOLELCOL, des)

#Heat Pump

RECS2015 <- RECS2015 %>% 
  mutate(CENACHP = as.factor(case_when(CENACHP == 1 ~ "Has a Heat Pump",
                                       CENACHP == 0 ~ "No Heat Pump",
                                       CENACHP == -2 ~ "NA")))

svytotal(~CENACHP, des)

central_air_df <- data.frame(RECS2015$DOLELSPH, 
                             RECS2015$DOLELCOL, 
                             RECS2015$CENACHP)

colnames(central_air_df) <- c("Electricity Space Heating Costs", 
                              "Electricity AC Costs", 
                              "Heat Pump Status")
RECS2015 %>%
  filter(EQUIPM == 4)%>%
  select(EQUIPM)
  

#heatpump v. space heating costs plot
ggplot(central_air_df, 
       aes(x = `Heat Pump Status`,
           y = `Electricity Space Heating Costs`,
           fill = `Heat Pump Status`))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Space Heating Costs With and Without Heatpumps")

#heatpump v. ac costs plot
ggplot(central_air_df, 
       aes(x = `Heat Pump Status`,
           y = `Electricity AC Costs`,
           fill = `Heat Pump Status`))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "AC Costs With and Without Heatpumps")

#t-test for heatpumps v. spaceheater

ttest_central_air_df <- central_air_df %>%
  mutate(`alt` = central_air_df$`Heat Pump Status` == "Has a Heat Pump",
         'null' = central_air_df$`Heat Pump Status` == "No Heat Pump")

no_hp <- ttest_central_air_df %>%
  filter(null == TRUE)
    
has_hp <- ttest_central_air_df %>%
  filter(alt == TRUE)

t.test(x = has_hp$`Electricity Space Heating Costs`,
       conf.level = 0.95,
       mu = mean(no_hp$`Electricity Space Heating Costs`))

#t-test for heatpumps v. AC

no_hp <- ttest_central_air_df %>%
  filter(null == TRUE)

has_hp <- ttest_central_air_df %>%
  filter(alt == TRUE)

t.test(x = has_hp$`Electricity AC Costs`,
       conf.level = 0.95,
       mu = mean(no_hp$`Electricity AC Costs`))

#chisquared test of heatpump related to income (removed NAs)
nona_central_air_df <- central_air_df #make sure to load income section DF before running this
nona_central_air_df <- subset(central_air_df, 
                              subset = `Heat Pump Status` != 'NA', 
                              drop = TRUE)

test <- table(nona_central_air_df$`Heat Pump Status`,
              nona_central_air_df$Income)

test <- table(droplevels(nona_central_air_df)$`Heat Pump Status`,
      nona_central_air_df$Income)

chisq_hp_inc <- chisq.test(test)

#other common appliances----- ----
#Electricity costs for water heating (???DOLELWTH??? variable)----

RECS2015$DOLELWTH <- currency(RECS2015$DOLELWTH,
                              symbol = "$",
                              digits = 0L,
                              format = "f",
                              big.mark = ",",
                              sep = "")
svytotal(~DOLELWTH, des)

#Electricity costs for all refrigerators (???DOLELRFG??? variable),

RECS2015$DOLELRFG <- currency(RECS2015$DOLELRFG,
                              symbol = "$",
                              digits = 0L,
                              format = "f",
                              big.mark = ",",
                              sep = "")
svytotal(~DOLELRFG, des)

#Income and Energy Expenditure info ------
#Annual gross household income for the last year (???MONEYPY??? variable),

RECS2015 <- RECS2015 %>%
  mutate(MONEYPY = as.factor(case_when(MONEYPY == 1 ~ "Less than $20,000",
                                       MONEYPY == 2 ~ "$20,000 - $39,999",
                                       MONEYPY == 3 ~ "$40,000 - $59,999",
                                       MONEYPY == 4 ~ "$60,000 to $79,999",
                                       MONEYPY == 5 ~ "$80,000 to $99,999",
                                       MONEYPY == 6 ~ "$100,000 to $119,999",
                                       MONEYPY == 7 ~ "$120,000 to $139,999",
                                       MONEYPY == 8 ~ "$140,000 or more")))

central_air_df["Income"] <- data.frame(RECS2015$MONEYPY)

has_hp <- central_air_df %>%
  filter(`Heat Pump Status` == "Has a Heat Pump")

no_hp <- central_air_df %>%
  filter(`Heat Pump Status` == "No Heat Pump")

central_air_df %>%
  arrange(`Heat Pump Status`) %>%
  mutate(Income = factor(Income, levels = c("Less than $20,000",
                                            "$20,000 - $39,999",
                                            "$40,000 - $59,999",
                                            "$60,000 to $79,999",
                                            "$80,000 to $99,999",
                                            "$100,000 to $119,999",
                                            "$120,000 to $139,999",
                                            "$140,000 or more")))%>%
  ggplot(aes(x = Income,
             y = `Heat Pump Status`,
             fill = `Heat Pump Status`))+
  geom_bar(stat = "identity")+
  labs(title = "Income Bracket And Heat Pump Status",
       ylab = "")+
  theme(axis.text.x = element_text(angle = 45, size = 9, margin = margin(r=0)),
        axis.text.y=element_blank()) #needs to fix income brackets to be ascending

#boxplot of income and yearly energy costs
central_air_df["TotElectricity"] <- RECS2015$DOLLAREL

central_air_df <- outlierKD2(central_air_df, TotElectricity, rm= TRUE)

central_air_df %>%
  arrange(`Heat Pump Status`) %>%
  mutate(Income = factor(Income, levels = c("Less than $20,000",
                                            "$20,000 - $39,999",
                                            "$40,000 - $59,999",
                                            "$60,000 to $79,999",
                                            "$80,000 to $99,999",
                                            "$100,000 to $119,999",
                                            "$120,000 to $139,999",
                                            "$140,000 or more")))%>%
  ggplot(aes(x = Income,
             y = TotElectricity, 
             color = Income)) + 
  geom_boxplot(stat = "boxplot") +
  labs(title = " Electricity Cost Between Different Income Level ", 
       x = "Income level",
       y = "Yearly Electricity Cost (in Dollars)")+  
  theme(axis.text.x = element_text(angle = 45, size = 9, margin = margin(r=0)), 
        axis.text.y=element_text())

#Spatial differences info------

#Preparing Census Regions 

RECS2015 <- RECS2015 %>%
  mutate(REGIONC = as.factor(case_when(REGIONC == 1 ~ "Northeast",
                                       REGIONC == 2 ~ "Midwest",
                                       REGIONC == 3 ~ "South",
                                       REGIONC == 4 ~ "West")))
RECS2015$REGIONC
plot(RECS2015$REGIONC)

#Preparing Census Divisions

RECS2015 <- RECS2015 %>%
  mutate(DIVISION = as.factor(case_when(DIVISION == 1 ~ "New England",
                                       DIVISION == 2 ~ "Middle Atlantic",
                                       DIVISION == 3 ~ "East North Central",
                                       DIVISION == 4 ~ "West North Central",
                                       DIVISION == 5 ~ "South Atlantic",
                                       DIVISION == 6 ~ "East South Central",
                                       DIVISION == 7 ~ "West South Central",
                                       DIVISION == 8 ~ "Mountain North",
                                       DIVISION == 9 ~ "Mountain South",
                                       DIVISION == 10 ~ "Pacific",)))

RECS2015$DIVISION
plot(RECS2015$DIVISION)

#Electricity cost differences between urban and rural areas (???UATYP10??? variable),

#rename levels of area 
RECS2015 <- RECS2015 %>% 
  mutate(UATYP10 = as.factor(recode(UATYP10,
                          U = "Urban Area",
                          R = "Rural",
                          C = "Urban Cluster")))

#dataframe of area and total energy
Tot_Energy_area_df <- data.frame(RECS2015$UATYP10, RECS2015$DOLLAREL, RECS2015$DIVISION, RECS2015$CLIMATE_REGION_PUB)
colnames(Tot_Energy_area_df) <- c("Urban Density", "Yearly Electricity Costs", "Division", "Climate")

#Yearly expenditure differences by Division and Density-----

Tot_Energy_area_df <- outlierKD2(Tot_Energy_area_df, `Yearly Electricity Costs`, rm= TRUE)

Tot_Energy_area_df %>%
  arrange(`Yearly Electricity Costs`)%>%
  mutate(Division = factor(Division, levels = c("New England",
                                                "Middle Atlantic",
                                                "East North Central",
                                                "West North Central",
                                                "South Atlantic",
                                                "East South Central",
                                                "West South Central",
                                                "Mountain North",
                                                "Mountain South",
                                                "Pacific")))%>%
  ggplot(
    aes(x = `Urban Density`,
        y = `Yearly Electricity Costs`,
        fill = fct_reorder(`Division`, `Yearly Electricity Costs`)))+
  geom_boxplot(stat = "boxplot", 
               position = "dodge")+
  labs(title = "Urban Density and Electricity Costs",
       fill= "Division")+
  theme(axis.text.x = element_text(size = 9, margin = margin(r=0)), 
        axis.text.y=element_text())+ 
  scale_color_brewer(palette = "Pastel2")

chisq.test(Tot_Energy_area_df[c(1,2)])

#What are the energy costs (???DOLLAREL??? variable) for homeowners based on the number of rooms (???TOTROOMS??? variable)?-----

house_size <- data.frame(RECS2015$DOLLAREL, RECS2015$TOTROOMS, RECS2015$TOTSQFT_EN)

#rough plot for total squarefootage (x), and energy costs (y)
plot(x = log(house_size$RECS2015.TOTSQFT_EN),
     y = log(house_size$RECS2015.DOLLAREL))


#Is Electric heating and cooling costs respective to certain climates (???CLIMATE_REGION_PUB???)?

hist(log(RECS2015$TOTSQFT_EN))
hist(log(RECS2015$DOLLAREL))


heatpump_lm <- lm(CENACHP ~ TOTSQFT_EN + DOLLAREL + KWHCOL + BTUELCOL, data = RECS2015)

summary(heatpump_lm)

plot(heatpump_lm)





#Mapping the data------
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

# census divisions

states <- states %>%
  mutate(region = as.factor(case_when(region == "connecticut" ~ "new_england",
                                      region == "maine" ~ "new_england",
                                      region == "massachusetts" ~ "new_england",
                                      region == "new hampshire" ~ "new_england",
                                      region == "rhode island" ~ "new_england",
                                      region == "vermont" ~ "new_england",
                                      region == "new jersey" ~ "middle_atlantic",
                                      region == "new york" ~ "middle_atlantic",
                                      region == "pennsylvania" ~ "middle_atlantic",
                                      region == "indiana" ~ "east_north_central",
                                      region == "illinois" ~ "east_north_central",
                                      region == "michigan" ~ "east_north_central",
                                      region == "ohio" ~ "east_north_central",
                                      region == "wisconsin" ~ "east_north_central",
                                      region == "iowa" ~ "west_north_central",
                                      region == "kansas" ~ "west_north_central",
                                      region == "minnesota" ~ "west_north_central",
                                      region == "missouri" ~ "west_north_central",
                                      region == "nebraska" ~ "west_north_central",
                                      region == "north dakota" ~ "west_north_central",
                                      region == "south dakota" ~ "west_north_central",
                                      region == "delaware" ~ "south_atlantic",
                                      region == "district of columbia" ~ "south_atlantic",
                                      region == "florida" ~ "south_atlantic",
                                      region == "georgia" ~ "south_atlantic",
                                      region == "maryland" ~ "south_atlantic",
                                      region == "north carolina" ~ "south_atlantic",
                                      region == "south carolina" ~ "south_atlantic",
                                      region == "virginia" ~ "south_atlantic",
                                      region == "west virginia" ~ "south_atlantic",
                                      region == "alabama" ~ "east_south_central",
                                      region == "kentucky" ~ "east_south_central",
                                      region == "mississippi" ~ "east_south_central",
                                      region == "tennessee" ~ "east_south_central",
                                      region == "arkansas" ~ "west_south_central",
                                      region == "louisiana" ~ "west_south_central",
                                      region == "oklahoma" ~ "west_south_central",
                                      region == "texas" ~ "west_south_central",
                                      region == "arizona" ~ "mountain",
                                      region == "colorado" ~ "mountain",
                                      region == "idaho" ~ "mountain",
                                      region == "new mexico" ~ "mountain",
                                      region == "montana" ~ "mountain",
                                      region == "utah" ~ "mountain",
                                      region == "nevada" ~ "mountain",
                                      region == "wyoming" ~ "mountain",
                                      region == "alaska" ~ "pacific",
                                      region == "california" ~ "pacific",
                                      region == "hawaii" ~ "pacific",
                                      region == "oregon" ~ "pacific",
                                      region == "washington" ~ "pacific",)))

ggplot(data = states) + 
  geom_polygon(aes(x = long, 
                   y = lat, 
                   fill = region, 
                   group = group), 
               color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                     panel.background = element_blank(), axis.line = element_line(colour = "black"))

# rename "region" column from `states` dataframe to match `RECS2015` column name
colnames(states) <- c("long", "lat", "group", "order", "DIVISION", "subregion")

# rename `Tot_Energy_area_df` columns
#see line 263 ish

colnames(Tot_Energy_area_df) <- c("Urban Type", "Elec cost", "DIVISION")

# subsetting divisions to find their average energy cost --> will remove because aggregate works better
Tot_Energy_NewEngland <- Tot_Energy_area_df[Tot_Energy_area_df$DIVISION == "new_england", ]
NewEngland_CostMean <- mean(Tot_Energy_NewEngland$`Elec cost`)

Tot_Energy_MiddleAtlantic <- Tot_Energy_area_df[Tot_Energy_area_df$DIVISION == "middle_atlantic", ]
MiddleAtlantic_CostMean <- mean(Tot_Energy_MiddleAtlantic$`Elec cost`)

# Climate data-----

RECS2015$CLIMATE_REGION_PUB <- as.factor(RECS2015$CLIMATE_REGION_PUB)
Tot_Energy_area_df$Climate

Tot_Energy_area_df %>%
  arrange(`Yearly Electricity Costs`)%>%
  mutate(Climate = factor(Climate, levels = c("Hot-Dry/Mixed-Dry","Marine","Cold/Very Cold","Mixed-Humid","Hot-Humid")))%>%
ggplot(aes(x = Climate,
           y = `Yearly Electricity Costs`,
           fill = Climate)) +
  geom_boxplot() +
  labs(title = "Yearly Electricity Expenditure by Climate")

# Histogram of Electricity Costs
ggplot(data=Tot_Energy_area_df, aes(x = `Yearly Electricity Costs`)) + 
  geom_histogram(breaks=seq(19, 8122, by = 100), 
                 col="black", 
                 fill="dark green", 
                 alpha = .7) + # opacity
  labs(x="Electricity Cost", y="Frequency") +
  labs(title="Histogram of Total Electricity Cost, Using `ggplot`")
