#setwd() Meng Fei's WD  
setwd("C:/Users/18045/Documents/R/Data_Intro_Class/Project1")# Sean's WD
#setwd("C:/Users/danif/OneDrive/Documents/GWU - Data Science (Spring 2023)/DATS 6101/Project/Project1.R") Daniel's WD
library(readr)
library(ggplot2)
#install.packages("survey","ggmap","maps","mapdata","formattable", "forcats", "RColorBrewer","gridExtra", "usmap", "xtable", "glmnet", "boot", Metrics","ggfortify","cowplots","rattle", "fancyRpartPlot")
library(survey)
library(dplyr)
library(ggmap)
library(maps)
library(mapdata)
library(usmap)
library(formattable)
library(forcats)
library(RColorBrewer)
library(ezids)
library(readxl)
library(gridExtra)
library(lattice)
library(corrplot)
library(reshape2)
library(xtable)
library(car)
library(glmnet)
library(Metrics)
library(boot)
library(ggfortify)
library(cowplot)
library(rattle)
library(fancyRpartPlot)
library(stargazer)

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

#PLOT General Appliances comparisons----

app_cost <- data.frame(RECS2015$DOLELSPH,
                       RECS2015$DOLELCOL,
                       RECS2015$DOLELWTH,
                       RECS2015$DOLELRFG,
                       RECS2015$DOLELFRZ,
                       RECS2015$DOLELCOK,
                       RECS2015$DOLELMICRO,
                       RECS2015$DOLELCW,
                       RECS2015$DOLELCDR,
                       RECS2015$DOLELDWH,
                       RECS2015$DOLELLGT,
                       RECS2015$DOLELTVREL,
                       RECS2015$DOLELCFAN)

colnames(app_cost) <- c(
  "Spaceheating", 
  "AC", 
  "Waterheating",
  "Refrigerator", 
  "Freezer",
  "Cooking", 
  "Microwave",
  "Clothewasher", 
  "Clothedryer",
  "Dishwasher",
  "Light", 
  "TV",
  "Fans")

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
app_cost <- outlierKD2(app_cost, (Fans), rm =TRUE)

app_cost <- app_cost[-ncol(app_cost)]

app_cost["series"] <- rep(1, nrow(app_cost))

test <- melt(app_cost,  id.vars = 'series', variable.name = 'index')

test %>%
  arrange(index)%>%
  mutate(index = factor(index, levels = c("series",
                                          "Waterheating",
                                          "AC",
                                          "Spaceheating", 
                                          "Light",
                                          "TV",
                                          "Refrigerator",
                                          "Clothedryer",
                                          "Fans",
                                          "Freezer",
                                          "Cooking", 
                                          "Microwave", 
                                          "Dishwasher",
                                          "Clothewasher"
                                          ))) %>%
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
#Electricity costs for space heating (“DOLELSPH” variable),

RECS2015$DOLELSPH <- currency(RECS2015$DOLELSPH,
                              symbol = "$",
                              digits = 2L,
                              format = "f",
                              big.mark = ",",
                              sep = "")
svytotal(~DOLELSPH, des)

#Electricity costs for air conditioning (“DOLELCOL” variable),

RECS2015$DOLELCOL <- currency(RECS2015$DOLELCOL,
                              symbol = "$",
                              digits = 2L,
                              format = "f",
                              big.mark = ",",
                              sep = "")
svytotal(~DOLELCOL, des)

#Natural Gas costs (“DOLLERNG” variable),

RECS2015$DOLLARNG <- currency(RECS2015$DOLLARNG,
                              symbol = "$",
                              digits = 2L,
                              format = "f",
                              big.mark = ",",
                              sep = "")

#Natural Gas costs (“DOLLARFO” variable),

RECS2015$DOLLARFO <- currency(RECS2015$DOLLARFO,
                              symbol = "$",
                              digits = 2L,
                              format = "f",
                              big.mark = ",",
                              sep = "")

#Heat Pump dataframe 

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

#dataframe prep for combo plot
i <- 1
newdf <- 0
for(i in 2:2){
  
  newdf["Electricity Costs in $"] <- rbind(as_tibble(central_air_df[,1]), as_tibble(central_air_df[,i]))
  
}
newdf <- as.data.frame(newdf)
newdf["Heat Pump Status"] <- rep(central_air_df$`Heat Pump Status`, 2) 
newdf["Group"] <- rbind(as_tibble(rep("Heating", nrow(central_air_df))), 
                        as_tibble(rep("Cooling", nrow(central_air_df)))) 

newdf <- newdf[-1]
colnames(newdf) <- c("Electricity Costs in $", "Heat Pump Status", "Group")

group.colors <- c("Heating" = "Orange", "Cooling" = "Light Blue")

#Plot Heatpump by Heating and Cooling new, combo graph----
ggplot(newdf,
       aes(x = `Heat Pump Status`,
           y = `Electricity Costs in $`,
           fill = `Group`))+
  geom_col(stat = "identity", position = "dodge")+
  labs(title = "Yearly Central Air Costs (in $)")+
  scale_fill_manual(values=group.colors)

#T-Test for heatpumps v. spaceheater----

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

#T-Test for heatpumps v. AC-----

no_hp <- ttest_central_air_df %>%
  filter(null == TRUE)

has_hp <- ttest_central_air_df %>%
  filter(alt == TRUE)

t.test(x = has_hp$`Electricity AC Costs`,
       conf.level = 0.95,
       mu = mean(no_hp$`Electricity AC Costs`))

#----------------------Income and Energy Expenditure info----------------------- ------
#Annual gross household income for the last year (“MONEYPY” variable),

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

RECS2015 <- RECS2015 %>%
  mutate(EDUCATION = as.factor(case_when(EDUCATION == 1 ~ "Less than high school diploma or GED",
                                         EDUCATION == 2 ~ "High school diploma or GED",
                                         EDUCATION == 3 ~ "Some college or Associate’s degree",
                                         EDUCATION == 4 ~ "Bachelor’s degree",
                                         EDUCATION == 5 ~ "Grad Plus degree")))

RECS2015 <- RECS2015 %>%
  mutate(YEARMADERANGE = as.factor(case_when(YEARMADERANGE == 1 ~ "Before 1950",
                                             YEARMADERANGE == 2 ~ "1950 to 1959",
                                             YEARMADERANGE == 3 ~ "1960 to 1969",
                                             YEARMADERANGE == 4 ~ "1970 to 1979",
                                             YEARMADERANGE == 5 ~ "1980 to 1989",
                                             YEARMADERANGE == 6 ~ "1990 to 1999",
                                             YEARMADERANGE == 7 ~ "2000 to 2009",
                                             YEARMADERANGE == 8 ~ "2010 to 2015")))

RECS2015 <- RECS2015 %>%
  mutate(EQUIPM = as.factor(case_when(EQUIPM == 2 ~ "Steam/hot water system with radiators or pipes",
                                      EQUIPM == 3 ~ "Central furnace",
                                      EQUIPM == 4 ~ "Heat pump",
                                      EQUIPM == 5 ~ "Built-in electric units installed in walls, ceilings, baseboards, or floors",
                                      EQUIPM == 6 ~ "Built-in floor/wall pipeless furnace",
                                      EQUIPM == 7 ~ "Built-in room heater burning gas, oil, or kerosene",
                                      EQUIPM == 8 ~ "Wood-burning stove (cordwood or pellets)",
                                      EQUIPM == 9 ~ "Fireplace",
                                      EQUIPM == 10 ~ "Portable electric heaters",
                                      EQUIPM == 21 ~ "Some other equipment",
                                      EQUIPM == -2 ~ NA)))

has_hp <- central_air_df %>%
  filter(`Heat Pump Status` == "Has a Heat Pump")

no_hp <- central_air_df %>%
  filter(`Heat Pump Status` == "No Heat Pump")

#PLOT HP status by income----
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
        axis.text.y=element_blank()) 

#BOXPLOT of income and yearly energy costs----
central_air_df["TotElectricity"] <- RECS2015$DOLLAREL

central_air_df <- outlierKD2(central_air_df, TotElectricity, rm= TRUE)

#PLOT income by total electricity usage-----
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
  labs(title = "Yearly Electricity Cost by Income Level ", 
       x = "Income level",
       y = "Yearly Electricity Cost (in $)")+  
  theme(axis.text.x = element_text(angle = 45, size = 9, margin = margin(r=0)), 
        axis.text.y=element_text())

#ANOVA Test (Income level)----
anovaInc = aov(TotElectricity ~ Income, data = central_air_df)
xkabledply(anovaInc, title = "ANOVA result summary")

#Spatial Prep------

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

#Electricity cost differences between urban and rural areas (“UATYP10” variable),

#rename levels of area 
RECS2015 <- RECS2015 %>% 
  mutate(UATYP10 = as.factor(case_when(UATYP10 == "U" ~ "Urban Area",
                                       UATYP10 == "R" ~ "Rural",
                                       UATYP10 == "C" ~ "Urban Cluster")))

#dataframe of area and total energy
Tot_Energy_area_df <- data.frame(RECS2015$UATYP10, RECS2015$DOLLAREL, RECS2015$DIVISION, RECS2015$CLIMATE_REGION_PUB)
colnames(Tot_Energy_area_df) <- c("Urban Density", "Yearly Electricity Costs", "Division", "Climate")

#HISTOGRAM and Q-Q Plot before outliers are removed----
ggplot(data=Tot_Energy_area_df, aes(x = `Yearly Electricity Costs`)) + 
  geom_histogram(breaks=seq(19, 8122, by = 100), 
                 col="black", 
                 fill="dark green", 
                 alpha = .7) + # opacity
  labs(x="Electricity Cost", y="Frequency") +
  labs(title="Histogram of Total Electricity Cost, Using `ggplot`")

qqnorm(Tot_Energy_area_df$`Yearly Electricity Costs`, main = "Q-Q Plot of Total Electricity Cost")
qqline(Tot_Energy_area_df$`Yearly Electricity Costs`)

### Removing outliers:
Tot_Energy_area_df <- outlierKD2(Tot_Energy_area_df, `Yearly Electricity Costs`, rm= TRUE)

#HISTOGRAM and Q-Q Plot again after removing outliers----

ggplot(data=Tot_Energy_area_df, aes(x = `Yearly Electricity Costs`)) + 
  geom_histogram(breaks=seq(19, 8122, by = 100), 
                 col="black", 
                 fill="dark green", 
                 alpha = .7) + # opacity
  labs(x="Electricity Cost", y="Frequency") +
  labs(title="Histogram of Total Electricity Cost, Using `ggplot`")

qqnorm(Tot_Energy_area_df$`Yearly Electricity Costs`, main = "Q-Q Plot of Total Electricity Cost")
qqline(Tot_Energy_area_df$`Yearly Electricity Costs`)

#PLOT Yearly expenditure differences by Division and Density-----
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

#ANOVA Test (Urban density)----
anovaUrb = aov(`Yearly Electricity Costs` ~ `Urban Density`, data = Tot_Energy_area_df)
xkabledply(anovaUrb, title = "ANOVA result summary")

anovaDiv = aov(`Yearly Electricity Costs` ~ Division, data = Tot_Energy_area_df)
xkabledply(anovaDiv, title = "ANOVA result summary")

#REGRESSION Heatpump with controls -----

house_size <- data.frame(RECS2015$CENACHP,RECS2015$DOLLAREL, log(RECS2015$DOLLAREL/RECS2015$TOTSQFT_EN),RECS2015$YEARMADERANGE, 
                         RECS2015$TOTSQFT_EN, log(RECS2015$TOTSQFT_EN), RECS2015$DOLELSPH, RECS2015$DOLELCOL,
                         RECS2015$CLIMATE_REGION_PUB, RECS2015$MONEYPY, RECS2015$NHSLDMEM, RECS2015$EDUCATION,
                         RECS2015$DOLLARNG, RECS2015$DOLLARFO, RECS2015$EQUIPM)

colnames(house_size) <- c("Heat Pump", 'Total Electricity', 'Log Total Electricity/sqft',"Year Made Range", 
                          "SqFoot", "Log SqFoot", "Heating Cost", "AC Cost",
                          "Climate Region", "Income", "Number of Household Members", "Education",
                          "Total Natural Gas Cost", "Total Fuel Oil/Kerosene Costs", "SH_Type")
#write_excel_csv(house_size,file = 'house_size.xlsx')
#house_size["Time"] <- sample(c(0,1), size = nrow(house_size), replace = TRUE)

house_size <- house_size %>%
  mutate(`Heat Pump` = case_when(`Heat Pump` == "No Heat Pump" ~ '0',
                                 `Heat Pump` == "Has a Heat Pump" ~ '1',
                                 `Heat Pump` == "NA" ~ "NA"))

house_size <- subset(house_size, house_size$SH_Type != "Heat pump", drop = FALSE)

house_size <- subset(house_size, !is.na(SH_Type), drop = FALSE)

house_size <- house_size[!grepl("NA", house_size$`Heat Pump`),]

house_size_cor <- cor(house_size[-c(9,10,12,15)])

corrplot.mixed(house_size_cor)

#Regressions

heatpump1 <- lm(`Log Total Electricity/sqft` ~ `SqFoot`+ `Heat Pump` + `Heat Pump`:`Log SqFoot`, data = house_size)

heatpump2 <- lm(`Log Total Electricity/sqft` ~ `SqFoot`+ `Heat Pump` + `Heat Pump`:`Log SqFoot`+`Climate Region`:`Heat Pump` + `Climate Region`, data = house_size)

heatpump3 <- lm(`Log Total Electricity/sqft` ~ `SqFoot`+ `Heat Pump` + `Heat Pump`:`Log SqFoot`+`Climate Region`:`Heat Pump` + `Climate Region` + 
                  `SH_Type`+ `Total Natural Gas Cost` + `Total Fuel Oil/Kerosene Costs`, data = house_size)

heatpump4 <- lm(`Log Total Electricity/sqft` ~ `SqFoot`+ `Heat Pump` + `Heat Pump`:`Log SqFoot`+`Climate Region`:`Heat Pump` +`Climate Region` + 
                  `SH_Type`+ `Total Natural Gas Cost` + `Total Fuel Oil/Kerosene Costs` +`Income` + `Education`, data = house_size)

xkabledply(anova(heatpump1, heatpump2, heatpump3, heatpump4))
xtable(anova(heatpump1, heatpump2, heatpump3, heatpump4))
xtable(heatpump4)

summary(heatpump3)
xtable(vif(heatpump3))

par(mfrow=c(2,2))
plot(heatpump3)

stargazer(heatpump1, heatpump2, heatpump3, title="Results", align=TRUE)

#Matrix of the predictor variables
X <- model.matrix(`Log Total Electricity/sqft` ~ `SqFoot`+ `Heat Pump` + `Heat Pump`:`Log SqFoot`+`Climate Region`:`Heat Pump` + `Climate Region` + 
                    `SH_Type`+ `Total Natural Gas Cost` + `Total Fuel Oil/Kerosene Costs`, data = house_size)

#Response variable
y <- house_size$`Log Total Electricity/sqft`

#Tuning lambda
cvfit <- cv.glmnet(X, y, alpha = 0, standardize = TRUE)

#PLOT RIDGE Regression Lambda ----
plot(cvfit)

#Optimal value of lambda fit
fit <- glmnet(X, y, alpha = 0, lambda = cvfit$lambda.min, standardize = TRUE)

coef(fit)
ty <- as.matrix(fit[[2]])
xtable(ty)

#PLOT Ridge Regression

fit <- glmnet::cv.glmnet(X, y)
autoplot(fit, colour = 'blue')

#Training matrix with lambda set
data_list <- list(X = X, y = y, lambda = cvfit$lambda.min)

#PLOT Final Regression Line------

ggplot(data = house_size,
       aes(x = `SqFoot`,
           y = `Log Total Electricity/sqft`,
           fill = `Heat Pump`)) +
  geom_point() +
  geom_smooth(method = "lm")

#--------------------------Mapping the data-------------------------------------
#Load USA states----

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
  mutate(region = as.factor(case_when(region == "connecticut" ~ "New England",
                                      region == "maine" ~ "New England",
                                      region == "massachusetts" ~ "New England",
                                      region == "new hampshire" ~ "New England",
                                      region == "rhode island" ~ "New England",
                                      region == "vermont" ~ "New England",
                                      region == "new jersey" ~ "Middle Atlantic",
                                      region == "new york" ~ "Middle Atlantic",
                                      region == "pennsylvania" ~ "Middle Atlantic",
                                      region == "indiana" ~ "East North Central",
                                      region == "illinois" ~ "East North Central",
                                      region == "michigan" ~ "East North Central",
                                      region == "ohio" ~ "East North Central",
                                      region == "wisconsin" ~ "East North Central",
                                      region == "iowa" ~ "West North Central",
                                      region == "kansas" ~ "West North Central",
                                      region == "minnesota" ~ "West North Central",
                                      region == "missouri" ~ "West North Central",
                                      region == "nebraska" ~ "West North Central",
                                      region == "north dakota" ~ "West North Central",
                                      region == "south dakota" ~ "West North Central",
                                      region == "delaware" ~ "South Atlantic",
                                      region == "district of columbia" ~ "South Atlantic",
                                      region == "florida" ~ "South Atlantic",
                                      region == "georgia" ~ "South Atlantic",
                                      region == "maryland" ~ "South Atlantic",
                                      region == "north carolina" ~ "South Atlantic",
                                      region == "south carolina" ~ "South Atlantic",
                                      region == "virginia" ~ "South Atlantic",
                                      region == "west virginia" ~ "South Atlantic",
                                      region == "alabama" ~ "East South Central",
                                      region == "kentucky" ~ "East South Central",
                                      region == "mississippi" ~ "East South Central",
                                      region == "tennessee" ~ "East South Central",
                                      region == "arkansas" ~ "West South Central",
                                      region == "louisiana" ~ "West South Central",
                                      region == "oklahoma" ~ "West South Central",
                                      region == "texas" ~ "West South Central",
                                      region == "arizona" ~ "Mountain",
                                      region == "colorado" ~ "Mountain",
                                      region == "idaho" ~ "Mountain",
                                      region == "new mexico" ~ "Mountain",
                                      region == "montana" ~ "Mountain",
                                      region == "utah" ~ "Mountain",
                                      region == "nevada" ~ "Mountain",
                                      region == "wyoming" ~ "Mountain",
                                      region == "alaska" ~ "Pacific",
                                      region == "california" ~ "Pacific",
                                      region == "hawaii" ~ "Pacific",
                                      region == "oregon" ~ "Pacific",
                                      region == "washington" ~ "Pacific",)))

#PLOT the US Census Divisions------
divisions_map <- ggplot(data = states) + 
  geom_polygon(aes(x = long, 
                   y = lat, 
                   fill = region, 
                   group = group), 
               color = "white") + 
  coord_fixed(1.3)

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

divisions_map + ditch_the_axes

# rename "region" column from `states` dataframe to match `RECS2015` column name
colnames(states) <- c("long", "lat", "group", "order", "DIVISION", "subregion")

#PLOT Climate v. Yearly Electricity----

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

#ANOVA Test Prep (Subset of Climates)----
HotDry_MixedDry <- Tot_Energy_area_df[Tot_Energy_area_df$Climate == "Hot-Dry/Mixed-Dry", ]
HotDry_MixedDry <- na.omit(HotDry_MixedDry)
Marine <- Tot_Energy_area_df[Tot_Energy_area_df$Climate == "Marine", ]
Marine <- na.omit(Marine)
Cold_VeryCold <- Tot_Energy_area_df[Tot_Energy_area_df$Climate == "Cold/Very Cold", ]
Cold_VeryCold <- na.omit(Cold_VeryCold)
Mixed_Humid <- Tot_Energy_area_df[Tot_Energy_area_df$Climate == "Mixed-Humid", ]
Mixed_Humid <- na.omit(Mixed_Humid)
Hot_Humid <- Tot_Energy_area_df[Tot_Energy_area_df$Climate == "Hot-Humid", ]
Hot_Humid <- na.omit(Hot_Humid)

# ANOVA Test Prep (Histogram of each Climate)
hist_HotDry_MixedDry <- ggplot(data=HotDry_MixedDry, aes(`Yearly Electricity Costs`)) + 
  geom_histogram(breaks=seq(18.7, 3354, by = 100),
                 col="black", 
                 fill="dark green", 
                 alpha = .7) + # opacity
  labs(x="Yearly Electricity Costs", y="Frequency") +
  labs(title="Hot-Dry/Mixed-Dry")

hist_Marine <- ggplot(data=Marine, aes(`Yearly Electricity Costs`)) + 
  geom_histogram(breaks=seq(146, 3271, by = 100),
                 col="black", 
                 fill="dark green", 
                 alpha = .7) + # opacity
  labs(x="Yearly Electricity Costs", y="Frequency") +
  labs(title="Marine")

hist_Cold_VeryCold <- ggplot(data=Cold_VeryCold, aes(`Yearly Electricity Costs`)) + 
  geom_histogram(breaks=seq(48, 3325, by = 100),
                 col="black", 
                 fill="dark green", 
                 alpha = .7) + # opacity
  labs(x="Yearly Electricity Costs", y="Frequency") +
  labs(title="Cold/Very Cold")

hist_Mixed_Humid <- ggplot(data=Mixed_Humid, aes(`Yearly Electricity Costs`)) + 
  geom_histogram(breaks=seq(219, 3355, by = 100),
                 col="black", 
                 fill="dark green", 
                 alpha = .7) + # opacity
  labs(x="Yearly Electricity Costs", y="Frequency") +
  labs(title="Mixed-Humid")

hist_Hot_Humid <- ggplot(data=Hot_Humid, aes(`Yearly Electricity Costs`)) + 
  geom_histogram(breaks=seq(60.5, 3355, by = 100),
                 col="black", 
                 fill="dark green", 
                 alpha = .7) + # opacity
  labs(x="Yearly Electricity Costs", y="Frequency") +
  labs(title="Hot-Humid")

grid.arrange(hist_HotDry_MixedDry, hist_Marine, hist_Cold_VeryCold, hist_Mixed_Humid, hist_Hot_Humid, ncol=3)

#ANOVA Test (Climate)----
anovaCli = aov(`Yearly Electricity Costs` ~ Climate, data = Tot_Energy_area_df)
xkabledply(anovaCli, title = "ANOVA result summary")

#ANOVA Test (Division)----
anovaDiv = aov(`Yearly Electricity Costs` ~ Division, data = Tot_Energy_area_df)
xkabledply(anovaDiv, title = "ANOVA result summary")

#Cross-tab of Divisions and Climates
xkabledply(table(Tot_Energy_area_df$Division, Tot_Energy_area_df$Climate), "Cross-Tab of Division and Climate")

#PLOT Stacked bar graph climates by US Census Division -----
Tot_Energy_area_df %>%
  mutate(Division = factor(Division, levels = c("Pacific","Mountain North","East North Central","West North Central", "Middle Atlantic", "New England", "Mountain South", "East South Central", "West South Central", "South Atlantic")))%>%
  ggplot(aes(fill=`Climate`, y="Percent", x=Division)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(title = "Census Division and Climate",
       ylab = "Percent") +
  theme(axis.text.x = element_text(angle = 45, size = 11, margin = margin(r=0)),
        axis.text.y=element_blank())

#PLOT Yearly Energy Expenditures by Census Division----
Tot_Energy_area_df %>%
  arrange(`Yearly Electricity Costs`)%>%
  mutate(Division = factor(Division, levels = c("Pacific","Mountain North","East North Central","West North Central", "Middle Atlantic", "New England", "Mountain South", "East South Central", "West South Central", "South Atlantic")))%>%
  ggplot(aes(x = Division,
             y = `Yearly Electricity Costs`,
             fill = Division)) +
  geom_boxplot() +
  labs(title = "Yearly Electricity Expenditure by Division") +
  theme(axis.text.x = element_blank())

#Load 2015 state median incomes----
conusmedinc_df <- read_xls("medianincome2015.xls")

conusmedinc_df <- conusmedinc_df[-c(1:3, 56:59), c(1, 10, 11)]
colnames(conusmedinc_df) <- c("State", "Median Income", "Std.Error")

conusmedinc_df <- conusmedinc_df %>%
  arrange(`Median Income`)

#IRA 80% threshold
conusmedinc_df["Threshold"] <- conusmedinc_df$`Median Income` * .8

conusmedinc_df <- conusmedinc_df[-31,]

statepop <- data.frame(statepop)
statepop <- statepop[-5]
test <- merge(x = conusmedinc_df, 
              y = statepop, 
              by.x = "State",
              by.y = "full")

#MAP IRA HP Subsidy----  
plot_usmap(data = test, 
           values = 'Threshold', 
           color = "blue") + 
  labs(title = "State Threshold for a Full IRA Heatpump Subsidy")+
  scale_fill_continuous(
    low = "white", 
    high = "blue", 
    name = "80% Median Income Threshold", 
    label = scales::comma) + theme(legend.position = "right",
                                   plot.title = element_text(hjust = 0.5))

test <- test %>%
  arrange(test$`Median Income`)

test$`Median Income` <- currency(test$`Median Income`, 
                                 symbol = "$",
                                 digits = 0L,
                                 format = "f",
                                 big.mark = ",",
                                 sep = "")

test$Threshold <- currency(test$Threshold,
                           symbol = "$",
                           digits = 0L,
                           format = "f",
                           big.mark = ",",
                           sep = "")

#print to LaTex
#print(xtable(test, type = "latex"), file = "filename2.tex")

#PLOT Climate v. Yearly electricity costs----
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

#PLOT Bar Plot of US Census v. Yearly Electricity Costs, Controlling for Climate----
level_order <- c("Mountain North", "Mountain South", "New England", "East South Central", "West North Central", "Middle Atlantic", "West South Central", "East North Central", "Pacific", "South Atlantic")
Tot_Energy_area_df$Division <- factor(Tot_Energy_area_df$Division, levels = level_order)

ggplot(Tot_Energy_area_df, aes(x=Division, 
                               y=`Yearly Electricity Costs`, 
                               color=Climate, 
                               fill = Climate)) +
  geom_bar(stat = "identity") +
  labs(x="US Census Division", y="Yearly Electricity Costs", title="Division vs. Electricity Cost, Colored by Climate") +
  theme(axis.text.x = element_text(angle = 45, size = 9, margin = margin(r=0)), 
        axis.text.y=element_text())

# Preparing Linear Models for climate and census division
lm.Division <- lm(`Yearly Electricity Costs`~Division, data = Tot_Energy_area_df)
lm.Climate <- lm(`Yearly Electricity Costs`~Climate, data = Tot_Energy_area_df)
lm.Division.Climate <- lm(`Yearly Electricity Costs`~Division+Climate, data = Tot_Energy_area_df)
lm.DivCli.interaction <- lm(`Yearly Electricity Costs`~Division + Climate + Division*Climate, data = Tot_Energy_area_df)

# Tables of LM comparisons, and ANOVA of LMs (Elect cost, climate, census division)
xkabledply(lm.Division, title = paste("Model (factor): ", format(formula(lm.Division))))
xkabledply(lm.Climate, title = paste("Model (factor): ", format(formula(lm.Climate))))
xkabledply(lm.Division.Climate, title = paste("Model (factor): ", format(formula(lm.Division.Climate))))
xkabledply(lm.DivCli.interaction)

anova.Tot_Area <- anova(lm.Division, lm.Climate, lm.Division.Climate, lm.DivCli.interaction)
#ANOVA test (which Regression is best) ----
xkabledply(anova.Tot_Area, title = "ANOVA comparison between the models")

#REGRESSION Regression of electricity costs controlling for income, urban area type -----

# Adding variables from RECS

Tot_Energy_area_df <- data.frame(RECS2015$UATYP10, RECS2015$DOLLAREL, RECS2015$DIVISION, RECS2015$CLIMATE_REGION_PUB, RECS2015$TOTROOMS, RECS2015$TOTSQFT_EN, RECS2015$MONEYPY)
colnames(Tot_Energy_area_df) <- c("Urban Density", "Yearly Electricity Costs", "Division", "Climate","Total Rooms", "SqFoot","Income")
Tot_Energy_area_df <- outlierKD2(Tot_Energy_area_df,`Yearly Electricity Costs` , rm =TRUE)

Tot_Energy_area_df$`Urban Density`<-as.factor(Tot_Energy_area_df$`Urban Density`)
Tot_Energy_area_df$Division<-as.factor(Tot_Energy_area_df$Division)
Tot_Energy_area_df$Climate<-as.factor(Tot_Energy_area_df$Climate)

## Linear Models for Income, Urban Density, and Division---

fit1 <- lm(`Yearly Electricity Costs`~Income + `Urban Density`, data =Tot_Energy_area_df)
fit2 <- lm(`Yearly Electricity Costs`~Income + `Urban Density`+ Division, data = Tot_Energy_area_df)
fit3 <- lm(`Yearly Electricity Costs`~Income + `Urban Density`+ Division +`Urban Density`*Division , data = Tot_Energy_area_df)

## Multicollinearity test
xkablevif(fit1)
xkablevif(fit2)
xkablevif(fit3)

## Plot for fitted model
plot(fit1)
plot(fit2)
plot(fit3)

#step(fit3)

#library(jtools)
#library(interactions)
#interact_plot(fit3, pred = `Urban Density`, modx = `Division`)

## Tables of LM comparisons, and ANOVA of LMs---
xkabledply(fit1, title = paste("Model 1:", format(formula(fit1)) ))
xkabledply(fit2, title = paste("Model 2 :", format(formula(fit2)) ))
xkabledply(fit3, title = paste("Model 3 (interactive):", format(formula(fit3))))

anovaRes<-anova(fit1,fit2,fit3)
xkabledply(anovaRes, title = "ANOVA comparison between the models")

#STEPWISE Regression (Full Model)
#REGRESSION TREE----

loadPkg("ISLR")
loadPkg("tree")
loadPkg("rpart")
loadPkg("rpart.plot")

treefit <- tree(log(`Yearly Electricity Costs`) ~ `Income` + `Urban Density` + `Division` + `Climate` + `Total Rooms` + `SqFoot`, data = Tot_Energy_area_df)
# renaming columns for the formula
names(Tot_Energy_area_df) <- c("Urban_Density", "Yearly_Electricity_Costs", "Division", "Climate", "Total_Rooms", "SqFoot", "Income")

# tree regressions
treefit <- tree(log(Yearly_Electricity_Costs) ~ Income + Urban_Density + Division + Climate + Total_Rooms + SqFoot, data = Tot_Energy_area_df)
summary(treefit)
plot(treefit)
text(treefit,cex=0.75)

treefitRpart <- rpart(log(Yearly_Electricity_Costs) ~ Income + Urban_Density + Division + Climate + Total_Rooms +SqFoot, data=Tot_Energy_area_df, control = list(maxdepth = 8, cp=0.009) )
summary(treefitRpart)
fancyRpartPlot(treefitRpart, cex=0.9)
