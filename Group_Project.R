USAcc <- read.csv("US_Accidents_Dec20.csv")
head(USAcc$End_Lat)
str(USAcc)
summary(USAcc)
sum(is.na(USAcc$Civil_Twilight))
is.null(USAcc$Civil_Twilight)
unique(USAcc$County)
tail(USAcc$County)
lapply(USAcc6,function(x) { length(which(is.na(x)))})
USAcc[,12] <- lapply(X = USAcc[,12], FUN = factor)
is.na.data.frame(USAccx)
??map
summary(USAcc$Start_Time)
install.packages(tidyverse)
library(maptools)
library(maps)
library(mapdata)
library(dbplyr)
library(car)
library(leaps)
install.packages("leaps")

map("usa", fill = TRUE, col = "white", bg = "lightblue")
points(USAcc$Start_Lng, USAcc$Start_Lat, pch = 20, cex  = 0.01, col = "red")

length(USAcc$ID)
unique(USAcc$ID)
length(unique(USAcc$ID))

d <- data.frame(date = c("2017-02-23", "2017-02-22"))
separate(d, "date", c("Year", "Month", "Day"), sep = "-")

??separate
head(USAcc$Start_Time)
stamp <-separate(head(USAcc$Start_Time), "date", c("Year", "Month", "Day"), sep = "-") 
as.numeric(USAcc$Start_Time)
summary(USAcc$Start_Time)
as.Date.numeric(USAcc$Start_Time)
strptime(USAcc$Start_Time ,"%Y-%m-%d %H:%M:%S")
strptime(as.character(USAcc$Start_Time),"%Y-%m-%d %H:%M:%S")
separate(USAcc,"Start_Time", c("date", "time"), sep = " ")
head(separate(USAcc,"Start_Time", c("date", "time"), sep = " "))
tail(USAcc$Start_Time)
max(USAcc$Start_Time)
unique(USAcc$Wind_Direction)
is
76384/4232541
98668/4232541
2065589/4232541
unique(USAcc$Weather_Condition)
unique(USAcc6$Civil_Twilight)
length(unique(USAcc6$Civil_Twilight))
count(USAcc$Civil_Twilight, "Night")
lapply(USAcc,function(x){ length(unique(x))})
Civil_Twilight <-table(USAcc$Civil_Twilight)
Civil_Twilight
Sunrise_Sunset <-table(USAcc$Sunrise_Sunset)
Sunrise_Sunset
Nautical <-table(USAcc$Nautical_Twilight)
Nautical
Astronomical <-table(USAcc$Nautical_Twilight)
Astronomical
count_side <- count(USAcc$Side)
count_side
rf_side <- count_side_final$freq/sum(count_side_final$freq)
rf_side
count_side_final <- count_side[-1,]
barplot(rf_side, names.arg = count_side_final$x, main = "Relative Frequency by Accident Side", axes=TRUE)
?barplot

Side <-table(USAcc$Side)
Side
Sour <-table(USAcc$Source)
Sour

Duration = as.numeric(difftime(USAcc$End_Time, USAcc$Start_Time, units = 'hours'))
USAcc$Duration = Duration
summary(Duration)
max(Duration)/24
plot(Duration)
count_Duration <- count(USAcc$Duration)
count_Duration
plot(count_Duration)
USAcc1 <- USAcc[,-c(1,2,3,9,10,12,13,20, 21,22,23,25, 30,31,45, 46,48,49 )]
is.null(USAcc1)
summary(USAcc1)
USAcc2 <- USAcc1[!is.na(USAcc1$Temperature.F.),]
summary(USAcc2$Temperature.F.)
USAcc2 <-USAcc1[,-c(2,3)]
USAcc3 <- USAcc2[!is.na(USAcc2$Humidity...),]

summary(USAcc3$Humidity...)
USAcc4 <- USAcc3[!is.na(USAcc3$Pressure.in.),]
summary(USAcc4$Pressure.in.)
USAcc5 <- USAcc4[!is.na(USAcc4$Visibility.mi.),]
summary(USAcc5$Visibility.mi.)
USAcc6 <- USAcc5[!is.na(USAcc5$Duration),]
summary(USAcc6$Duration)

unique(USAcc8$Wind_Direction)
USAcc7 <- USAcc6[!USAcc6$Civil_Twilight== '',]

USAcc8 <- USAcc7[!USAcc7$Wind_Direction== 'NA',]
USAcc9 <- USAcc8[!USAcc8$Wind_Direction %in% '',]
unique(USAcc9$Wind_Direction)
USAcc10 <- USAcc9[!USAcc9$Zipcode %in% '',]
unique(USAcc10$Zipcode)
sum(is.na(USAcc10$Zipcode))

USAcc11 <- USAcc10[!USAcc10$Weather_Condition %in% '',]
unique(USAcc11$Weather_Condition)

summary(USAcc11$City)
unique(USAcc11$Side)
sum(is.na(USAcc12$City))
sum(USAcc12$Zipcode %in% 'null')
USAcc12 <- USAcc11[!USAcc11$Side %in% ' ',]
unique(USAcc12$Street)
length(USAcc12$Zipcode)
unique(USAcc12$Zipcode)
summary(USAcc12)

write.csv(USAcc12, "USAcc12.csv")

USAcc12 <- read.csv("USAcc12.csv")
unique(USAcc12$Wind_Direction)
WindDirection <-table(USAcc12$Wind_Direction)
WindDirection
(110090+85963)/4015359
summary(USAcc12)
WindDir <- data.frame(USAcc12$Wind_Direction)
WindDir<- as.factor(WindDir)
#Create a vector of all the categorical variables
Cats <- c("Wind_Direction", "")
#Convert individual variable as factor
WindDir<- as.factor(WindDir)
USAcc12$Wind_Direction <- as.character(USAcc12$Wind_Direction)

# We can use lapply() to convert all of our
# facs variables to factors
cs[ ,facs] <- lapply(X = cs[ , facs], 
                     FUN = factor)

USAcc12$wind_Dir_coded <- replace(USAcc12$Wind_Direction, "North", 'N')

max(Accident_PA$y1)
max(Accident_PA$y)

USAcc13 <- data.frame(USAcc13)
WindDirection <-table(USAcc12$Wind_Direction)
WindDirection

Weather_cond <-table(USAcc12$Weather_Condition)
Weather_cond
wc <- as.data.frame(Weather_cond)
write.csv(wc, "wc.csv")
unique(USAcc13$Wind_Direction)
sum(USAcc12$Wind_Direction %in% '')

USAcc12$Wind_Direction[USAcc12$Wind_Direction == "North"] <- "N"
unique(USAcc12$Wind_Direction)

#############################################################################
#Data Pre-Processing
#Dataset obtained from cleaning USAcc12
USAcc13 <- read.csv("USAcc13.csv")

#Wind_Direction variable
USAcc13<- USAcc12
USAcc13$Wind_Direction[USAcc12$Wind_Direction == "North"] <- "N"
USAcc13$Wind_Direction[USAcc12$Wind_Direction == "VAR"] <- "Variable"
USAcc13$Wind_Direction[USAcc12$Wind_Direction == "West"] <- "W"
USAcc13$Wind_Direction[USAcc12$Wind_Direction == "South"] <- "S"
USAcc13$Wind_Direction[USAcc12$Wind_Direction == "East"] <- "E"
USAcc13$Wind_Direction[USAcc12$Wind_Direction == "CALM"] <- "Calm"
unique(Accident1$Wind_Direction)

#Duration Variable #Zalon
USAcc13$Duration_exp <-exp(-1*USAcc13$Duration)

#Street variable converted to Highway/City
USAcc13 <- USAcc13 %>% mutate(Road_type = case_when(
    str_detect(Street, "Fwy|Expy|Highway|US-|I-") ~ "Highway",
  !str_detect(Street, "Fwy|Expy|Highway|US-|I-") ~ "City")) %>% 
  mutate(Road_type = as.factor(Road_type)) 

USAcc13 <-USAcc13 %>% mutate
        (Road_Type = case_when(str_detect(USAcc13$Street,"Fwy|Expy|Highway|US-|I-")~"Highway",
                               !str_detect(USAcc13$Street, "Fwy|Expy|Highway|US-|I-") ~ "City"
                               )
          )%>% mutate(Road_Type = as.factor(Road_Type))

USAcc13$Road_type <- Road_Type

#Converting Dummy variables
library(caret)
write.csv(USAcc13, "USAcc13.csv")
USAcc14 <- read.csv(file = "USAcc13.csv",
                stringsAsFactors = TRUE)
USAcc14$City <- as.character(USAcc14$City)
USAcc14$County <- as.character(USAcc14$County)
USAcc14$State <- as.character(USAcc14$State)
USAcc14$Zipcode<- as.character(USAcc14$Zipcode)

USAcc_f<-USAcc14
remove(USAcc15)

USAcc15 <- dummyVars(formula = ~ Road_type, data = USAcc14)
USAcc15_dums <- predict (object = USAcc15, newdata = USAcc14)
USAcc16 <- dummyVars(formula = ~ Civil_Twilight, data = USAcc14)
USAcc16_dums <- predict (object = USAcc16, newdata = USAcc14)
USAcc17 <- dummyVars(formula = ~ Traffic_Signal, data = USAcc14)
USAcc17_dums <- predict (object = USAcc17, newdata = USAcc14)
USAcc18 <- dummyVars(formula = ~ Traffic_Calming, data = USAcc14)
USAcc18_dums <- predict (object = USAcc18, newdata = USAcc14)
USAcc19 <- dummyVars(formula = ~ Stop, data = USAcc14)
USAcc19_dums <- predict (object = USAcc19, newdata = USAcc14)
USAcc20 <- dummyVars(formula = ~ Station, data = USAcc14)
USAcc20_dums <- predict (object = USAcc20, newdata = USAcc14)
USAcc21 <- dummyVars(formula = ~ Roundabout, data = USAcc14)
USAcc21_dums <- predict (object = USAcc21, newdata = USAcc14)
USAcc22 <- dummyVars(formula = ~ Railway, data = USAcc14)
USAcc22_dums <- predict (object = USAcc22, newdata = USAcc14)
USAcc23 <- dummyVars(formula = ~ No_Exit, data = USAcc14)
USAcc23_dums <- predict (object = USAcc23, newdata = USAcc14)
USAcc24 <- dummyVars(formula = ~ Junction, data = USAcc14)
USAcc24_dums <- predict (object = USAcc24, newdata = USAcc14)
USAcc25 <- dummyVars(formula = ~ Give_Way, data = USAcc14)
USAcc25_dums <- predict (object = USAcc25, newdata = USAcc14)
USAcc26 <- dummyVars(formula = ~ Crossing, data = USAcc14)
USAcc26_dums <- predict (object = USAcc26, newdata = USAcc14)
USAcc27 <- dummyVars(formula = ~ Bump, data = USAcc14)
USAcc27_dums <- predict (object = USAcc27, newdata = USAcc14)
USAcc28 <- dummyVars(formula = ~ Amenity, data = USAcc14)
USAcc28_dums <- predict (object = USAcc28, newdata = USAcc14)
USAcc29 <- dummyVars(formula = ~ Side, data = USAcc14)
USAcc29_dums <- predict (object = USAcc29, newdata = USAcc14)

USAcc_f <- data.frame(USAcc_f, USAcc15_dums,USAcc16_dums, USAcc17_dums,USAcc18_dums,
                        USAcc19_dums,USAcc20_dums, USAcc21_dums,USAcc22_dums,
                        USAcc23_dums,USAcc24_dums, USAcc25_dums,USAcc26_dums,
                        USAcc27_dums,USAcc28_dums, USAcc29_dums)
USAcc_dum <-USAcc_f
write.csv(USAcc_dum, "USAcc_dum.csv")
USAcc_dum1 <-USAcc_dum

USAcc_dum1$Weather_Condition <- as.character(USAcc_dum1$Weather_Condition)

USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Fair","Fair / Windy")] <- "Fair"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Clear")] <- "Clear"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Mostly Cloudy","Mostly Cloudy / Windy","Overcast","Cloudy","Cloudy / Windy","Funnel Cloud","N/A Precipitation")] <- "Cloudy"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Partly Cloudy","Partly Cloudy / Windy","Scattered Clouds")] <- "Partly cloudy"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Light Drizzle","Light Freezing Drizzle","Light Drizzle / Windy")] <- "Light Drizzle"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Light Rain","Drizzle","Light Rain / Windy","Showers in the Vicinity","Light Rain Showers","Light Rain Shower","Light Rain Shower / Windy","Drizzle / Windy")] <- "Light Rain"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Rain","Rain / Windy","Heavy Drizzle","Rain Showers","Rain Shower")] <- "Rain"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Heavy Rain","Heavy Rain / Windy","Heavy Rain Showers","Heavy Rain Shower")] <- "Heavy Rain"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Light Snow","Light Snow / Windy","Light Snow Showers","Light Snow with Thunder","Light Snow Grains","Low Drifting Snow","Light Snow Shower","Light Blowing Snow")] <- "Light Snow"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Snow","Blowing Snow","Snow / Windy","Blowing Snow / Windy","Snow Grains","Snow Showers","Drifting Snow")] <- "Snow"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Heavy Snow","Heavy Snow / Windy","Heavy Snow with Thunder","Heavy Blowing Snow","Thunderstorms and Snow")] <- "Heavy Snow"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Haze","Fog","Patches of Fog","Shallow Fog","Light Freezing Fog","Haze / Windy","Drizzle and Fog","Fog / Windy","Partial Fog","Light Haze","Patches of Fog / Windy","Light Fog","Partial Fog / Windy","Mist")] <- "Fog"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Smoke","Smoke / Windy","Heavy Smoke")] <- "Smoke"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("T-Storm","Thunderstorm","Thunderstorms and Rain","T-Storm / Windy","Snow and Thunder","Thunder and Hail","Thunder and Hail / Windy","Squalls / Windy","Squalls")] <- "Thunderstorm"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Light Thunderstorms and Rain","Light Rain with Thunder","Light Thunderstorms and Snow","Light Thunderstorm")] <- "Light Thunderstorm"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Heavy T-Storm","Heavy Thunderstorms and Rain","Heavy T-Storm / Windy","Heavy Thunderstorms with Small Hail","Heavy Thunderstorms and Snow")] <- "Heavy Thunderstorm"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Thunder in the Vicinity","Thunder","Thunder / Windy")] <- "Thunder"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Wintry Mix","Wintry Mix / Windy","Thunder / Wintry Mix","Thunder / Wintry Mix / Windy")] <- "Wintry Mix"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Widespread Dust","Blowing Dust / Windy","Blowing Dust","Sand / Dust Whirlwinds","Sand / Dust Whirlwinds / Windy","Blowing Sand","Sand / Dust Whirls Nearby","Widespread Dust / Windy")] <- "Dust"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Small Hail","Light Ice Pellets","Ice Pellets","Snow and Sleet","Snow and Sleet / Windy","Light Snow and Sleet","Sleet","Freezing Rain","Light Sleet","Light Freezing Rain / Windy","Light Snow and Sleet / Windy","Heavy Sleet","Heavy Freezing Rain","Heavy Ice Pellets","Light Sleet / Windy","Freezing Drizzle","Light Hail","Hail","Heavy Freezing Drizzle","Rain and Sleet","Freezing Rain / Windy","Sleet / Windy","Light Freezing Rain")] <- "Hail"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Volcanic Ash")] <- "Volcanic Ash"
USAcc_dum1$Weather_Condition[USAcc_dum1$Weather_Condition  %in% c("Tornado")] <- "Tornado"

unique(USAcc_dum1$Weather_Condition)

USAcc_dum1$Weather_Condition <- as.factor(USAcc_dum1$Weather_Condition)

USAcc30 <- dummyVars(formula = ~ Weather_Condition, data = USAcc_dum1)
USAcc30_dums <- predict (object = USAcc30, newdata = USAcc_dum1)

USAcc_dum1 <-data.frame(USAcc_dum1, USAcc30_dums)
names(USAcc_dum1)
USAcc_dum2 <-USAcc_dum1

USAcc31 <- dummyVars(formula = ~ Wind_Direction, data = USAcc_dum2)
USAcc31_dums <- predict (object = USAcc31, newdata = USAcc_dum2)

USAcc_dum3 <-data.frame(USAcc_dum2, USAcc31_dums)

write.csv(USAcc_dum3, "USAcc_dum3.csv")
names(USAcc_dum3)
USAccdum4 <- USAcc_dum3[,-c(1,2)]
names(USAccdum4)
USAccdum5 <- USAccdum4[,-c(6,15:29)]
names(USAccdum5)
USAccdum6 <- USAccdum5[,-c(16)]
write.csv(USAccdum6, "Accident.csv")

Accident <- read.csv("Accident.csv")
Accident$Zipcode <- substr(Accident$Zipcode, 1, 5)
Accident1<- Accident1[,-1]

USAcc32 <- dummyVars(formula = ~ Severity, data = Accident1)
USAcc32_dums <- predict (object = USAcc32, newdata = Accident1)

Accident1$Severity <- as.factor(Accident1$Severity)
Accident2 <- data.frame(Accident1, USAcc32_dums)

unique(Accident1$Severity)
unique(USAcc13$Wind_Direction)
WindDirection <-table(USAcc13$Wind_Direction)
min(WindDirection)

write.csv(Accident2, "Accident_Final.csv", row.names = FALSE)

###############################################################################
#Regression Analysis
Accident2  <- Accident2[,-1]

Accident_Final <- Accident_Final %>% rename(
  y1 = Duration,
  y =  Duration_exp,
  x2 =  Start_Lat,
  x3 =  Start_Lng,
  x4 = Distance.mi.,
  x5 =  Street,
  x6 =  City,
  x7 =  County,
  x8 = State,
  x9 =  Zipcode,
  x10 =  Temperature.F.,
  x11 = Humidity...,
  x12 =  Pressure.in.,
  x13 =  Visibility.mi.,
  x14 =  Road_type.City,
  x15 = Road_type.Highway,
  x16 =  Civil_Twilight.Day,
  x17 = Civil_Twilight.Night,
  x18 =  Traffic_Signal.False,
  x19 = Traffic_Signal.True,
  x20 =  Traffic_Calming.False,
  x21 = Traffic_Calming.True,
  x22 =  Stop.False,
  x23 =  Stop.True,
  x24 = Station.False,
  x25 =  Station.True,
  x26 = Roundabout.False,
  x27 =  Roundabout.True,
  x28 =  Railway.False,
  x29 = Railway.True,
  x30 =  No_Exit.False,
  x31 =  No_Exit.True,
  x32 = Junction.False,
  x33 =  Junction.True,
  x34 =  Give_Way.False,
  x35 = Give_Way.True,
  x36 =  Crossing.False,
  x37 =  Crossing.True,
  x38 = Bump.False,
  x39 =  Bump.True,
  x40 =  Amenity.False,
  x41 = Amenity.True,
  x42 =  Side.L,
  x43 =  Side.R,
  x44 = Weather_ConditionClear,
  x45 =  Weather_ConditionCloudy,
  x46 = Weather_ConditionDust,
  x47 =  Weather_ConditionFair,
  x48 = Weather_ConditionFog,
  x49 =  Weather_ConditionHail,
  x50 = Weather_ConditionHeavy.Rain,
  x51 =  Weather_ConditionHeavy.Snow,
  x52 = Weather_ConditionHeavy.Thunderstorm,
  x53 =  Weather_ConditionLight.Drizzle,
  x54 = Weather_ConditionLight.Rain,
  x55 =  Weather_ConditionLight.Snow,
  x56 = Weather_ConditionLight.Thunderstorm,
  x57 = Weather_ConditionPartly.cloudy,
  x58 = Weather_ConditionRain,
  x59 =  Weather_ConditionSmoke,
  x60 = Weather_ConditionSnow,
  x61 =  Weather_ConditionThunder,
  x62 = Weather_ConditionThunderstorm,
  x63 =  Weather_ConditionTornado,
  x64 = Weather_ConditionVolcanic.Ash,
  x65 =  Weather_ConditionWintry.Mix,
  x66 = Wind_Direction.Calm,
  x67 =  Wind_Direction.E,
  x68 =  Wind_Direction.ENE,
  x69 = Wind_Direction.ESE,
  x70 =  Wind_Direction.N,
  x71 = Wind_Direction.NE,
  x72 = Wind_Direction.NNE,
  x73 =  Wind_Direction.NNW,
  x74 =  Wind_Direction.NW,
  x75 = Wind_Direction.S,
  x76 = Wind_Direction.SE,
  x77 =  Wind_Direction.SSE,
  x78 = Wind_Direction.SSW,
  x79 =  Wind_Direction.SW,
  x80 = Wind_Direction.Variable,
  x81 =  Wind_Direction.W,
  x82 = Wind_Direction.WNW,
  x83 = Wind_Direction.WSW,
  x84 = Severity.1,
  x85 = Severity.2,
  x86 = Severity.3,
  x87 = Severity.4
)

Accident1<- Accident1[,-1]

write.csv(Accident_Final, "Accident_Final.csv", row.names = FALSE)
###########################################################################################################
#Load the datatset
Accident_Final <- read.csv("Accident_Final.csv")
#Subset PA dataset
Accident_PA <- Accident_Final[Accident_Final$x8=='PA',]
unique(Accident_PA$x8)
names(Accident_PA)
reg_PA_empty<- lm(y~1, Accident_PA)
#Step 2: Run full regression
reg_PA_full<- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x27+x29+x31+x33+x35+
                   x37+x39+x41+x43+x44+x45+x46+x47+x48+x49+x50+x51+x52+x53+x54+x55+x56+x57+
                   x58+x59+x60+x61+x62+x63+x64+x66+x67+x68+x70+x71+x72+x73+x74+x75+x76+x77+
                   x78+x79+x80+x81+x82+x83+x84+x85+x86, Accident_PA)
summary(reg_PA_full)
#sample_reg <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x27+x29+x31+x33+x35+
 #                  x37+x39+x41+x43+x44+x45+x46+x47+x48+x49+x50+x51+x52+x53+x54+x55+x56+x57+
  #                 x58+x59+x60+x61+x62+x63+x64+x66+x67+x68+x70+x71+x72+x73+x74+x75+x76+x77+
   #                x78+x79+x80+x81+x82+x83+x84+x85+x86, Accident_PA_Keep)
#summary(sample_reg)

#No_Keep <- c('x5',	'x6',	'x7',	'x8',	'x9',	'x14',	'x16',	'x18',	'x20',	'x22',	'x24',	'x26',	'x28',	'x30',	'x32',	'x34',	'x36',	'x38',	'x40',	'x42',	'x65',	'x69',	'x87')
#Keep <- names(No_Keep)[!names(No_Keep)]
Accident_PA_NoKeep <- Accident_PA[,c(4,	5,	6,	7,	8,	15,	17,	19,	21,	23,	25,	27,	29,	31,	33,	35,	37,	39,	41,	43,	66,	70,	88)]
Accident_PA_Keep <- Accident_PA[,c(1,	2,	3,	9,	10,	11,	12,	13,	14,	16,	18,	20,	22,	24,	26,	28,	30,	32,	34,	36,	38,	40,	42,	44,	45,	46,	47,	48,	49,	50,	51,	52,	53,	54,	55,	56,	57,	58,	59,	60,	61,	62,	63,	64,	65,	67,	68,	69,	71,	72,	73,	74,	75,	76,	77,	78,79,	80,	81,	82,	83,	84,	85,	86,	87)]
#Step 3: check for alias and remove the variable
alias(reg_PA_full)
#Remove the variables x27, x46, x63, x64
reg_PA_minus_alias<- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x31+x33+x35+
                   x37+x39+x41+x43+x44+x45+x47+x48+x49+x50+x51+x52+x53+x54+x55+x56+x57+
                   x58+x59+x60+x61+x62+x66+x67+x68+x70+x71+x72+x73+x74+x75+x76+x77+
                   x78+x79+x80+x81+x82+x83+x84+x85+x86, Accident_PA)
summary(reg_PA_minus_alias)

alias(reg_PA_minus_alias)
vif(reg_PA_minus_alias)
#Step 4: Splitting the dataset
#sub_PA <- createDataPartition(y = Accident_PA$y, p = 0.70, list = FALSE)
  # We will create 2 dataframes - one training and one testing
#train_PA <- Accident_PA[sub_PA, ]
#test_PA <- Accident_PA[-sub_PA, ]

sub_PA_new <- createDataPartition(y = Accident_PA_Keep$y, p = 0.70, list = FALSE)
train_PA_new <- Accident_PA_Keep[sub_PA_new, ]
test_PA_new <- Accident_PA_Keep[-sub_PA_new, ]




#Step 6:Run the regression for training dataset
Model1 <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x31+x33+x35+
               x37+x39+x41+x43+x44+x45+x47+x48+x49+x50+x51+x52+x53+x54+x55+x56+x57+
               x58+x59+x60+x61+x62+x66+x67+x68+x70+x71+x72+x73+x74+x75+x76+x77+
               x78+x79+x80+x81+x82+x83+x84+x85+x86, train_PA_new)
summary(Model1)
alias(Model1)
  #After running regression on training dataset, error "there are aliased coefficients in the model"
  #Remove the variable causing alias x59

Model1A <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x31+x33+x35+
               x37+x39+x41+x43+x44+x45+x47+x48+x49+x50+x51+x52+x53+x54+x55+x56+x57+
               x58+x60+x61+x62+x66+x67+x68+x70+x71+x72+x73+x74+x75+x76+x77+
               x78+x79+x80+x81+x82+x83+x84+x85+x86, train_PA_new)
summary(Model1A)
alias(Model1A)
#Step 7 : Check for VIF, Remove the onewith  highest VIF and Check CD and remove the highest CD observation
vif(Model1A)
max(vif(Model1A))
  #Remove the variable x45 with highest VIF as 186.2465
  #check the cook's distance'
cooks.distance(Model1A)
max(cooks.distance(Model1A))
min(cooks.distance(Model1A))
  #All the values are in the range of -1 to 1, hence there is no outlier



Model1A1 <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x31+x33+x35+
                x37+x39+x41+x43+x44+x47+x48+x49+x50+x51+x52+x53+x54+x55+x56+x57+
                x58+x60+x61+x62+x66+x67+x68+x70+x71+x72+x73+x74+x75+x76+x77+
                x78+x79+x80+x81+x82+x83+x84+x85+x86, train_PA_new)
summary(Model1A1)
alias(Model1A1)
vif(Model1A1)
max(vif(Model1A1))
  #After removing one varable now max VIF is 7.143425 for x66, not removing as of now.

#Step 10: Check for p-values and remove non-significant variables one by one

  #VAriable x62 has highest p-value as 0.846980, remove this variable and run the regression
Model2 <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x31+x33+x35+
               x37+x39+x41+x43+x44+x47+x48+x49+x50+x51+x52+x53+x54+x55+x56+x57+
               x58+x60+x61+x66+x67+x68+x70+x71+x72+x73+x74+x75+x76+x77+
               x78+x79+x80+x81+x82+x83+x84+x85+x86, train_PA_new)
summary(Model2)

  #VAriable x55 has highest p-value as 0.749782, remove this variable and run the regression 
Model2A <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x31+x33+x35+
                x37+x39+x41+x43+x44+x47+x48+x49+x50+x51+x52+x53+x54+x56+x57+
                x58+x60+x61+x66+x67+x68+x70+x71+x72+x73+x74+x75+x76+x77+
                x78+x79+x80+x81+x82+x83+x84+x85+x86, train_PA_new)
summary(Model2A)

  #VAriable x50 has highest p-value as 0.623898, remove this variable and run the regression 
Model2B <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x31+x33+x35+
                x37+x39+x41+x43+x44+x47+x48+x49+x51+x52+x53+x54+x56+x57+
                x58+x60+x61+x66+x67+x68+x70+x71+x72+x73+x74+x75+x76+x77+
                x78+x79+x80+x81+x82+x83+x84+x85+x86, train_PA_new)
summary(Model2B)

  #VAriable x37 has highest p-value as 0.323153, remove this variable and run the regression 
Model2C <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x31+x33+x35+
              x39+x41+x43+x44+x47+x48+x49+x51+x52+x53+x54+x56+x57+
                x58+x60+x61+x66+x67+x68+x70+x71+x72+x73+x74+x75+x76+x77+
                x78+x79+x80+x81+x82+x83+x84+x85+x86, train_PA_new)
summary(Model2C)

  #VAriable x31 has highest p-value as 0.298451, remove this variable and run the regression 
Model2D <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+
                x39+x41+x43+x44+x47+x48+x49+x51+x52+x53+x54+x56+x57+
                x58+x60+x61+x66+x67+x68+x70+x71+x72+x73+x74+x75+x76+x77+
                x78+x79+x80+x81+x82+x83+x84+x85+x86, train_PA_new)
summary(Model2D)

  #VAriable x78 has highest p-value as 0.298169, remove this variable and run the regression 
Model2E <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+
                x39+x41+x43+x44+x47+x48+x49+x51+x52+x53+x54+x56+x57+
                x58+x60+x61+x66+x67+x68+x70+x71+x72+x73+x74+x75+x76+x77+
                x79+x80+x81+x82+x83+x84+x85+x86, train_PA_new)
summary(Model2E)

#VAriable x52 has highest p-value as 0.272505, remove this variable and run the regression 
Model2F <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+
                x39+x41+x43+x44+x47+x48+x49+x51+x52+x53+x54+x56+x57+
                x58+x60+x61+x66+x67+x68+x70+x71+x72+x73+x74+x75+x76+x77+
                x79+x80+x81+x82+x83+x84+x85+x86, train_PA_new)
summary(Model2F)

  #VAriable x74 has highest p-value as 0.912016, remove this variable and run the regression 
Model2F <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+
                x39+x41+x43+x44+x47+x48+x49+x51+x52+x53+x54+x56+x57+
                x58+x60+x61+x66+x67+x68+x70+x71+x72+x73+x75+x76+x77+
                x79+x80+x81+x82+x83+x84+x85+x86, train_PA_new)
summary(Model2F)

#VAriable x68 has highest p-value as 0.490027, remove this variable and run the regression 
Model2G <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+
                x39+x41+x43+x44+x47+x48+x49+x51+x52+x53+x54+x56+x57+
                x58+x60+x61+x66+x67+x70+x71+x72+x73+x75+x76+x77+
                x79+x80+x81+x82+x83+x84+x85+x86, train_PA_new)
summary(Model2G)

#VAriable x73 has highest p-value as 0.557493, remove this variable and run the regression 
Model2H <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+
                x39+x41+x43+x44+x47+x48+x49+x51+x52+x53+x54+x56+x57+
                x58+x60+x61+x66+x67+x70+x71+x72+x75+x76+x77+
                x79+x80+x81+x82+x83+x84+x85+x86, train_PA_new)
summary(Model2H)

#VAriable x67 has highest p-value as 0.334083, remove this variable and run the regression 
Model2I <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+
                x39+x41+x43+x44+x47+x48+x49+x51+x52+x53+x54+x56+x57+
                x58+x60+x61+x66+x70+x71+x72+x75+x76+x77+
                x79+x80+x81+x82+x83+x84+x85+x86, train_PA_new)
summary(Model2I)

#VAriable x52 has highest p-value as 0.296129, remove this variable and run the regression 
Model2J <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+
                x39+x41+x43+x44+x47+x48+x49+x51+x53+x54+x56+x57+
                x58+x60+x61+x66+x70+x71+x72+x75+x76+x77+
                x79+x80+x81+x82+x83+x84+x85+x86, train_PA_new)
summary(Model2J)

#VAriable x81 has highest p-value as 0.249561, remove this variable and run the regression 
Model2k <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+
                x39+x41+x43+x44+x47+x48+x49+x51+x53+x54+x56+x57+
                x58+x60+x61+x66+x70+x71+x72+x75+x76+x77+
                x79+x80+x82+x83+x84+x85+x86, train_PA_new)
summary(Model2k)

#VAriable x56 has highest p-value as 0.202572, remove this variable and run the regression 
Model2L <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+
                x39+x41+x43+x44+x47+x48+x49+x51+x53+x54+x57+
                x58+x60+x61+x66+x70+x71+x72+x75+x76+x77+
                x79+x80+x82+x83+x84+x85+x86, train_PA_new)
summary(Model2L)

#VAriable x76 has highest p-value as 0.185580, remove this variable and run the regression 
Model2M <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+
                x39+x41+x43+x44+x47+x48+x49+x51+x53+x54+x57+
                x58+x60+x61+x66+x70+x71+x72+x75+x77+
                x79+x80+x82+x83+x84+x85+x86, train_PA_new)
summary(Model2M)

#VAriable x53 has highest p-value as 0.170125, remove this variable and run the regression 
Model2N <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+
                x39+x41+x43+x44+x47+x48+x49+x51+x54+x57+
                x58+x60+x61+x66+x70+x71+x72+x75+x77+
                x79+x80+x82+x83+x84+x85+x86, train_PA_new)
summary(Model2N)

#VAriable x82 has highest p-value as 0.096575, remove this variable and run the regression 
Model2O <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+
                x39+x41+x43+x44+x47+x48+x49+x51+x54+x57+
                x58+x60+x61+x66+x70+x71+x72+x75+x77+
                x79+x80+x83+x84+x85+x86, train_PA_new)
summary(Model2O)

#VAriable x23 has highest p-value as 0.065700, remove this variable and run the regression 
Model2P <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x25+x29+x33+x35+
                x39+x41+x43+x44+x47+x48+x49+x51+x54+x57+
                x58+x60+x61+x66+x70+x71+x72+x75+x77+
                x79+x80+x83+x84+x85+x86, train_PA_new)
summary(Model2P)

#VAriable x21 has highest p-value as 0.061149, remove this variable and run the regression 
Model2Q <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x25+x29+x33+x35+
                x39+x41+x43+x44+x47+x48+x49+x51+x54+x57+
                x58+x60+x61+x66+x70+x71+x72+x75+x77+
                x79+x80+x83+x84+x85+x86, train_PA_new)
summary(Model2Q)
max(vif(Model2Q))

#All the variables are significant in Model2Q

#Step 11: 
train_empty <- lm(y~1, train_PA_new)
train_full <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x27+x29+x31+x33+x35+
                   x37+x39+x41+x43+x44+x45+x46+x47+x48+x49+x50+x51+x52+x53+x54+x55+x56+x57+
                   x58+x59+x60+x61+x62+x63+x64+x66+x67+x68+x70+x71+x72+x73+x74+x75+x76+x77+
                   x78+x79+x80+x81+x82+x83+x84+x85+x86, train_PA_new)

  #stepAIC Forward
stepAIC(train_empty, direction='forward', scope = list(lower = train_empty, upper = train_full))

  #Regression model from stepAIC forward
Model3 <- lm( y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                x82 + x81 + x49 + x25 + x39 + x61 + x79 + x57 + x45 + x11 + 
                x66 + x83 + x72 + x35 + x71 + x75 + x78 + x70 + x77 + x55 + 
                x41 + x80 + x60 + x21 + x23 + x74 + x52 + x54 + x50, data = train_PA_new)
summary(Model3)
vif(Model3)
max(vif(Model3))
#x45 has vif = 13.50692. Remove the variable and re-run the regression
cooks.distance(Model3)
max(cooks.distance(Model3))
min(cooks.distance(Model3))
#All observations are in the range of -1 to 1, hence there are no outliers
Model3A <- lm( y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                x82 + x81 + x49 + x25 + x39 + x61 + x79 + x57 + x11 + 
                x66 + x83 + x72 + x35 + x71 + x75 + x78 + x70 + x77 + x55 + 
                x41 + x80 + x60 + x21 + x23 + x74 + x52 + x54 + x50, data = train_PA_new)
summary(Model3A)
vif(Model3A)
max(vif(Model3A))
#All variables have vif less than 5 now.

#Variable x50 has highest p-value as 0.856934, remove this variable and run the regression 
Model3B <- lm( y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                 x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                 x82 + x81 + x49 + x25 + x39 + x61 + x79 + x57 + x11 + 
                 x66 + x83 + x72 + x35 + x71 + x75 + x78 + x70 + x77 + x55 + 
                 x41 + x80 + x60 + x21 + x23 + x74 + x52 + x54 , data = train_PA_new)
summary(Model3B)

#Variable x55 has highest p-value as 0.700174, remove this variable and run the regression 
Model3C <- lm( y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                 x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                 x82 + x81 + x49 + x25 + x39 + x61 + x79 + x57 + x11 + 
                 x66 + x83 + x72 + x35 + x71 + x75 + x78 + x70 + x77 + 
                 x41 + x80 + x60 + x21 + x23 + x74 + x52 + x54 , data = train_PA_new)
summary(Model3C)

#Variable x81 has highest p-value as 0.690474, remove this variable and run the regression 
Model3D <- lm( y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                 x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                 x82 + x49 + x25 + x39 + x61 + x79 + x57 + x11 + 
                 x66 + x83 + x72 + x35 + x71 + x75 + x78 + x70 + x77 + 
                 x41 + x80 + x60 + x21 + x23 + x74 + x52 + x54 , data = train_PA_new)
summary(Model3D)

#Variable x82 has highest p-value as 0.466016, remove this variable and run the regression 
Model3E <- lm( y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                 x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                 x49 + x25 + x39 + x61 + x79 + x57 + x11 + 
                 x66 + x83 + x72 + x35 + x71 + x75 + x78 + x70 + x77 + 
                 x41 + x80 + x60 + x21 + x23 + x74 + x52 + x54 , data = train_PA_new)
summary(Model3E)

#Variable x52 has highest p-value as 0.239413, remove this variable and run the regression 
Model3F <- lm( y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                 x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                 x49 + x25 + x39 + x61 + x79 + x57 + x11 + 
                 x66 + x83 + x72 + x35 + x71 + x75 + x78 + x70 + x77 + 
                 x41 + x80 + x60 + x21 + x23 + x74 + x54 , data = train_PA_new)
summary(Model3F)

#Variable x23 has highest p-value as 0.073986, remove this variable and run the regression 
Model3F <- lm( y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                 x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                 x49 + x25 + x39 + x61 + x79 + x57 + x11 + 
                 x66 + x83 + x72 + x35 + x71 + x75 + x78 + x70 + x77 + 
                 x41 + x80 + x60 + x21 + x74 + x54 , data = train_PA_new)
summary(Model3F)

#Variable x74 has highest p-value as 0.069743, remove this variable and run the regression 
Model3G <- lm( y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                 x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                 x49 + x25 + x39 + x61 + x79 + x57 + x11 + 
                 x66 + x83 + x72 + x35 + x71 + x75 + x78 + x70 + x77 + 
                 x41 + x80 + x60 + x21 + x54 , data = train_PA_new)
summary(Model3G)

#Variable x21 has highest p-value as 0.065695, remove this variable and run the regression 
Model3H <- lm( y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                 x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                 x49 + x25 + x39 + x61 + x79 + x57 + x11 + 
                 x66 + x83 + x72 + x35 + x71 + x75 + x78 + x70 + x77 + 
                 x41 + x80 + x60 + x54 , data = train_PA_new)
summary(Model3H)

  #No more variable is non significant

#stepAIC : Backword
stepAIC(train_full, direction='backward', scope = list(lower = train_empty, upper = train_full))

#Regression model from stepAIC backward

Model4 <- lm(formula = y ~ x2 + x3 + x4 + x10 + x11 + x12 + x13 + x15 + 
               x17 + x19 + x21 + x23 + x25 + x29 + x33 + x35 + x39 + x41 + 
               x43 + x44 + x45 + x48 + x49 + x50 + x51 + x52 + x53 + x54 + 
               x55 + x56 + x57 + x58 + x60 + x62 + x66 + x70 + x71 + x72 + 
               x74 + x75 + x77 + x78 + x79 + x80 + x83 + x84 + x85 + x86, 
             data = train_PA_new)
summary(Model4)

#Check for VIF in Model4
vif(Model4)
max(vif(Model4))
#VIF for all variables are under 5.
cooks.distance(Model4)
max(cooks.distance(Model4))
min(cooks.distance(Model4))
#All observations are in the range of -1 to 1, hence there are no outliers

#Variable x51 has highest p-value as 0.098492, remove this variable and run the regression 
Model4A <- lm(formula = y ~ x2 + x3 + x4 + x10 + x11 + x12 + x13 + x15 + 
               x17 + x19 + x21 + x23 + x25 + x29 + x33 + x35 + x39 + x41 + 
               x43 + x44 + x45 + x48 + x49 + x50 + x52 + x53 + x54 + 
               x55 + x56 + x57 + x58 + x60 + x62 + x66 + x70 + x71 + x72 + 
               x74 + x75 + x77 + x78 + x79 + x80 + x83 + x84 + x85 + x86, 
             data = train_PA_new)
summary(Model4A)

#Variable x23 has highest p-value as 0.067810, remove this variable and run the regression 
Model4B <- lm(formula = y ~ x2 + x3 + x4 + x10 + x11 + x12 + x13 + x15 + 
                x17 + x19 + x21 + x25 + x29 + x33 + x35 + x39 + x41 + 
                x43 + x44 + x45 + x48 + x49 + x50 + x52 + x53 + x54 + 
                x55 + x56 + x57 + x58 + x60 + x62 + x66 + x70 + x71 + x72 + 
                x74 + x75 + x77 + x78 + x79 + x80 + x83 + x84 + x85 + x86, 
              data = train_PA_new)
summary(Model4B)

#Variable x21 has highest p-value as 0.063898, remove this variable and run the regression 
Model4C <- lm(formula = y ~ x2 + x3 + x4 + x10 + x11 + x12 + x13 + x15 + 
                x17 + x19 + x25 + x29 + x33 + x35 + x39 + x41 + 
                x43 + x44 + x45 + x48 + x49 + x50 + x52 + x53 + x54 + 
                x55 + x56 + x57 + x58 + x60 + x62 + x66 + x70 + x71 + x72 + 
                x74 + x75 + x77 + x78 + x79 + x80 + x83 + x84 + x85 + x86, 
              data = train_PA_new)
summary(Model4C)

#Variable x74 has highest p-value as 0.057490, remove this variable and run the regression 
Model4D <- lm(formula = y ~ x2 + x3 + x4 + x10 + x11 + x12 + x13 + x15 + 
                x17 + x19 + x25 + x29 + x33 + x35 + x39 + x41 + 
                x43 + x44 + x45 + x48 + x49 + x50 + x52 + x53 + x54 + 
                x55 + x56 + x57 + x58 + x60 + x62 + x66 + x70 + x71 + x72 + 
                x75 + x77 + x78 + x79 + x80 + x83 + x84 + x85 + x86, 
              data = train_PA_new)
summary(Model4D)

#stepAIC : Both
stepAIC(train_empty, direction='both', scope = list(lower = train_empty, upper = train_full))

Model5 <- lm(y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                x49 + x25 + x39 + x61 + x79 + x57 + x45 + x11 + x66 + x83 + 
                x72 + x35 + x71 + x75 + x78 + x70 + x77 + x55 + x41 + x80 + 
                x60 + x21 + x23 + x74 + x52 + x54 + x50, data = train_PA_new)
summary(Model5)

vif(Model5)
max(vif(Model5))
#Variable x45 has vif 13.49548. Remove the variable.
cooks.distance(Model5)
max(cooks.distance(Model5))
min(cooks.distance(Model5))
#All observations are in the range of -1 to 1, hence there are no outliers

#Variable x45 has highest p-value as 0.123390, remove this variable and run the regression 
Model5A <- lm(y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                x49 + x25 + x39 + x61 + x79 + x57 + x11 + x66 + x83 + 
                x72 + x35 + x71 + x75 + x78 + x70 + x77 + x55 + x41 + x80 + 
                x60 + x21 + x23 + x74 + x52 + x54 + x50, data = train_PA_new)
summary(Model5A)

#Variable x50 has highest p-value as 0.859014, remove this variable and run the regression 
Model5B <- lm(y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                x49 + x25 + x39 + x61 + x79 + x57 + x11 + x66 + x83 + 
                x72 + x35 + x71 + x75 + x78 + x70 + x77 + x55 + x41 + x80 + 
                x60 + x21 + x23 + x74 + x52 + x54 , data = train_PA_new)
summary(Model5B)

#Variable x55 has highest p-value as 0.709150, remove this variable and run the regression 
Model5C <- lm(y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                x49 + x25 + x39 + x61 + x79 + x57 + x11 + x66 + x83 + 
                x72 + x35 + x71 + x75 + x78 + x70 + x77 + x41 + x80 + 
                x60 + x21 + x23 + x74 + x52 + x54 , data = train_PA_new)
summary(Model5C)

#Variable x52 has highest p-value as 0.239413, remove this variable and run the regression 
Model5D <- lm(y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                x49 + x25 + x39 + x61 + x79 + x57 + x11 + x66 + x83 + 
                x72 + x35 + x71 + x75 + x78 + x70 + x77 + x41 + x80 + 
                x60 + x21 + x23 + x74 + x54 , data = train_PA_new)
summary(Model5D)

#Variable x23 has highest p-value as 0.073986, remove this variable and run the regression 
Model5E <- lm(y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                x49 + x25 + x39 + x61 + x79 + x57 + x11 + x66 + x83 + 
                x72 + x35 + x71 + x75 + x78 + x70 + x77 + x41 + x80 + 
                x60 + x21 + x74 + x54 , data = train_PA_new)
summary(Model5E)

#Variable x74 has highest p-value as 0.069743, remove this variable and run the regression 
Model5E <- lm(y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                x49 + x25 + x39 + x61 + x79 + x57 + x11 + x66 + x83 + 
                x72 + x35 + x71 + x75 + x78 + x70 + x77 + x41 + x80 + 
                x60 + x21 + x54 , data = train_PA_new)
summary(Model5E)

#Variable x21 has highest p-value as 0.065695, remove this variable and run the regression 
Model5F <- lm(y ~ x12 + x17 + x47 + x86 + x85 + x15 + x84 + x19 + 
                x44 + x2 + x33 + x43 + x10 + x3 + x48 + x13 + x4 + x29 + 
                x49 + x25 + x39 + x61 + x79 + x57 + x11 + x66 + x83 + 
                x72 + x35 + x71 + x75 + x78 + x70 + x77 + x41 + x80 + 
                x60 + x54 , data = train_PA_new)
summary(Model5F)

#Step 17: regsubset
??regsubsets
regbest_PA <- regsubsets(train_PA_new[,-c(8,9,60,46,67)], train_PA_new[,9], nbest=1, nvmax=65, method="forward")
regbest_PA_sum <- summary(regbest_PA)
regbest_PA_sum
options(max.print = 100000)

regbest_PA_BIC <- regbest_PA_sum$bic
regbest_PA_BIC
min(regbest_PA_BIC) #Minimum is model27, 28, 26 
sort(regbest_PA_BIC)
#Model_minBIC_27: x2+x3+x4+x10+x12+x13+x15+x17+x19+x25+x29+x33+x39+x43+x44+x45+x47+x48+x49+x57+x72+x79+x82+x83+x84+x85+x86
Model_minBIC_27 <- lm(y~x2+x3+x4+x10+x12+x13+x15+x17+x19+x25+x29+x33+x39+x43+x44+x45+x47+x48+x49+x57+x72+x79+x82+x83+x84+x85+x86, data = train_PA_new)
summary(Model_minBIC_27)
#Model_minBIC_28: x2+x3+x4+x10+x12+x13+x15+x17+x19+x25+x29+x33+x35+x39+x43+x44+x45+x47+x48+x49+x57+x72+x79+x82+x83+x84+x85+x86
Model_minBIC_28 <- lm(y~x2+x3+x4+x10+x12+x13+x15+x17+x19+x25+x29+x33+x35+x39+x43+x44+x45+x47+x48+x49+x57+x72+x79+x82+x83+x84+x85+x86, data = train_PA_new)
summary(Model_minBIC_28)
#Model_minBIC_26: x2+x3+x4+x10+x12+x13+x15+x17+x19+x25+x29+x33+x39+x43+x44+x45+x47+x48+x49+x57+x79+x82+x83+x84+x85+x86
Model_minBIC_26 <- lm(y~x2+x3+x4+x10+x12+x13+x15+x17+x19+x25+x29+x33+x39+x43+x44+x45+x47+x48+x49+x57+x79+x82+x83+x84+x85+x86, data = train_PA_new)
summary(Model_minBIC_26)

regbest_PA_cp <- regbest_PA_sum$cp
regbest_PA_cp
min(regbest_PA_cp)
sort(regbest_PA_cp)
#Minimum is model49 (cp value = 48), 50  (cp value = 51)
#Model_minCP_49: x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+x39+x41+x43+x44+x45+x47+x48+x49+x50+x51+x52+x54+x55+x57+x58+x60+x61+x67+x68+x70+x71+x72+x73+x74+x75+x76+x78+x79+x81+x83+x84+x85+x86)
Model_minCP_49 <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+x39+x41+x43+x44+x45+x47+x48+x49+x50+x51+x52+x54+x55+x57+x58+x60+x61+x67+x68+x70+x71+x72+x73+x74+x75+x76+x78+x79+x81+x83+x84+x85+x86, data = train_PA_new)
summary(Model_minCP_49)

#Remove x50 as the p-value is higher than 0.05  i.e. 0.278233
Model_minCP_49A <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+x39+x41+x43+x44+x45+x47+x48+x49+x50+x51+x52+x54+x55+x57+x58+x60+x61+x67+x68+x70+x71+x72+x73+x75+x76+x78+x79+x81+x83+x84+x85+x86, data = train_PA_new)
summary(Model_minCP_49A)

#Remove x60 as the p-value is higher than 0.05  i.e. 0.401023
Model_minCP_49B <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+x39+x41+x43+x44+x45+x47+x48+x49+x50+x51+x52+x54+x55+x57+x58+x61+x67+x68+x70+x71+x72+x73+x75+x76+x78+x79+x81+x83+x84+x85+x86, data = train_PA_new)
summary(Model_minCP_49B)

#Remove x51 as the p-value is higher than 0.05  i.e. 0.629008
Model_minCP_49C <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+x39+x41+x43+x44+x45+x47+x48+x49+x50+x52+x54+x55+x57+x58+x61+x67+x68+x70+x71+x72+x73+x75+x76+x78+x79+x81+x83+x84+x85+x86, data = train_PA_new)
summary(Model_minCP_49C)

#Remove x70 as the p-value is higher than 0.05  i.e. 0.143483
Model_minCP_49D <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+x39+x41+x43+x44+x45+x47+x48+x49+x50+x52+x54+x55+x57+x58+x61+x67+x68+x71+x72+x73+x75+x76+x78+x79+x81+x83+x84+x85+x86, data = train_PA_new)
summary(Model_minCP_49D)

#Remove x78 as the p-value is higher than 0.05  i.e. 0.111773
Model_minCP_49E <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+x39+x41+x43+x44+x45+x47+x48+x49+x50+x52+x54+x55+x57+x58+x61+x67+x68+x71+x72+x73+x75+x76+x79+x81+x83+x84+x85+x86, data = train_PA_new)
summary(Model_minCP_49E)

#Remove x75 as the p-value is higher than 0.05  i.e. 0.111079
Model_minCP_49F <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+x39+x41+x43+x44+x45+x47+x48+x49+x50+x52+x54+x55+x57+x58+x61+x67+x68+x71+x72+x73+x76+x79+x81+x83+x84+x85+x86, data = train_PA_new)
summary(Model_minCP_49F)

#Remove x71 as the p-value is higher than 0.05  i.e. 0.116084
Model_minCP_49G <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+x39+x41+x43+x44+x45+x47+x48+x49+x50+x52+x54+x55+x57+x58+x61+x67+x68+x72+x73+x76+x79+x81+x83+x84+x85+x86, data = train_PA_new)
summary(Model_minCP_49G)

#Remove x61 as the p-value is higher than 0.05  i.e. 0.081096
Model_minCP_49H <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+x39+x41+x43+x44+x45+x47+x48+x49+x50+x52+x54+x55+x57+x58+x67+x68+x72+x73+x76+x79+x81+x83+x84+x85+x86, data = train_PA_new)
summary(Model_minCP_49H)

#Remove x23 as the p-value is higher than 0.05  i.e. 0.066204
Model_minCP_49I <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x25+x29+x33+x35+x39+x41+x43+x44+x45+x47+x48+x49+x50+x52+x54+x55+x57+x58+x67+x68+x72+x73+x76+x79+x81+x83+x84+x85+x86, data = train_PA_new)
summary(Model_minCP_49I)

#Remove x21 as the p-value is higher than 0.05  i.e. 0.059472
Model_minCP_49J <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x25+x29+x33+x35+x39+x41+x43+x44+x45+x47+x48+x49+x50+x52+x54+x55+x57+x58+x67+x68+x72+x73+x76+x79+x81+x83+x84+x85+x86, data = train_PA_new)
summary(Model_minCP_49J)

#Step18
regbest_PA1 <- regsubsets(train_PA_new[,-c(8,9,60,46,67)], train_PA_new[,9], nbest=1, nvmax=65, method="backward")
regbest_PA_sum1 <- summary(regbest_PA1)
regbest_PA_sum1
options(max.print = 100000)

regbest_PA1_BIC <- regbest_PA_sum1$bic
regbest_PA1_BIC
min(regbest_PA1_BIC) #Minimum is model32, 33, 34
sort(regbest_PA1_BIC)

#Model_min1BIC_32: x2+x3+x4+x10+x12+x13+x15+x17+x19+x25+x29+x33+x39+x43+x44+x45+x48+x49+x50+x52+x53+x54+x55+x57+x58+x62+x79+x82+x83+x84+x85+x86
Model_min1BIC_32 <- lm(y~x2+x3+x4+x10+x12+x13+x15+x17+x19+x25+x29+x33+x39+x43+x44+x45+x48+x49+x50+x52+x53+x54+x55+x57+x58+x62+x79+x82+x83+x84+x85+x86, data = train_PA_new)
summary(Model_min1BIC_32)

#Model_min1BIC_33: x2+x3+x4+x10+x12+x13+x15+x17+x19+x25+x29+x33+x39+x43+x44+x45+x48+x49+x50+x52+x53+x54+x55+x57+x58+x62+x72+x79+x82+x83+x84+x85+x86
Model_min1BIC_33 <- lm(y~x2+x3+x4+x10+x12+x13+x15+x17+x19+x25+x29+x33+x39+x43+x44+x45+x48+x49+x50+x52+x53+x54+x55+x57+x58+x62+x72+x79+x82+x83+x84+x85+x86, data = train_PA_new)
summary(Model_min1BIC_33)

#Model_min1BIC_34: x2+x3+x4+x10+x12+x13+x15+x17+x19+x25+x29+x33+x35+x39+x43+x44+x45+x48+x49+x50+x52+x53+x54+x55+x57+x58+x62+x72+x79+x82+x83+x84+x85+x86
Model_min1BIC_34 <- lm(y~x2+x3+x4+x10+x12+x13+x15+x17+x19+x25+x29+x33+x35+x39+x43+x44+x45+x48+x49+x50+x52+x53+x54+x55+x57+x58+x62+x72+x79+x82+x83+x84+x85+x86, data = train_PA_new)
summary(Model_min1BIC_34)

regbest_PA1_cp <- regbest_PA_sum1$cp
regbest_PA1_cp
min(regbest_PA1_cp)
sort(regbest_PA1_cp)
#Minimum is model43 (cp value = 45.78705), 44  (cp value = 44.42434)
#Model_minCP1_44: x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+x39+x41+x43+x44+x45+x48+x49+x50+x52+x53+x54+x55+x56+x57+x58+x60+x62+x67+x68+x72+x73+x76+x79+x82+x83+x84+x85+x86)
Model_minCP1_44 <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x21+x23+x25+x29+x33+x35+x39+x41+x43+x44+x45+x48+x49+x50+x52+x53+x54+x55+x56+x57+x58+x60+x62+x67+x68+x72+x73+x76+x79+x82+x83+x84+x85+x86, data = train_PA_new)
summary(Model_minCP1_44)

#Remove x21 as the p-value is higher than 0.05
#Model_minCP1_44A: x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x23+x25+x29+x33+x35+x39+x41+x43+x44+x45+x48+x49+x50+x52+x53+x54+x55+x56+x57+x58+x60+x62+x67+x68+x72+x73+x76+x79+x82+x83+x84+x85+x86)
Model_minCP1_44A <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x23+x25+x29+x33+x35+x39+x41+x43+x44+x45+x48+x49+x50+x52+x53+x54+x55+x56+x57+x58+x60+x62+x67+x68+x72+x73+x76+x79+x82+x83+x84+x85+x86, data = train_PA_new)
summary(Model_minCP1_44A)

#Remove x23 as the p-value is higher than 0.05  i.e. 0.066691
#Model_minCP1_44B: x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x25+x29+x33+x35+x39+x41+x43+x44+x45+x48+x49+x50+x52+x53+x54+x55+x56+x57+x58+x60+x62+x67+x68+x72+x73+x76+x79+x82+x83+x84+x85+x86)
Model_minCP1_44B <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x25+x29+x33+x35+x39+x41+x43+x44+x45+x48+x49+x50+x52+x53+x54+x55+x56+x57+x58+x60+x62+x67+x68+x72+x73+x76+x79+x82+x83+x84+x85+x86, data = train_PA_new)
summary(Model_minCP1_44B)

#Model_minCP1_43: x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x23+x25+x29+x33+x35+x39+x41+x43+x44+x45+x48+x49+x50+x52+x53+x54+x55+x56+x57+x58+x60+x62+x67+x68+x72+x73+x76+x79+x82+x83+x84+x85+x86)
Model_minCP1_43 <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x23+x25+x29+x33+x35+x39+x41+x43+x44+x45+x48+x49+x50+x52+x53+x54+x55+x56+x57+x58+x60+x62+x67+x68+x72+x73+x76+x79+x82+x83+x84+x85+x86, data = train_PA_new)
summary(Model_minCP1_43)

#Remove x23 as the p-value is higher than 0.05  i.e. 0.059781
#Model_minCP1_43A: x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x25+x29+x33+x35+x39+x41+x43+x44+x45+x48+x49+x50+x52+x53+x54+x55+x56+x57+x58+x60+x62+x67+x68+x72+x73+x76+x79+x82+x83+x84+x85+x86)
Model_minCP1_43A <- lm(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x25+x29+x33+x35+x39+x41+x43+x44+x45+x48+x49+x50+x52+x53+x54+x55+x56+x57+x58+x60+x62+x67+x68+x72+x73+x76+x79+x82+x83+x84+x85+x86, data = train_PA_new)
summary(Model_minCP1_43A)

#Model_minCP_49J 


#Step20 : Exhaustive search will be SLOW, must have really.big = T
#regbest_PA3 <- regsubsets(train_PA_new[,-c(8,9,60,46,67)], train_PA_new[,9], nbest=1, nvmax=65, method="exhaustive")
#regbest_PA_sum3 <- summary(regbest_PA3)
#regbest_PA_sum3
#options(max.print = 300000)

summary(Model2Q)

#Final Models
Model2Q #Model from full regression
Model3H #Model from stepAIC forward
Model4D #Model from stepAIC backward
Model5F #Model from stepAIC both
Model_minBIC_26 #Model from min BIC Forward
Model_minBIC_27 #Model from min BIC Forward
Model_minBIC_28 #Model from min BIC Forward
Model_minCP_49J #Model from min CP Forward
summary(Model_min1BIC_32) #Model from min BIC backward
Model_min1BIC_33 #Model from min BIC backward
Model_min1BIC_34 #Model from min BIC backward
Model_minCP1_43A #Model from min CP backward
Model_minCP1_44B #Model from min CP backward



################################################################################################################
#Model Comparison

Model_1 =Model2Q
Model_2 =Model3H
Model_3 =Model4D
Model_4 =Model5F
Model_5 =Model_minBIC_26
Model_6 =Model_minBIC_27
Model_7 =Model_minBIC_28
Model_8 =Model_minCP_49J
Model_12 =Model_min1BIC_32
Model_10 =Model_min1BIC_33
Model_11 =Model_min1BIC_34
Model_12 = Model_minCP1_43A
Model_13 =Model_minCP1_44B

##------------------------------------------
#r -square, adjusted R square, BIC, AIC
?glance

M1 = as.data.frame(glance(Model_1))
M2 = as.data.frame(glance(Model_2))
M3 = as.data.frame(glance(Model_3))
M4 = as.data.frame(glance(Model_4))
M5 = as.data.frame(glance(Model_5))
M6 = as.data.frame(glance(Model_6))
M7 = as.data.frame(glance(Model_7))
M8 = as.data.frame(glance(Model_8))
M9 = as.data.frame(glance(Model_12))
M10 = as.data.frame(glance(Model_10))
M11 = as.data.frame(glance(Model_11))
M12 = as.data.frame(glance(Model_12))
M13 = as.data.frame(glance(Model_13))

# VIF and cooks distance
P1 = c(max(vif(Model1)), max(cooks.distance(Model_1)))
P2 = c(max(vif(Model2)), max(cooks.distance(Model_2)))
P3 = c(max(vif(Model3)), max(cooks.distance(Model_3)))
P4 = c(max(vif(Model4)), max(cooks.distance(Model_4)))
P5 = c(max(vif(Model5)), max(cooks.distance(Model_5)))
P6 = c(max(vif(Model6)), max(cooks.distance(Model_6)))
P7 = c(max(vif(Model7)), max(cooks.distance(Model_7)))
P8 = c(max(vif(Model8)), max(cooks.distance(Model_8)))
P9 = c(max(vif(Model8)), max(cooks.distance(Model_12)))
P10 = c(max(vif(Model8)), max(cooks.distance(Model_10)))
P11 = c(max(vif(Model8)), max(cooks.distance(Model_11)))
P12 = c(max(vif(Model8)), max(cooks.distance(Model_12)))
P13 = c(max(vif(Model8)), max(cooks.distance(Model_13)))


results = rbind(M1,M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13)
results
#params = rbind(P1,P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13)
#params



##------------------------------------------
#residual plots


#Model_1
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_1$residuals, main = "Model 1")
qqline(Model_1$residuals, col =4)

plot(Model_1$residuals ~ Model_1$fitted.values, main = "Model 1")
par(plotgrid)

#Model_2
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_2$residuals)
qqline(Model_2$residuals, col=4)

plot(Model_2$residuals ~ Model_2$fitted.values)
par(plotgrid)

#Model_3
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_3$residuals)
qqline(Model_3$residuals, col =4)

plot(Model_3$residuals ~ Model_3$fitted.values)
par(plotgrid)

#Model_4
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_4$residuals)
qqline(Model_4$residuals, col =4)

plot(Model_4$residuals ~ Model_4$fitted.values)
par(plotgrid)

#Model5
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_5$residuals)
qqline(Model_5$residuals, col =4)

plot(Model_5$residuals ~ Model_5$fitted.values)
par(plotgrid)

#Model_6
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_6$residuals)
qqline(Model_6$residuals, col =4)

plot(Model_6$residuals ~ Model_6$fitted.values)
par(plotgrid)

#Model_7
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_7$residuals)
qqline(Model_7$residuals, col =4)

plot(Model_7$residuals ~ Model_7$fitted.values)
par(plotgrid)

#Model_8
plotgrid <- par(mfrow=c(1, 2))
qqnorm(Model_8$residuals)
qqline(Model_8$residuals, col =4)

plot(Model_8$residuals ~ Model_8$fitted.values)
par(plotgrid)

##------------------------------------------
#press staticstics
library(dplyr)
library(leaps)
library(MASS)
library(car)
library(caret)
library(MPV)
library(broom)


press = rbind(
  PRESS(Model_1),
  PRESS(Model_2),
  PRESS(Model_3),
  PRESS(Model_4),
  PRESS(Model_5),
  PRESS(Model_6),
  PRESS(Model_7),
  PRESS(Model_8),
  PRESS(Model_12),
  PRESS(Model_10),
  PRESS(Model_11),
  PRESS(Model_12),
  PRESS(Model_13))
press
##------------------------------------------
#Sum of Square Prediction Error

Pred1<-predict(Model_1,test_PA_new);sum((Pred1-test_PA_new[,"y"])^2)
Pred2<-predict(Model_2,test_PA_new);sum((Pred2-test_PA_new[,"y"])^2)
Pred3<-predict(Model_3,test_PA_new);sum((Pred3-test_PA_new[,"y"])^2)
Pred4<-predict(Model_4,test_PA_new);sum((Pred4-test_PA_new[,"y"])^2)
Pred5<-predict(Model_5,test_PA_new);sum((Pred5-test_PA_new[,"y"])^2)
Pred6<-predict(Model_6,test_PA_new);sum((Pred6-test_PA_new[,"y"])^2)
Pred7<-predict(Model_7,test_PA_new);sum((Pred7-test_PA_new[,"y"])^2)
Pred8<-predict(Model_8,test_PA_new);sum((Pred8-test_PA_new[,"y"])^2)
Pred9<-predict(Model_12,test_PA_new);sum((Pred9-test_PA_new[,"y"])^2)
Pred10<-predict(Model_10,test_PA_new);sum((Pred10-test_PA_new[,"y"])^2)
Pred11<-predict(Model_11,test_PA_new);sum((Pred11-test_PA_new[,"y"])^2)
Pred12<-predict(Model_12,test_PA_new);sum((Pred12-test_PA_new[,"y"])^2)
Pred13<-predict(Model_13,test_PA_new);sum((Pred13-test_PA_new[,"y"])^2)

predr2 = rbind(Model_1 = sum((Pred1-test_PA_new[,"y"])^2),
               Model_2 = sum((Pred2-test_PA_new[,"y"])^2),
               Model_3 = sum((Pred3-test_PA_new[,"y"])^2),
               Model_4 = sum((Pred4-test_PA_new[,"y"])^2),
               Model_5 = sum((Pred5-test_PA_new[,"y"])^2),
               Model_6 = sum((Pred6-test_PA_new[,"y"])^2),
               Model_7 = sum((Pred7-test_PA_new[,"y"])^2),
               Model_8 = sum((Pred8-test_PA_new[,"y"])^2),
               Model_12 =sum((Pred9-test_PA_new[,"y"])^2),
               Model_10 =sum((Pred10-test_PA_new[,"y"])^2),
               Model_11 =sum((Pred11-test_PA_new[,"y"])^2),
               Model_12 =sum((Pred12-test_PA_new[,"y"])^2),
               Model_13 =sum((Pred13-test_PA_new[,"y"])^2))
predr2

total = cbind(results, press, predr2)
row.names(total) = c(reg_TX14,
                     TX_BIC_bkwd,
                     TX_CP_bkwd4,
                     TX_BIC_frwd,
                     TX_CP_frwd4,
                     TX_AIC_frwd6,
                     TX_AIC_both12,
                     TX_AIC_bkwd14)
total[,c(1,2,8,9,13,14)]

#------------------------------------------------
#Kfold cross validation
set.seed(144)

c<-trainControl(method="cv",number=10)


Kfold_Model1 = train(y~x2+x3+x4+x10+x11+x12+x13+x15+x17+x19+x23+x25+x33+x35+
                       x37+x41+x44+x46+x47+x48+x50+x52+x53+x54+x55+x56+x57+
                       x58+x60+x61+x62+x66+x67+x68+x71+x74+x75+x76+x77+
                       x78+x79+x80+x81+x82+x83+x84+x85+x86, Accident_TX, trControl=c,method="lm")

Kfold_Model2 = train(y ~ x2 + x3 + x4 + x10 + x11 + x12 + x13 + x15 + 
                       x17 + x19 + x23 + x25 + x33 + x35 + x44 + x45 + x48 + 
                       x50 + x53 + x54 + x56 + x57 + x58 + x62 + x80 + x84 + 
                       x86, Accident_TX, trControl=c,method="lm")

Kfold_Model3 = train(y ~ x2 + x3 + x4 + x10 + x11 + x12 + x13 + x15 + 
                       x17 + x19 + x23 + x25 + x33 + x35 + x37 + x41 + 
                       x44 + x45 + x48 + x49 + x50 + x53 + x54 + x56 + 
                       x57 + x58 + x60 + x61 + x62 + x66 + x67 + x68 + 
                       x70 + x71 + x72 + x74 + x75 + x76 + x77 + x78 + 
                       x79 + x80 + x81 + x82 + x83 + x84 + x85 + x86, Accident_TX, trControl=c,method="lm")

Kfold_Model4 = train(y ~ x2 + x3 + x4 + x10 + x11 + x12 + x13 + x15 + 
                       x17 + x19 + x23 + x25 + x33 + x35 + x44 + x46 + x47 + 
                       x48 + x50 + x52 + x54 + x55 + x58 + x60 + x61 + x62 + 
                       x80 + x84 + x86, Accident_TX, trControl=c,method="lm")

Kfold_Model5 = train(y ~ x2 + x3 + x4 + x10 + x11 + x12 + x13 + x15 + 
                       x17 + x19 + x23 + x25 + x33 + x35 + x37 + x41 + 
                       x44 + x46 + x47 + x48 + x50 + x52 + x54 + 
                       x55 + x57 + x58 + x60 + x61 + x62 + x66 + x67 + x68 + 
                       x70 + x71 + x72 + x74 + x75 + x76 + x77 + x78 + 
                       x79 + x80 + x81 + x82 + x83 + x84 + x85 + x86, Accident_TX, trControl=c,method="lm")

Kfold_Model6 = train(y ~ x47 + x4 + x12 + x33 + x3 + x15 + x86 + x44 + 
                       x19 + x17 + x84 + x13 + x10 + x23 + x48 + x2 + x80 + x52 + 
                       x11 + x25 + x35 + x61 + x60 + x50 + x57 + x45 + 
                       x85 + x55 + x81 + x82 + x46 + x78 + x73 + x37 + x41, Accident_TX, trControl=c,method="lm")

Kfold_Model7 = train(y ~ x86 + x15 + x12 + x47 + x17 + x11 + x85 + x84 + 
                       x33 + x10 + x44 + x4 + x43 + x57 + x19 + x81 + 
                       x48 + x70 + x83 + x23 + x74 + x73 + x37 + 
                       x71 + x25 + x45 + 
                       x75 + x61 + x56 + x54 + x13, Accident_TX, trControl=c,method="lm")

Kfold_Model8 = train(y ~ x2 + x3 + x4 + x10 + x11 + x12 + x13 + x15 + 
                       x17 + x19 + x23 + x25 + x33 + x35 + x37 + x41 + x44 + 
                       x45 + x48 + x50 + x52 + x53 + x54 + x56 + x57 + x58 + 
                       x60 + x61 + x62 + 
                       x78 + x80 + x81 + x82 + x84 + 
                       x85 + x86, Accident_TX, trControl=c,method="lm")


rbind(Model1 = Kfold_Model1$results,  
      Model2 = Kfold_Model2$results,
      Model3 = Kfold_Model3$results,
      Model4 = Kfold_Model4$results,
      Model5 = Kfold_Model5$results, 
      Model6 = Kfold_Model6$results, 
      Model7 = Kfold_Model7$results, 
      Model8 = Kfold_Model8$results )

#Prediction R square

PRM1 <- 1- PRESS(Model_1)/ sum(anova(Model_1)$`Sum Sq`)
PRM2 <- 1- PRESS(Model_2)/ sum(anova(Model_2)$`Sum Sq`)
PRM3 <- 1- PRESS(Model_3)/ sum(anova(Model_3)$`Sum Sq`)
PRM4 <- 1- PRESS(Model_4)/ sum(anova(Model_4)$`Sum Sq`)
PRM5 <- 1- PRESS(Model_5)/ sum(anova(Model_5)$`Sum Sq`)
PRM6 <- 1- PRESS(Model_6)/ sum(anova(Model_6)$`Sum Sq`)
PRM7 <- 1- PRESS(Model_7)/ sum(anova(Model_7)$`Sum Sq`)
PRM8 <- 1- PRESS(Model_8)/ sum(anova(Model_8)$`Sum Sq`)
PRM9 <- 1- PRESS(Model_12)/ sum(anova(Model_12)$`Sum Sq`)
PRM10 <- 1- PRESS(Model_10)/ sum(anova(Model_10)$`Sum Sq`)
PRM11 <- 1- PRESS(Model_11)/ sum(anova(Model_11)$`Sum Sq`)
PRM12 <- 1- PRESS(Model_12)/ sum(anova(Model_12)$`Sum Sq`)
PRM13 <- 1- PRESS(Model_13)/ sum(anova(Model_13)$`Sum Sq`)

rbind(PRM1, PRM2, PRM3, PRM4, PRM5, PRM6, PRM7, PRM8, PRM9, PRM10,
      PRM11, PRM12, PRM13)


comp1<- cbind(summary(Model_1)$df[1],summary(Model_1)$r.squared,summary(Model_1)$adj.r.squared,summary(Model_1)$fstatistic[1],
      AIC(Model_1),BIC(Model_1), PRESS(Model_1))
comp2<- cbind(summary(Model_2)$df[1],summary(Model_2)$r.squared,summary(Model_2)$adj.r.squared,summary(Model_2)$fstatistic[1],
      AIC(Model_2),BIC(Model_2), PRESS(Model_2))
comp3 <- cbind(summary(Model_3)$df[1],summary(Model_3)$r.squared,summary(Model_3)$adj.r.squared,summary(Model_3)$fstatistic[1],
      AIC(Model_3),BIC(Model_3), PRESS(Model_3))
comp4 <-cbind(summary(Model_4)$df[1],summary(Model_4)$r.squared,summary(Model_4)$adj.r.squared,summary(Model_4)$fstatistic[1],
      AIC(Model_4),BIC(Model_4), PRESS(Model_4))
comp5 <- cbind(summary(Model_5)$df[1],summary(Model_5)$r.squared,summary(Model_5)$adj.r.squared,summary(Model_5)$fstatistic[1],
      AIC(Model_5),BIC(Model_5), PRESS(Model_5))
comp6 <- cbind(summary(Model_6)$df[1],summary(Model_6)$r.squared,summary(Model_6)$adj.r.squared,summary(Model_6)$fstatistic[1],
      AIC(Model_6),BIC(Model_6), PRESS(Model_6))
comp7<- cbind(summary(Model_7)$df[1],summary(Model_7)$r.squared,summary(Model_7)$adj.r.squared,summary(Model_7)$fstatistic[1],
      AIC(Model_7),BIC(Model_7), PRESS(Model_7))
comp8<- cbind(summary(Model_8)$df[1],summary(Model_8)$r.squared,summary(Model_8)$adj.r.squared,summary(Model_8)$fstatistic[1],
      AIC(Model_8),BIC(Model_8), PRESS(Model_8))
comp9<- cbind(summary(Model_9)$df[1],summary(Model_9)$r.squared,summary(Model_9)$adj.r.squared,summary(Model_9)$fstatistic[1],
      AIC(Model_9),BIC(Model_9), PRESS(Model_9))
comp10 <- cbind(summary(Model_10)$df[1],summary(Model_10)$r.squared,summary(Model_10)$adj.r.squared,summary(Model_10)$fstatistic[1],
      AIC(Model_10),BIC(Model_10), PRESS(Model_10))
comp11<- cbind(summary(Model_11)$df[1],summary(Model_11)$r.squared,summary(Model_11)$adj.r.squared,summary(Model_11)$fstatistic[1],
      AIC(Model_11),BIC(Model_11), PRESS(Model_11))
comp12<- cbind(summary(Model_12)$df[1],summary(Model_12)$r.squared,summary(Model_12)$adj.r.squared,summary(Model_12)$fstatistic[1],
      AIC(Model_12),BIC(Model_12), PRESS(Model_12))
comp13<- cbind(summary(Model_13)$df[1],summary(Model_13)$r.squared,summary(Model_13)$adj.r.squared,summary(Model_13)$fstatistic[1],
      AIC(Model_13),BIC(Model_13), PRESS(Model_13))

rbind(comp1, comp2, comp3, comp4, comp5, comp6, comp7, comp8, comp9, comp10, comp11, comp12, comp13)



