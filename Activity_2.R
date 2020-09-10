#Activity 2 

heights <- c(3, 2, 3)

heights[1]


#Creating Vectors
Character_Vector <- c("1", "2", "3", "Hi", "Bye")
Character_Vector

Numeric_Vector <- c("-100", "2", "2.2", "4.54", "-2.3")
Numeric_Vector

Integer_Vector <- c("1", "2", "3", "4", "5")
Integer_Vector

mydata <- c("1", "9", "7", "4", "8")
Factor_Vector <- as.factor(mydata)
Factor_Vector

#Matrix set up by rows:
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

#Matrix set up by columns
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol

#Look at row 1 column 2
Mat.bycol[1,2]

#Look at all values in column 2
Mat.bycol[,2]

#read in weather data 
datW <- read.csv("/Users/madelynbeitler/GitHub/ENVSTDATA/Activity2Data/noaa2011124.csv")

#Get information about the data frame 
str(datW)

#Convert data type into a factor
datW$NAME <- as.factor(datW$NAME)

#find out all unique site names
levels(datW$NAME)

#find mean max temperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])

#Use na.rm=TRUE to ignore NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#Same but with standard deviation
sd(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#calculate average daily temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get mean across all sites
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean", na.rm=TRUE)
averageTemp

#Change the automatic output to better names
#MAAT means MEan Annual Air Temp
colnames(averageTemp) <- c("NAME", "MAAT")
averageTemp

#Convert level to number for factor data type
datW$siteN <- as.numeric(datW$NAME)

#make a histogram for the first site in our levels
#main= is the title name argument
#Paste the actual name of the factor not the numeric index
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col= "blue",
     border="white")

#Question 4 Histogram for Mormon Flat AZ
hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE,
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col= "red",
     border="white")

#pnorm (value to evaluate at (note this will evaluate all values and below), mean, standard deviation)
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#pnorm with 5 --> area of the curve below 5, aka probability below 5
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#to get range from 0-5, subtract pnorm 5 -pnorm 0
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE)) - pnorm(0,
                                                         mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                                                         sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#pnorm of 20 gives me all probability below 20
#subtract 1 to get area above 20

1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#qnorm gives the value at which all other values below equal the probability given in the argument. 
#Calculating the value of probability of .95

qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN ==1],na.rm=TRUE))

#histogram of Aberdeen adding 4 degrees
CCAtemp <- datW$TAVE + 4

hist(CCAtemp[datW$siteN == 1],
     freq=FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col= "blue",
     border="white")

#Find probability of climate change temperatures to be above the threshold for extreme temps
1 - pnorm(18.51026,
          mean(CCAtemp[datW$siteN == 1],na.rm=TRUE),
          sd(CCAtemp[datW$siteN == 1],na.rm=TRUE))

#Make histogram for precipitation data
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily Precipitation",
     ylab = "Relative frequency",
     col= "blue",
     border="white")


#Get total precip across all sites for each year
Annual_Precip <- aggregate(datW$PRCP, by=list(datW$NAME, datW$year), FUN="sum",na.rm=TRUE)
colnames(Annual_Precip) <- c("Site", "Year", "Annual Precipitation")
Annual_Precip

Annual_Precip$Site <- as.factor(Annual_Precip$Site)
Annual_Precip$Site <- as.numeric(Annual_Precip$Site)
Annual_Precip$Site

#Histogram for Aberdeen and Mandan annual precipitation
hist(Annual_Precip$`Annual Precipitation`[Annual_Precip$Site == 1],
     freq=FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average Annual Precipitation",
     ylab = "Relative frequency",
     col= "blue",
     border="white")

hist(Annual_Precip$`Annual Precipitation`[Annual_Precip$Site == 3],
     freq=FALSE,
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average Annual Precipitation",
     ylab = "Relative frequency",
     col= "pink",
     border="white")

pnorm(700,
          mean(Annual_Precip$`Annual Precipitation`[Annual_Precip$Site == 1],na.rm=TRUE),
          sd(Annual_Precip$`Annual Precipitation`[Annual_Precip$Site == 1],na.rm=TRUE))

pnorm(700,
      mean(Annual_Precip$`Annual Precipitation`[Annual_Precip$Site == 3],na.rm=TRUE),
      sd(Annual_Precip$`Annual Precipitation`[Annual_Precip$Site == 3],na.rm=TRUE))

     
