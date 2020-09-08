#Activity 2 

heights <- c(3, 2, 3)

heights[1]

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



     
