#read in weather station file from the data folder
datW <- read.csv("/Users/madelynbeitler/GitHub/ENVSTDATA/Activity5Data/noaa2011124.csv")

#specify that the name column should be a factor
datW$NAME<- as.factor(datW$NAME)

#set up a vector of all names for each level
nameS <- levels(datW$NAME)
nameS

#Made a data frame with precip, year, and site name
#remove NA using na.omit, this removes the days that there was no observation.
datP <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           PRCP=datW$PRCP))

#Get total annual precip(mm)
precip <- aggregate(datW$PRCP, by=list(datW$NAME,datW$year), FUN="sum", na.rm=TRUE)

#Use aggregate to get total annual precipitation
precip <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="sum", na.rm=TRUE)

#Rename the columns
colnames(precip) <- c("NAME","year","totalP")

#add the x column from aggregate looking at the length of observations in each year
precip$ncount <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="length")$x

#make a new dataframe
pr <- precip[precip$ncount >=364, ]

#install package
install.packages("ggplot2")
library(ggplot2)

#base plot
ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ #data for plot
  geom_point()+ #make points at data point
  geom_path()+ #use lines to connect data points
  labs(x="year", y="Annual Precipitation")+
  theme_classic()















