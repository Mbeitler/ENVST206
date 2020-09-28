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

#look at only livermore california and morrisville new york preciptiation
ca <- pr[pr$NAME == nameS[2], ]
ny <- pr[pr$NAME == nameS[5], ]
nd <- pr[pr$NAME == nameS[3], ]

#install package
install.packages("ggplot2")
library(ggplot2)

#base plot
ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ #data for plot
  geom_point()+ #make points at data point
  geom_path()+ #use lines to connect data points
  labs(x="year", y="Annual Precipitation")+
  theme_classic()

#make a plot of california precip
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n")

#add y axis
#arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
#las = 2 changes the labels to be read in horizontal direction
axis(2, seq(200,800, by=200), las=2 )

#add ny
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")

#Plot with new axes ranges 
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(0, 1600))

#add y axis
axis(2, seq(0,1600, by=400), las=2 )

#add NY
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")

#add legend
legend("topleft", #position
       c("California", "New York"), #labels
       col= c("tomato3", "slateblue1"), #colors
       pch=19, #point shape
       lwd=1, #line thickness 1, anytime both point & line arguments are given both will be drawn
       bty="n") #always use this argument otherwise an ugly box is drawn


# Question 3 
#plot NY
plot(ny$year, ny$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year",
     yaxt = "n")

#plot ND
points(nd$year, nd$totalP,
       type = "b",
       pch = 18,
       cex = 1.5,
       col="steelblue1")

#new axis
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(0, 1000)) 

#add y axis
axis(2, seq(0,1000, by=100), las=2 )

#add legend
legend("topleft", #position
       c("New York", "North Dakota"), #labels
       col= c("black", "steelblue1"), #colors
       pch= c(19, 18), #point shape
       lwd=1, 
       bty="n") 

install.packages("ggplot2")

#Run this every time you start a ne R session.
library(ggplot2)













