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

