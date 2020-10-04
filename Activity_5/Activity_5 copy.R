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

#make datafram with temp, year
datT <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           TMAX=datW$TMAX))


#Get total annual precip(mm)
precip <- aggregate(datW$PRCP, by=list(datW$NAME,datW$year), FUN="sum", na.rm=TRUE)

#temp
tmax <- aggregate(datW$TMAX, by=list(datW$NAME,datW$year), FUN="mean", na.rm=TRUE)

#Use aggregate to get total annual precipitation
precip <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="sum", na.rm=TRUE)

#temp
tmax <- aggregate(datT$TMAX, by=list(datT$NAME,datT$year), FUN="mean", na.rm=TRUE)

#Rename the columns
colnames(precip) <- c("NAME","year","totalP")
colnames(tmax) <- c("NAME","year","tmax")

#add the x column from aggregate looking at the length of observations in each year
precip$ncount <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="length")$x

#temp
tmax$ncount <- aggregate(datT$TMAX, by=list(datT$NAME,datT$year), FUN="length")$x

#make a new dataframe
pr <- precip[precip$ncount >=364, ]

tm <- tmax[tmax$ncount >=364, ]

#look at only livermore california and morrisville new york preciptiation
ca <- pr[pr$NAME == nameS[2], ]
ny <- pr[pr$NAME == nameS[5], ]
nd <- pr[pr$NAME == nameS[3], ]

tny <- tm[tm$NAME == nameS[5], ]
tnd <- tm[tm$NAME == nameS[3], ]

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
plot(tny$year, tny$tmax,
     type = "b",
     pch = 19,
     ylab = "Annual Temperature (celcius)",
     xlab = "Year",
     yaxt = "n")

#plot ND
points(tnd$year, tnd$tmax,
       type = "b",
       pch = 18,
       cex = 1.5,
       col="steelblue1")

#new axis
plot(tny$year, tny$tmax,
     type = "b",
     pch = 19,
     ylab = "Annual Temperature (celcius",
     xlab = "Year", 
     yaxt = "n",
     xaxt = "n",
     ylim =c(8, 16),
     xlim =c(1930, 2020)) 

#add y axis
axis(2, seq(0,25, by=1), las=2 )
axis(1, seq(1930,2020, by=5), las=1)

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

ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ #data for plot
  geom_point()+ #make points at data point
  geom_path()+ #use lines to connect data points
  labs(x="year", y="Annual Precipitation") #make axis labels

ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ #data for plot
  geom_point()+ #make points at data point
  geom_path()+ #use lines to connect data points
  labs(x="year", y="Annual Precipitation")+ #make axis labels
  theme_classic() #change plot theme

ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+
  geom_point(alpha=0.5)+
  geom_path(alpha=0.5)+
  labs(x="year", y="Annual Precipitation")+
  theme_classic()+
  scale_color_manual(values = c("indianred3","mediumblue", "gold", "aquamarine3","grey37"))

ggplot(data = datW, aes(x=NAME, y=TMIN))+ #look at daily tmin
  geom_violin(fill=rgb(0.933,0.953,0.98))+ #add a violin plot with blue color
  geom_boxplot(width=0.2,size=0.25, fill="grey90")+ #add grey boxplots and make them about 20% smaller than normal with 25% thinner lines than normal
  theme_classic() #git rid of ugly gridlines

sub <- datW[datW$NAME == nameS[4] & datW$ year == 1974,]

#specify date format
#%Y means a four number year 
#- indicates that the date uses dashes to seperate
#%m means month
#%d means day
sub$DATE <- as.Date(sub$DATE,"%Y-%m-%d")

ggplot(data=sub, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximimum temperature (C)")

ggplot(data=sub, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily precipitation (mm)")

#new plots for Morrisville 
subWA <- datW[datW$NAME == nameS[1] & datW$ year == 1974,]
subWA$DATE <- as.Date(subWA$DATE,"%Y-%m-%d")

ggplot(data=subWA, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximimum temperature (C)")

ggplot(data=subWA, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily precipitation (mm)")

#Question 9 
ggplot(data = datW, aes(x=NAME, y=TMIN))+ #look at daily tmin
  geom_violin(fill=rgb(0.933,0.953,0.98))+ #add a violin plot with blue color
  geom_boxplot(width=0.2,size=0.25, fill="grey90")+ #add grey boxplots and make them about 20% smaller than normal with 25% thinner lines than normal
  theme_classic() #git rid of ugly gridlines
dev.off()
sub <- datW[datW$NAME == nameS[1] & datW$ year > 1999,]
sub$year <- as.factor(sub$year)


#make violin plot
ggplot(data = sub, aes(x=year, y=TMIN))+ #look at daily tmin
  geom_violin(fill=rgb(0.933,0.953,0.98))+ #add a violin plot with blue color
  geom_boxplot(width=0.2,size=0.25, fill="grey90")+ #add grey boxplots and make them about 20% smaller than normal with 25% thinner lines than normal
  theme_classic()+ #git rid of ugly gridlines
  labs(x="year", y="TMIN (degrees celcius)")










