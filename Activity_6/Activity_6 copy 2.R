install.packages(c("sp","rgdal","dplyr"))

#package for vector data
library(sp)
#package for reading in spatial data
library(rgdal)
#data manangement package
library(dplyr)

#read in shapefiles
#readOGR in rgdal does this
#glaciers in 1966
g1966 <- readOGR("/Users/madelynbeitler/GitHub/ENVSTDATA/Activity6Data/GNPglaciers/GNPglaciers_1966.shp")

plot(g1966)

g1966@proj4string

#glaciers in 2015
g2015 <- readOGR("/Users/madelynbeitler/GitHub/ENVSTDATA/Activity6Data/GNPglaciers/GNPglaciers_2015.shp")

str(g2015)

#map the glaciers filling in the polygons with light blue and making the borders grey
plot(g1966, col = "lightblue2", border="grey50")

#data stores all accompanying info/measurements for each spatial object
head(g2015@data)

g1966@proj4string

#check glacier names
g1966@data$GLACNAME

g2015@data$GLACNAME

#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

#lets combine area, first work with a smaller data frame
gdf66 <- data.frame(GLACNAME = g1966@data$GLACNAME,
                    area66 = g1966@data$Area1966)

gdf15 <- data.frame(GLACNAME = g2015@data$GLACNAME,
                    area15 = g2015@data$Area2015)

#join all data tables by glacier name

gAll <- full_join(gdf66,gdf15, by="GLACNAME")

gAll

#calculate the % change in area from 1966 to 2015
gAll$gdiff <- ((gAll$area66-gAll$area15)/gAll$area66)*100

#install package
install.packages("ggplot2")
library(ggplot2)

ggplot(data = gAll, aes(x = area66, y= gdiff))+
  geom_point()+ #make points at data point
  labs(x="Glacier Area (km squared)", y="% Change in Area")+
  theme_classic()

#join data with the spatial data table and overwrite into spatial data table 
g1966@data <- left_join(g1966@data, gAll, by="GLACNAME")

#use spplot to shade polygons based on the % change of labels
#first argument is the spatial object
#second is the column in of data to display with the different colors
#add a title using main
#col changes the color of the borders. This argument sets them to transparent
spplot(g1966, "gdiff", main="% change in area", col="transparent")

#look at the Vulture glacier in 1966
vulture66 <- g1966[g1966@data$GLACNAME == "Vulture Glacier",]
plot(vulture66, main = "Vulture Glacier in 1966", col="slategray")

#Mean of % loss
mean(gAll$gdiff)

#stdev of % loss
sd(gAll$gdiff)

#find largest glacier loss
max(gAll$gdiff)

#Find smallest glacier lost
min(gAll$gdiff)

boulder66 <- g1966[g1966@data$GLACNAME == "Boulder Glacier",]
boulder15 <- g2015[g2015@data$GLACNAME == "Boulder Glacier",]

#Plot both
plot(boulder66, main = "Boulder Glacier Retreat from 1966 to 2015", col="slategrey")
plot(boulder15, col= "lightcyan1", add=TRUE)

legend("topleft", 
       legend = c("1966", "2015"), 
       col = c("slategrey", 
               "lightcyan1"), 
       pch = c(15), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

#New variables for pumpelly
pumpelly66 <- g1966[g1966@data$GLACNAME == "Pumpelly Glacier",]
pumpelly15 <- g2015[g2015@data$GLACNAME == "Pumpelly Glacier",]

#Plot Pumpelly Glacier 
plot(pumpelly66, main = "Pumpelly Glacier Retreat from 1966 to 2015", col="slategrey")
plot(pumpelly15, col= "lightcyan1", add=TRUE)

legend("topleft", 
       legend = c("1966", "2015"), 
       col = c("slategrey", 
               "lightcyan1"), 
       pch = c(15), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))






