install.packages(c("sp","rgdal","dplyr", "ggmap", "rgeos"))

#package for vector data
library(sp)
#package for reading in spatial data
library(rgdal)
#data manangement package
library(dplyr)
library(raster)
library(ggplot2)
#map package
library(ggmap)

sea_ice_all <- readOGR("/Users/madelynbeitler/GitHub/ENVSTDATA/PROJECT/sea_ice_all")
sea_ice_1980 <- sea_ice_all[sea_ice_all$year == 1980, ]
sea_ice_2018 <- sea_ice_all[sea_ice_all$year == 2018, ]
sea_ice_1999 <- sea_ice_all[sea_ice_all$year == 1999, ]

plot(sea_ice_all, col = "lightblue2", border="grey50")
area(sea_ice_all)
sum(area(sea_ice_all))
sd(area(sea_ice_all))

plot(sea_ice_1980, col = "lightblue2", border="grey50")
area(sea_ice_1980)
sum(area(sea_ice_1980))
sd(area(sea_ice_1980))

plot(sea_ice_2018, main = "Arctic Sea Ice Area in 1980 and 2018", col = "grey49", border="grey50", add=TRUE)
area(sea_ice_2018)
sum(area(sea_ice_2018))
sd(area(sea_ice_2018))

legend("bottomleft", 
       legend = c("1980", "2018"), 
       col = c("lightblue2", 
               "grey50"), 
       pch = c(15), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

dat.sea_ice <- data.frame(year = sea_ice_all@data$year,
                           FID = sea_ice_all@data$FID)

#all plot code: 
plot(sea_ice_1980, main = "Arctic Sea Ice Area in 1980 and 2018", col = "lightblue2", border="grey50")
###plot(sea_ice_1999,col = "lightcyan", border="grey50", add=TRUE)
plot(sea_ice_2018, col = "grey49", border="grey50", add=TRUE)
legend("bottomleft", 
       legend = c("1980", "2018"), 
       col = c("lightblue2", 
               "grey50"), 
       pch = c(15), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))


#Add country outlines
world <- readOGR("/Users/madelynbeitler/GitHub/ENVSTDATA/PROJECT/WORLD/")
plot(world)

countriesC <- crop(world, extent(-180,180,50,90)) 
countriesP <- spTransform(countriesC, sea_ice_all@proj4string)
plot(countriesP)
plot(sea_ice_1980, main = "Arctic Sea Ice Area in 1980 and 2018", col = "lightblue2", border="grey50", add=TRUE)










