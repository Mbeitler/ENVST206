#Activity 2 

heights <- c(3, 2, 3)

datW <- read.csv("/Users/madelynbeitler/ENVSTDATA/Activity2Data/noaa2011124.csv")

datW$PRCP_CM <- datW$PRCP/10

mean(datW$PRCP_CM, na.rm=TRUE)
