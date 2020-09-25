########## ACTIVITY 4 ##########

#Read the data
datB <- read.csv("/Users/madelynbeitler/GitHub/ENVSTDATA/Activity4Data/a04/beaver_dam.csv")
head(datB)

#Make a scatter plot
#pch decides what symbol the points are.
plot(datB$dams.n, datB$area.h, 
     pch = 19,
     col = "dodgerblue",
     ylab = "Surface water area (ha)",
     xlab =  "Number of beaver dams")

#set up regression
dam.mod <- lm(datB$area.ha ~ datB$dams.n)

#get standardized residuals
dam.res <- rstandard(dam.mod)

#Check that the residuals are normally distributed
#set up qq plot --> visual assessment of normality 
qqnorm(dam.res)

#add qq line
qqline(dam.res)

#Shapiro-Wilk normality test
shapiro.test(dam.res)

#Check for equal variance 
#make residual plot
plot(datB$dams.n, dam.res, 
     pch = 19,
     xlab = "beaver damns", 
     ylab = "standardized residual")

#add a horizontal line at zero
abline(h=0)

#Print results of regression
summary(dam.mod)

#make plot of beaver dams and surface water
plot(datB$dams.n, datB$area.h, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab =  "Number of beaver dams")

#add regression line
#make line width thicker
abline(dam.mod, lwd=2)

#Read phenology data
pheno <- read.csv("/Users/madelynbeitler/GitHub/ENVSTDATA/Activity4Data/a04/red_maple_pheno.csv")

#set up panel of plots with one row and two columns
par(mfrow=c(1,2))
plot(pheno$Tmax,pheno$doy, 
     pch = 20, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Maximum temperature (C)")

plot(pheno$Prcp,pheno$doy, 
     pch = 20, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Precipitation (mm)")

#### Question 3 ####
par(mfrow=c(1,2))
plot(pheno$Lat,pheno$doy, 
     pch = 20, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Latitude)")

plot(pheno$elev,pheno$doy, 
     pch = 20, 
     col = "mediumaquamarine",
     ylab = "Day of leaf out",
     xlab =  "Elevation")

par(mfrow=c(1,2))
plot(pheno$Tmax,pheno$doy, 
     pch = 20, 
     col = "snow4",
     ylab = "Day of leaf out",
     xlab =  "Max temperature")

pheno$siteDesc <- as.factor(pheno$siteDesc)
plot(pheno$siteDesc,pheno$doy, 
     pch = 20, 
     col = "yellow",
     ylab = "Day of leaf out",
     xlab =  "Urban or Rural")

#to stop using the par argument
dev.off()

#Set up covariance plots
plot( ~  pheno$Lat + pheno$Tmax+ pheno$Tmin +pheno$Prcp + pheno$elev + pheno$siteDesc)

#Set up the regression
pheno$urID <- ifelse(pheno$siteDesc == "Urban",1,0)
pheno$urID

mlr <- lm(pheno$doy ~  pheno$Tmax  + pheno$Prcp + pheno$elev + pheno$urID)
mlr

#Calculate fitted values from the regression line for each observation.
mlFitted <- fitted(mlr)
mlFitted

#Print summary 
summary(mlr)

#qqnorm to test for normality
pheno.res <- rstandard(mlr)
qqnorm(pheno.res)
qqline(pheno.res)

plot(mlFitted, pheno.res)


