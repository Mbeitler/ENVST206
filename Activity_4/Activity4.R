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







