##########  ACTIVITY 3  ##########

#import data for lemmings
ch4 <- read.csv("/Users/madelynbeitler/GitHub/ENVSTDATA/Activity3Data/lemming_herbivory.csv")

#Make a box plot
#First make herbivory data into factor.
ch4$herbivory <- as.factor(ch4$herbivory)

#Make box plot
#X ~ y ...or... dependent variable ~ independent variable
plot(ch4$CH4_Flux ~ ch4$herbivory)

#Conduct shaprio test to check for normality
#Must do it on both groups
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])

shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])

#Conduct Bartlett test to check if variance are different between groups.
bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)

#T-test, 2 sample, test difference between groups.
t.test(ch4$CH4_Flux ~ ch4$herbivory)

########## QUESTION 1 ##########
#Get means
mean(ch4$CH4_Flux)

#T-test, 2 sample, test difference between groups.
#Null hypothesis: There is no difference in means of where lemmings graze and where they don't.
t.test(ch4$CH4_Flux ~ ch4$herbivory)

#read in insect data
datI <- read.csv("/Users/madelynbeitler/GitHub/ENVSTDATA/Activity3Data/insect_richness.csv")

#Make urbanName a factor
datI$urbanName <- as.factor(datI$urbanName)

#Test for normality 
shapiro.test(datI$Richness[datI$urbanName == "Suburban"])

shapiro.test(datI$Richness[datI$urbanName == "Developed"])

shapiro.test(datI$Richness[datI$urbanName == "Dense"])

shapiro.test(datI$Richness[datI$urbanName == "Natural"])

#Conduct Bartlett test to check if variance are different between groups.
bartlett.test(datI$Richness ~ datI$urbanName)

#specify model for species richness and urban type
in.mod <- lm(datI$Richness ~ datI$urbanName)

#run the ANOVA
in.aov <- aov(in.mod)

#print out ANOVA table
summary(in.aov)

#run Tukey HSD
tukeyT <- TukeyHSD(in.aov)

#view results
tukeyT

#make a plot
#make axes labels smaller than usual to fit on plot using cex.axis 
plot(tukeyT, cex.axis=0.75)

tapply(datI$Richness, datI$urbanName, "mean")

#set up contigency table
species <- matrix(c(18,8,15,32), ncol=2, byrow = TRUE) 
colnames(species) <- c("Not protected", "Protected")
rownames(species) <- c("Declining", "Stable/Increase")

#make a mosaic plot with an informative title and axes labels
mosaicplot(species, xlab="population status", ylab="legal protection",
           main="Legal protection impacts on populations")

#Conduct a chi-squared test
chisq.test(species)


