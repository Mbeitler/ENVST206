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



