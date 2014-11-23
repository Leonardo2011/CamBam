ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("foreign", "car", "RCurl", "ggplot2", "WDI", "httr", "iterators", "dplyr", "plyr",
              "XML", "maps", "ggmap", "Imap", "geosphere", "maptools", "rgeos", "foreach", "stargazer")
ipak(packages)
rm(packages)
rm(ipak)

#download a limited GTD we created for this assignment (5MB instead of 100MB file)
PreGTD_in_Memory <- getURL("https://rawgit.com/LBRETZIN/UrbanTerror/master/PreAnalysis/pregtd.csv", ssl.verifypeer=0L, followlocation=1L)
writeLines(PreGTD_in_Memory,'pregtd.csv')
rm(PreGTD_in_Memory)

#Load the Pre-Analysis Global Terrorism Database
PreGTD <- read.csv("pregtd.csv", header=TRUE)
PreGTD <-PreGTD[order(-PreGTD$eventid, na.last=TRUE) , ]

str(PreGTD)

# make numeric what we need
PreGTD$EN.URB.LCTY.UR.ZS <- as.numeric(PreGTD$EN.URB.LCTY.UR.ZS)
PreGTD$SP.URB.TOTL <- as.numeric(PreGTD$SP.URB.TOTL)
PreGTD$SP.RUR.TOTL <- as.numeric(PreGTD$SP.RUR.TOTL)
PreGTD$MAX.URB.TOTL <- as.numeric(PreGTD$MAX.URB.TOTL)
PreGTD$pop <- as.numeric(PreGTD$pop)
#more
PreGTD$CUC.dist.km <- as.numeric(PreGTD$CUC.dist.km)
PreGTD$iyear <- as.numeric(PreGTD$iyear)
PreGTD$capital <- as.numeric(PreGTD$capital)
PreGTD$costalMC <- as.numeric(PreGTD$costalMC)
PreGTD$Extra.WAR.In <- as.numeric(PreGTD$Extra.WAR.In)
PreGTD$Extra.WAR.Out <- as.numeric(PreGTD$Extra.WAR.Out)
PreGTD$Intra.WAR <- as.numeric(PreGTD$Intra.WAR)
PreGTD$Inter.WAR <- as.numeric(PreGTD$Inter.WAR)

# take a look at the structure of our data, make sure we have numerics
str(PreGTD)

# start to put together tables--this is a test
library(stargazer)

stargazer(PreGTD, type = "text", title="Descriptive statistics", digits=1, out="table1.txt")

#....noice

#subset the data and take out the non-numeric vectors
PreGTD.light <- subset(PreGTD, select = c(iyear, pop, capital, CUC.dist.km, PROPscale, HUMscale, EN.URB.LCTY.UR.ZS, EN.URB.MCTY, EN.URB.MCTY.TL.ZS, SP.URB.GROW, SP.URB.TOTL, SP.URB.TOTL.IN.ZS, EN.POP.DNST, EN.RUR.DNST, SP.RUR.TOTL, SP.RUR.TOTL.ZG, SP.RUR.TOTL.ZS, Extra.WAR.In, Extra.WAR.Out, Intra.WAR, Inter.WAR))

str(PreGTD.light)

stargazer(PreGTD.light, type = "text", title="Descriptive statistics", digits=1, out="table1.txt")
stargazer(PreGTD.light, type = "html", title="Descriptive statistics", digits=1, out="table1.html")
stargazer(PreGTD, type = "html", title="Descriptive statistics", digits=1, out="table2.html")

summary(PreGTD.light)
table(PreGTD.light) # hmm this looks strange...so many NAs

#correlation matrix using stargazer is the goal--lets try this out
m1 <- lm(iyear ~ CUC.dist.km, data=PreGTD.light)
m2 <- lm(iyear ~ CUC.dist.km + pop, data=PreGTD.light)
m3 <- lm(iyear ~ CUC.dist.km + pop + capital, data=PreGTD.light)
m4 <- lm(iyear ~ CUC.dist.km + pop + capital, data=PreGTD.light)
m5 <- lm(iyear ~ CUC.dist.km + pop + capital + PROPscale, data=PreGTD.light)
m6 <- lm(iyear ~ CUC.dist.km + pop + capital + PROPscale + HUMscale, data=PreGTD.light)
m7 <- lm(iyear ~ CUC.dist.km + pop + capital + PROPscale + HUMscale + EN.URB.LCTY.UR.ZS, data=PreGTD.light)
m8 <- lm(iyear ~ CUC.dist.km + pop + capital + PROPscale + HUMscale + EN.URB.LCTY.UR.ZS + EN.URB.MCTY, data=PreGTD.light)
m9 <- lm(iyear ~ CUC.dist.km + pop + capital + PROPscale + HUMscale + EN.URB.LCTY.UR.ZS + EN.URB.MCTY + EN.URB.MCTY.TL.ZS, data=PreGTD.light)
m10 <- lm(iyear ~ CUC.dist.km + pop + capital + PROPscale + HUMscale + EN.URB.LCTY.UR.ZS + EN.URB.MCTY + EN.URB.MCTY.TL.ZS +SP.URB.GROW, data=PreGTD.light)
m11 <- lm(iyear ~ CUC.dist.km + pop + capital + PROPscale + HUMscale + EN.URB.LCTY.UR.ZS + EN.URB.MCTY + EN.URB.MCTY.TL.ZS + SP.URB.GROW + SP.URB.TOTL + EN.POP.DNST, data=PreGTD.light)
m12 <- lm(iyear ~ CUC.dist.km + pop + capital + PROPscale + HUMscale + EN.URB.LCTY.UR.ZS + EN.URB.MCTY + EN.URB.MCTY.TL.ZS + SP.URB.GROW + SP.URB.TOTL + EN.POP.DNST + EN.RUR.DNST, data=PreGTD.light)
m13 <- lm(iyear ~ CUC.dist.km + pop + capital + PROPscale + HUMscale + EN.URB.LCTY.UR.ZS + EN.URB.MCTY + EN.URB.MCTY.TL.ZS + SP.URB.GROW + SP.URB.TOTL + EN.POP.DNST + EN.RUR.DNST + SP.RUR.TOTL, data=PreGTD.light)
m14 <- lm(iyear ~ CUC.dist.km + pop + capital + PROPscale + HUMscale + EN.URB.LCTY.UR.ZS + EN.URB.MCTY + EN.URB.MCTY.TL.ZS + SP.URB.GROW + SP.URB.TOTL + EN.POP.DNST + EN.RUR.DNST + SP.RUR.TOTL + SP.RUR.TOTL.ZG, data=PreGTD.light)
m15 <- lm(iyear ~ CUC.dist.km + pop + capital + PROPscale + HUMscale + EN.URB.LCTY.UR.ZS + EN.URB.MCTY + EN.URB.MCTY.TL.ZS + SP.URB.GROW + SP.URB.TOTL + EN.POP.DNST + EN.RUR.DNST + SP.RUR.TOTL + SP.RUR.TOTL.ZG + SP.RUR.TOTL.ZS, data=PreGTD.light)
m16 <- lm(iyear ~ CUC.dist.km + pop + capital + PROPscale + HUMscale + EN.URB.LCTY.UR.ZS + EN.URB.MCTY + EN.URB.MCTY.TL.ZS + SP.URB.GROW + SP.URB.TOTL + EN.POP.DNST + EN.RUR.DNST + SP.RUR.TOTL + SP.RUR.TOTL.ZG + SP.RUR.TOTL.ZS + Extra.WAR.In, data=PreGTD.light)
m17 <- lm(iyear ~ CUC.dist.km + pop + capital + PROPscale + HUMscale + EN.URB.LCTY.UR.ZS + EN.URB.MCTY + EN.URB.MCTY.TL.ZS + SP.URB.GROW + SP.URB.TOTL + EN.POP.DNST + EN.RUR.DNST + SP.RUR.TOTL + SP.RUR.TOTL.ZG + SP.RUR.TOTL.ZS + Extra.WAR.In + Extra.WAR.Out, data=PreGTD.light)
m18 <- lm(iyear ~ CUC.dist.km + pop + capital + PROPscale + HUMscale + EN.URB.LCTY.UR.ZS + EN.URB.MCTY + EN.URB.MCTY.TL.ZS + SP.URB.GROW + SP.URB.TOTL + EN.POP.DNST + EN.RUR.DNST + SP.RUR.TOTL + SP.RUR.TOTL.ZG + SP.RUR.TOTL.ZS + Extra.WAR.In + Extra.WAR.Out + Intra.WAR, data=PreGTD.light)
m19 <- lm(iyear ~ CUC.dist.km + pop + capital + PROPscale + HUMscale + EN.URB.LCTY.UR.ZS + EN.URB.MCTY + EN.URB.MCTY.TL.ZS + SP.URB.GROW + SP.URB.TOTL + EN.POP.DNST + EN.RUR.DNST + SP.RUR.TOTL + SP.RUR.TOTL.ZG + SP.RUR.TOTL.ZS + Extra.WAR.In + Extra.WAR.Out + Intra.WAR + Inter.WAR, data=PreGTD.light)


#toying around with an indicator variable
PreGTD.light$mcity.high <- (PreGTD.light$pop > 100000)
m20 <- glm(mcity.high ~ pop, family=binomial(link="logit"), data=PreGTD.light)

#try an output table and rename the variables for the table
stargazer(m1, m2, m3, m4,m5, m6, m7, m8,m9, m10, m11, m12,m13, m14, m15, m16,m17, m18, m19, m20, title="Urban Correlation Matrix", type="text",dep.var.labels=c("Time","Megacity Attack Rating=1"),covariate.labels=c("Distance From Closest Urban Center","Population","Capital", "Property Scale","People Killed Scale", "Population in the largest city (% of urban population)","Population in urban agglomerations of more than 1 million","Population in urban agglomerations of more than 1 million (% of total population)","Urban population growth (annual %)","Urban population", "Urban population (% of total)","Population density (people per sq. km of land area)","Rural population density (rural population per sq. km of arable land)","Rural population", "Rural population growth (annual %)","Rural population (% of total population)", "During War Time", "During Peace time", "Civil War", "Between Country War", out="models2.txt"))


# this is a non output table           
cor(PreGTD)
as.matrix(cor(PreGTD.light))


testpregtd<-aggregate(HUMscale~merge, data=PreGTD, FUN=sum)
testpregtd<-testpregtd[order(-PreGTD$HUMscale), ]

str(testpregtd)

index<-with(PreGTD, order(merge, HUMscale))
PreGTD[index, ]


testpregtd.orderhum<- order(testpregtd$HUMscale, testpregtd$merge, decreasing=TRUE)
testpregtd.orderhum

?ddply

install.packages("plyr")

library(plyr)

city.attacks <- ddply(PreGTD, c("iyear", "region_txt", "city", "pop"), summarise, count(iyear))

stargazer(city.attacks,digits=2, type="text", out="attacks.txt")

city.attacks

set.seed(1)
count.attacks <- PreGTD(iyear = rep(1970:2013), count)
print(city.attacks)

PreGTD

ddply(PreGTD, "city", transform, total.count=count(city))

set.seed(1)
d <- PreGTD(iyear = rep(1970:2013, each = 10), count)
print(d)


library(plyr)
ddply(PreGTD, "iyear", sum(costal) {
  mean.count <- mean(PreGTD$costal)
})

maxkills<-ddply(PreGTD, ~region_txt, summarize, maxvioperyr = max(HUMscale, na.rm = TRUE))

maxkills <- sort(maxkills, decreasing=TRUE, na.last=NA)
maxkills
