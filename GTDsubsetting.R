###Try to subset the GTD and make a table with special characteristics###

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
PreGTD_in_Memory <- getURL("https://rawgit.com/LBRETZIN/UrbanTerror/Restruc/TerrorData/Pregtd.csv", ssl.verifypeer=0L, followlocation=1L)
writeLines(PreGTD_in_Memory,'Pregtd.csv')
rm(PreGTD_in_Memory)

#Load the Pre-Analysis Global Terrorism Database
PreGTD <- read.csv("Pregtd.csv", header=TRUE)
PreGTD <-PreGTD[order(-PreGTD$eventid, na.last=TRUE) , ]

# make numeric what we need
PreGTD$SP.URB.TOTL <- as.numeric(PreGTD$SP.URB.TOTL)
PreGTD$MAX.URB.TOTL <- as.numeric(PreGTD$MAX.URB.TOTL)
PreGTD$pop <- as.numeric(PreGTD$pop)
PreGTD$WC.UC.dist.km <- as.numeric(PreGTD$WC.UC.dist.km)
PreGTD$iyear <- as.numeric(PreGTD$iyear)
PreGTD$capital <- as.numeric(PreGTD$capital)
PreGTD$coastalMC <- as.numeric(PreGTD$coastalMC)
PreGTD$Extra.WAR.In <- as.numeric(PreGTD$Extra.WAR.In)
PreGTD$Extra.WAR.Out <- as.numeric(PreGTD$Extra.WAR.Out)
PreGTD$Intra.WAR <- as.numeric(PreGTD$Intra.WAR)
PreGTD$Inter.WAR <- as.numeric(PreGTD$Inter.WAR)

###################messing around with tabling##################
#top city on humans attacked total (with region in there)
testpregtd<-aggregate(HUMscale~region_txt+merge, data=PreGTD, FUN=sum)
humscale.city<- testpregtd[order(-testpregtd$HUMscale),]

#top region
testpregtd<-aggregate(HUMscale~region_txt, data=PreGTD, FUN=sum)
humscale.region<- testpregtd[order(-testpregtd$HUMscale),]

#this is just city and humscale most
testpregtd<-aggregate(HUMscale~merge, data=PreGTD, FUN=sum)
humscale.topcities<-testpregtd[order(-PreGTD$HUMscale), ]

##this is the way to count on and order a vector
#we look at just how many attacks happened in a city and count
testpregtd2 <- data.frame(table(PreGTD$merge))
city.attacks.most <- testpregtd2[order(-testpregtd2$Freq), ]

#optional:ordering code
city.attacks.most.war <- xx[order(-xx$freq), ]
#############################################################

#########Creating a city-based data frame with different themes#################
#quickly rename for impending coding
PreGTD.all <- PreGTD

##do another subset of war (upon which other subsets will be created)
subset.peace <- subset(PreGTD.all, select=c(iyear, region_txt, merge, city, targtype1, HUMscale, TUPscale, Extra.WAR.In, Intra.WAR, Inter.WAR), Extra.WAR.In=="0" & Intra.WAR=="0" & Inter.WAR=="0")

#this is the top cities attacked not during war
peace.all <- count(subset.peace, ("merge"))
#rename the vector and remove old
peace.all$Peacetime <- peace.all$freq
peace.all <- peace.all[,-2]

##do another subset of war (upon which other subsets will be created)
subset.war <- subset(PreGTD.all, select=c(iyear, region_txt, merge, city, targtype1, HUMscale, TUPscale, Extra.WAR.In, Intra.WAR, Inter.WAR), Extra.WAR.In=="1")

#this is the top cities attacked not during war
wartime.all <- count(subset.wartime, ("merge"))
#rename the vector and remove old
wartime.all$Wartime <- wartime.all$freq
wartime.all <- wartime.all[,-2]


#just infrastructure subset during wartime
subset.infrastr <- subset(PreGTD.all, select=c(iyear, region_txt, merge, city, targtype1, HUMscale, TUPscale, Extra.WAR.In, Intra.WAR, Inter.WAR), TUPscale=="7")

#this is infrastructure attacked in cities
infrastr.all <- count(subset.infrastr, ("merge"))
#rename and remove old vector
infrastr.all$Infrastructure <- infrastr.all$freq
infrastr.all <- infrastr.all[,-2]

#how many killed and injured subset
subset.humscale <- subset(PreGTD.all, select=c(iyear, region_txt, merge, city, targtype1, HUMscale, TUPscale, Extra.WAR.In, Intra.WAR, Inter.WAR))

#human scale (killed and injured) in cities
humscale.all <- count(subset.humscale, ("merge"))
#rename and remove old vector
humscale.all$KilledAndInjured <- humscale.all$freq
humscale.all <- humscale.all[,-2]

##how many civilians
subset.civilian <- subset(PreGTD.all, select=c(iyear, region_txt, merge, city, targtype1, HUMscale, TUPscale, Extra.WAR.In, Intra.WAR, Inter.WAR), (TUPscale=="9" | TUPscale=="5"))

#count those civilian-esque attacks in cities
civilian.all <- count(subset.civilian, ("merge"))
#rename and remove old vector
civilian.all$CivilianLife <- civilian.all$freq
civilian.all <- civilian.all[,-2]

##how many against military and police
subset.milpol <- subset(PreGTD.all, select=c(iyear, region_txt, merge, city, targtype1, HUMscale, TUPscale, Extra.WAR.In, Intra.WAR, Inter.WAR), (TUPscale=="0" | TUPscale=="2"))

#count military and police attacks in cities
milpol.all <- count(subset.milpol, ("merge"))
#rename and remove old vector
milpol.all$MilitaryAndPolice <- milpol.all$freq
milpol.all <- milpol.all[,-2]


PreGTD.alles <- merge(wartime.all, peace.all, by=c("merge"), all=TRUE)
PreGTD.alles <- merge(PreGTD.alles, infrastr.all, by=c("merge"), all.x=TRUE)
PreGTD.alles <- merge(PreGTD.alles, civilian.all, by=c("merge"), all.x=TRUE)
PreGTD.alles <- merge(PreGTD.alles, milpol.all, by=c("merge"), all.x=TRUE)
PreGTD.alles <- merge(PreGTD.alles, humscale.all, by=c("merge"), all.x=TRUE)


##various decreasing sorts
PreGTD.alles <- PreGTD.alles[order(-PreGTD.alles$Peacetime), ]
#plot the top 10 in peacetime
library(ggplot2)
peacetop10 <- PreGTD.alles[1:10,]
peacetop10 <- within(peacetop10, merge <- reorder(merge, as.integer(Peacetime), FUN = min))
ggplot(peacetop10, aes(x = merge, y = Peacetime)) + geom_histogram(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1))

#now wartime
PreGTD.alles <- PreGTD.alles[order(-PreGTD.alles$Wartime), ]
#plot the top 10 in wartime
wartop10 <- PreGTD.alles[1:10,]
wartop10 <- within(wartop10, merge <- reorder(merge, as.integer(Wartime), FUN = min))
ggplot(wartop10, aes(x = merge, y = Wartime)) + geom_histogram(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1))

#sort for top infrastructure cities
PreGTD.alles <- PreGTD.alles[order(-PreGTD.alles$Infrastructure), ]
#plot top 10 infrastructure
infratop10 <- PreGTD.alles[1:10,]
infratop10 <- within(infratop10, merge <- reorder(merge, as.integer(Infrastructure), FUN = min))
ggplot(infratop10, aes(x = merge, y = Infrastructure)) + geom_histogram(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1))

#sort for top Civilian Life attacked cities
PreGTD.alles <- PreGTD.alles[order(-PreGTD.alles$CivilianLife), ]
#plot top 10 infrastructure
civtop10 <- PreGTD.alles[1:10,]
civtop10 <- within(civtop10, merge <- reorder(merge, as.integer(CivilianLife), FUN = min))
ggplot(civtop10, aes(x = merge, y = CivilianLife)) + geom_histogram(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1))

#sort for top 10 cities with military and police targeted
PreGTD.alles <- PreGTD.alles[order(-PreGTD.alles$MilitaryAndPolice), ]
#plot top 10 infrastructure
mipotop10 <- PreGTD.alles[1:10,]
mipotop10 <- within(mipotop10, merge <- reorder(merge, as.integer(MilitaryAndPolice), FUN = min))
ggplot(mipotop10, aes(x = merge, y = MilitaryAndPolice)) + geom_histogram(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1))

#sort for top 10 humscale
PreGTD.alles <- PreGTD.alles[order(-PreGTD.alles$KilledAndInjured), ]
#plot top 10 infrastructure
humtop10 <- PreGTD.alles[1:10,]
humtop10 <- within(humtop10, merge <- reorder(merge, as.integer(KilledAndInjured), FUN = min))
ggplot(humtop10, aes(x = merge, y = KilledAndInjured)) + geom_histogram(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1))


##just for an idea in mind...combine the PreGTD.all and PreGTD.alles
PreGTD.allestest <- merge(PreGTD.alles, PreGTD.all, by=c("merge"), all=TRUE)

###what's the deal with these facets?
PreGTD.allestest$HUMscale[is.na(PreGTD.allestest$HUMscale)] <- 0

AttackedCities <- ggplot(PreGTD.allestest, aes(x = iyear, y = "HUMscale", colour = region_txt)) + stat_summary(fun.y = sum, geom = "point")
AttackedCities + theme(axis.text.x = element_text(angle = 90, vjust = 0.25), legend.position = "none") + facet_wrap(~region_txt)

##how many of these attacks take place in coastal cities?
subset.coastal <- subset(PreGTD, select=c(iyear, region_txt, merge, city, coastalMC, targtype1, HUMscale, TUPscale, Extra.WAR.In, Intra.WAR, Inter.WAR), coastalMC=="1")
coastalcount<-data.frame(table(count(subset.coastal, c("merge", "coastalMC"))))
coastalcount<-order(coastalcount[coastalcount$Freq],)
