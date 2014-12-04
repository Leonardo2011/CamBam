#########Creating a city-based data frame with different themes#################
###By region###
#quickly rename for impending coding
PreGTD.all <- PreGTD

##do another subset of war (upon which other subsets will be created)
subset.peace <- subset(PreGTD.all, select=c(iyear, region_txt, GTD.city, targtype1, HUMscale, TUPscale, Extra.WAR.In, Intra.WAR, Inter.WAR), Extra.WAR.In=="0" & Intra.WAR=="0" & Inter.WAR=="0")

#this is the top cities attacked not during war
peace.all <- count(subset.peace, ("region_txt"))
#rename the vector and remove old
peace.all$Peacetime <- peace.all$freq
peace.all <- peace.all[,-2]

##do another subset of war (upon which other subsets will be created)
subset.war <- subset(PreGTD.all, select=c(iyear, region_txt, GTD.city, targtype1, HUMscale, TUPscale, Extra.WAR.In, Intra.WAR, Inter.WAR), Extra.WAR.In=="1")

#this is the top cities attacked not during war
wartime.all <- count(subset.war, ("region_txt"))
#rename the vector and remove old
wartime.all$Wartime <- wartime.all$freq
wartime.all <- wartime.all[,-2]


#just infrastructure subset during wartime
subset.infrastr <- subset(PreGTD.all, select=c(iyear, region_txt, merge, GTD.city, targtype1, HUMscale, TUPscale, Extra.WAR.In, Intra.WAR, Inter.WAR), TUPscale=="7")

#this is infrastructure attacked in cities
infrastr.all <- count(subset.infrastr, ("region_txt"))
#rename and remove old vector
infrastr.all$Infrastructure <- infrastr.all$freq
infrastr.all <- infrastr.all[,-2]

#how many killed and injured subset
subset.humscale <- subset(PreGTD.all, select=c(iyear, region_txt, merge, GTD.city, targtype1, HUMscale, TUPscale, Extra.WAR.In, Intra.WAR, Inter.WAR))

#human scale (killed and injured) in cities
humscale.all <- count(subset.humscale, ("region_txt"))
#rename and remove old vector
humscale.all$KilledAndInjured <- humscale.all$freq
humscale.all <- humscale.all[,-2]

##how many civilians
subset.civilian <- subset(PreGTD.all, select=c(iyear, region_txt, merge, GTD.city, targtype1, HUMscale, TUPscale, Extra.WAR.In, Intra.WAR, Inter.WAR), (TUPscale=="9" | TUPscale=="5"))

#count those civilian-esque attacks in cities
civilian.all <- count(subset.civilian, ("region_txt"))
#rename and remove old vector
civilian.all$CivilianLife <- civilian.all$freq
civilian.all <- civilian.all[,-2]

##how many against military and police
subset.milpol <- subset(PreGTD.all, select=c(iyear, region_txt, merge, GTD.city, targtype1, HUMscale, TUPscale, Extra.WAR.In, Intra.WAR, Inter.WAR), (TUPscale=="0" | TUPscale=="2"))

#count military and police attacks in cities
milpol.all <- count(subset.milpol, ("region_txt"))
#rename and remove old vector
milpol.all$MilitaryAndPolice <- milpol.all$freq
milpol.all <- milpol.all[,-2]


PreGTD.alles <- merge(wartime.all, peace.all, by=c("region_txt"), all=TRUE)
PreGTD.alles <- merge(PreGTD.alles, infrastr.all, by=c("region_txt"), all.x=TRUE)
PreGTD.alles <- merge(PreGTD.alles, civilian.all, by=c("region_txt"), all.x=TRUE)
PreGTD.alles <- merge(PreGTD.alles, milpol.all, by=c("region_txt"), all.x=TRUE)
PreGTD.alles <- merge(PreGTD.alles, humscale.all, by=c("region_txt"), all.x=TRUE)

table.pregtd.alles <- table(PreGTD.alles$region_txt, PreGTD.alles$Peacetime)
*# of attacks per world region--multicolored lines
*human damage per region--code is in assignment 2

levels(PreGTD$region_txt)


library(knitr)

install.packages("xtable")

library(xtable)
library(maptools)

install.packages("pander")
library(pander)


library(ascii)
install.packages("ascii")

**Need some plotted table--

PreGTD.alles <- PreGTD.alles[order(-PreGTD.alles$Peacetime), ]
#plot the top 10 in peacetime
library(ggplot2)
peacetop10 <- PreGTD.alles[1:10,]
peacetop10 <- within(peacetop10, merge <- reorder(merge, as.numeric(Peacetime), FUN = min))
ggplot(peacetop10, aes(x = merge, y = Peacetime)) + geom_histogram(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1))


###Prepare PreGTD for plotting on multi-level regional 1970-2013###
library(ggplot2)
levels(PreGTD$region_txt)
levels(PreGTD.all$region_txt)
levels(PreGTD.all$region_txt) <- c("", "Oceania", "Central America", "Central Asia", 
                             "East Asia", "Eastern Europe", "MENA", "North America", "Russia", "South America", 
                             "South Asia", "Southeast Asia", "Sub-Saharan Africa", "Western Europe")
pregtd.alltest <- subset(PreGTD.all, select=(c(iyear, region_txt, GTD.city)))
pregtd.alltest <- data.frame(year=pregtd.alltest$iyear, region=pregtd.alltest$region_txt, city=pregtd.alltest$GTD.city)
regionyearplot <- ggplot(pregtd.alltest, aes(x=year, y=count(*), color=region)) + geom_line() + geom_point() + xlab("Year") + ylab("Attacks") + ggtitle("Total Attacks By Region From 1970-2013")
regionyearplot + theme(axis.text.x = element_text(angle = 90), complete = TRUE)

?plyr


regionTotal <- ddply(PreGTD.all, ~region_txt, count)

ggplot(regionTotal, aes(x = region_txt, y = count, fill = region_txt)) +
  geom_bar(stat="identity", show_guide=FALSE) +
  coord_flip() +
  ggtitle("Terrorist Attacks in World Regions Since 1970") +
  xlab("") +
  ylab("# of Attacks") +
  scale_y_continuous("count") +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(face="bold"))

regionyearcount <- ddply(PreGTD.all, region_txt ~ iyear, FUN="sum", "nattacks")


ggplot(pregtd.alltest, aes(x = year, y = year, color = region)) +
  geom_line() +
  geom_point() +
  xlab("Year") + 
  ggtitle("Number of Terrorist Attacks in World Regions Since 1970") + 
  ylab("# of Attacks") + 
  theme(legend.justification = c(0,1), legend.position = c(0,1), legend.title = element_blank(),
        plot.title = element_text(face="bold")) +
  guides(col = guide_legend(ncol = 2))




###now for humscale###
pregtd.alltest2 <- subset(PreGTD.all, select=(c(iyear, region_txt, GTD.city, HUMscale)))
pregtd.alltest2$HUMscale <- as.integer(pregtd.alltest2$HUMscale)
humyearplot <- qplot(HUMscale, data=pregtd.alltest2, geom = "freqpoly", color = region_txt, group=region_txt)
humyearplot + theme(axis.text.x = element_text(angle = 90), complete = TRUE)



PlotTUP <- qplot(factor(year), data=Plotframe, geom = "freqpoly", color = TUP, group = TUP, ylab= "sum of attacks", xlab= "year",
                 main="Sum of attacks per year
                 with indication for the transition of collecting entities:
                 PIGS until 1997, CETIS until 2008, ISVG until 2011 and START since 2011") + geom_vline(data=intersepz, 
                                                                                                        




civilian.all <- count(subset.civilian, ("region_txt"))
#rename and remove old vector
civilian.all$CivilianLife <- civilian.all$freq
civilian.all <- civilian.all[,-2]






####Plot from the second assignment of multiple lines###
#prepare a data frame for plotting
Plotframe <- data.frame(year=GTD$iyear, TUP=TUPforPlot, PROP=PROPforPlot, HUM=GTD$HUMscale)

#as we want to get a sense if different collection issues merged in the GTD, we will split plots with
# horizontal lines that indicate an transition on collecting entity (PIGS, CETIS, ISVG, START)
intersepz=data.frame(date=as.numeric(c("27", "38", "41")), date2=as.numeric(c("1997", "2009", "2011")), event=c("PGIS Data Collection End", "CETIS Data Collection End", "ISVG Data Collection End"))

# the first plot shows the count of attack on our categoriesed targets
PlotTUP <- qplot(factor(year), data=Plotframe, geom = "freqpoly", color = TUP, group = TUP, ylab= "sum of attacks", xlab= "year",
                 main="Sum of attacks per year
                 with indication for the transition of collecting entities:
                 PIGS until 1997, CETIS until 2008, ISVG until 2011 and START since 2011") + geom_vline(data=intersepz, 
                                                                                                        mapping=aes(xintercept=date))
PlotTUP + theme(axis.text.x = element_text(angle = 90), complete = TRUE)


# the second plot shows the count of attack in different categories of economic damage
PlotPROP <- qplot(factor(year), data=Plotframe, geom = "freqpoly", color = PROP, group = PROP, ylab= "sum of attacks", xlab= "year",
                  main="Sum of attacks per year
                  with indication for the transition of collecting entities:
                  PIGS until 1997, CETIS until 2008, ISVG until 2011 and START since 2011") + geom_vline(data=intersepz,
                                                                                                         mapping=aes(xintercept=date))
PlotPROP + theme(axis.text.x = element_text(angle = 90), complete = TRUE)


