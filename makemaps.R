ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("foreign", "car", "RCurl", "ggplot2", "WDI", "httr", "iterators", "dplyr", "plyr", "stargazer",
              "XML", "maps", "ggmap", "Imap", "geosphere", "maptools", "rgeos", "foreach", "DataCombine")
ipak(packages)
rm(packages)
rm(ipak)

#download a limited GTD we created for this assignment (5MB instead of 100MB file)
PreGTD_in_Memory <- getURL("https://rawgit.com/LBRETZIN/UrbanTerror/master/TerrorData/Pregtd.csv", ssl.verifypeer=0L, followlocation=1L)
writeLines(PreGTD_in_Memory,'Pre.GTD.csv')
rm(PreGTD_in_Memory)
PreGTD <- read.csv("Pre.GTD.csv", header=TRUE)

WC.UC.dist_in_Memory <- getURL("https://rawgit.com/LBRETZIN/UrbanTerror/master/Cache/WC.UC.dist.csv", ssl.verifypeer=0L, followlocation=1L)
writeLines(WC.UC.dist_in_Memory,'WC.UC.dist.csv')
rm(WC.UC.dist_in_Memory)
WC.UC.dist <- read.csv("WC.UC.dist.csv", header=TRUE)

install.packages("ggmap")

qmap('houston', zoom = 13)


# only violent crimes
violent_crimes <- subset(crime,
                         offense != "auto theft" & offense != "theft" &
                           offense != "burglary")

# order violent crimes
violent_crimes$offense <- factor(violent_crimes$offense,
                                 levels = c("robbery", "aggravated assault", "rape", "murder"))
# Set up base map
HoustonMap <- qmap("houston", zoom = 14,  
                   source = "stamen", maptype = "toner",
                   legend = "topleft")

# Add points
FinalMap <- HoustonMap +
  geom_point(aes(x = lon, y = lat, colour = offense,
                 size = offense),
             data = violent_crimes) +
  guides(size = guide_legend(title = 'Offense'),
         colour = guide_legend(title = 'Offense'))
