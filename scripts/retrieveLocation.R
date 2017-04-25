library(ggplot2)
library(dplyr)
library(tidyr)
library(ggmap)
library(rworldmap)
github<-"https://raw.githubusercontent.com/ashishjain1988/STAT585-FinalProject/master/data/"
matches <- read.csv(paste0(github,"matches.csv"), stringsAsFactors = FALSE)
cities<-unlist(matches %>% distinct(city) %>% filter(city != ""))
points<-geocode(cities)
row.names(points)<-cities
points<-points[,2:3]
coord2loc <- function(points)
{
  countriesSP <- getMap(resolution='low')
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  indices = over(pointsSP, countriesSP)
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
  indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}
countries<-coord2loc(as.matrix(points))
points[,3]<-countries
points["Kochi",]$V3<-"India"
write.csv(points,"location_mapping.csv")