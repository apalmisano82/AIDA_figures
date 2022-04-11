# Install library if not installed -------------------------------------####
if (!('rcarbon' %in% installed.packages())){
  install.packages('rcarbon')
}

# Install library if not installed -------------------------------------####
if (!('doParallel' %in% installed.packages())){
  install.packages('doParallel')
}

# Install library if not installed -------------------------------------####
if (!('rgdal' %in% installed.packages())){
  install.packages('rgdal')
}

# Install library if not installed -------------------------------------####
if (!('raster' %in% installed.packages())){
  install.packages('raster')
}

if (!('maptools' %in% installed.packages())){
  install.packages('maptools')
}

if (!('maps' %in% installed.packages())){
  install.packages('maps')
}

# Install library if not installed -------------------------------------####
if (!('cartography' %in% installed.packages())){
  install.packages('cartography')
}

## Add libraries (you may need to install these first)
library(rcarbon)
library(doParallel)
library(rgdal)
library(raster)
library(maptools)
library(maps)

## Load data
mydates <- read.csv("csv/dates.csv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8",na.strings=c("NA",""),strip.white=TRUE)

# Load study area polygon (from GIS-friendly, ESRI-format shapefiles  to "SpatialPolygonsDataFrame" objects in R)
ita<-readOGR("shp/italy_regions.shp", layer="italy_regions")
north<-ita[ita$ID==1,]
central<-ita[ita$ID==2,]
south<-ita[ita$ID==3,]
sicily<-ita[ita$ID==4,]
sardinia<-ita[ita$ID==5,]

# Turn the whole date dataset into a spatial object than can be plotted on a map
# First, make sure the coordinates are treated as numbers
mydates$Longitude <- as.numeric(mydates$Longitude) 
mydates$Latitude <- as.numeric(mydates$Latitude)
# Then throw out any without coordinates
datesp <- mydates[!is.na(mydates$Longitude) & !is.na(mydates$Latitude),]
# Then designate the coordinate columns
coordinates(datesp) <- ~Longitude+Latitude
# Then stipulate that the coordinate system of this data is the same as the polygon data we have (i.e. unprojected latlon)
proj4string(datesp) <- proj4string(north)<-proj4string(central)<-proj4string(south)<-proj4string(sardinia)<-proj4string(sicily)

#Spatial query: selection of points (datesp) located within region (=1)
query_north<-over(datesp,north)
query_central<-over(datesp,central)
query_south<-over(datesp,south)
query_sicily<-over(datesp,sicily)
query_sardinia<-over(datesp,sardinia)

#convert vector into dataframe
north<-as.data.frame(datesp)
central<-as.data.frame(datesp)
south<-as.data.frame(datesp)  
sicily<-as.data.frame(datesp)
sardinia<-as.data.frame(datesp)

#add column for storing spatial query's results
north["query"]<-NA
central["query"]<-NA
south["query"]<-NA
sicily["query"]<-NA
sardinia["query"]<-NA
#update the column "query"
north$query<-query_north
central$query<-query_central
south$query<-query_south
sicily$query<-query_sicily
sardinia$query<-query_sardinia
#subsetting the dates located in the study area
mydates_north<-subset(north, north$query==1)
mydates_central<-subset(central, central$query==2)
mydates_south<-subset(south, south$query==3)
mydates_sicily<-subset(sicily, sicily$query==4)
mydates_sardinia<-subset(sardinia, sardinia$query==5)
#add column for defining region
mydates_north["Region"]<-1
mydates_central["Region"]<-2
mydates_south["Region"]<-3
mydates_sicily["Region"]<-4
mydates_sardinia["Region"]<-5

#aggregate all Radiocarbon dates
mydates<-rbind(mydates_north,mydates_central,mydates_south,mydates_sardinia, mydates_sicily)
#select radiocarbon dates from short-lived samples 
shortlived<-mydates[grep(paste(c("antler","bone","seed","seeds", "tissue","fruit", "bome", "tooth","horn", "twig","grain", "collagen", "hair", "leather"),collapse="|"), mydates$Material),] # subselect shortlived samples

# Turn the whole date dataset into a spatial object than can be plotted on a map
# First, make sure the coordinates are treated as numbers
mydates$Longitude <- as.numeric(mydates$Longitude) 
mydates$Latitude <- as.numeric(mydates$Latitude)
mydates_north$Longitude <- as.numeric(mydates_north$Longitude) 
mydates_north$Latitude <- as.numeric(mydates_north$Latitude)
mydates_central$Longitude <- as.numeric(mydates_central$Longitude) 
mydates_central$Latitude <- as.numeric(mydates_central$Latitude)
mydates_south$Longitude <- as.numeric(mydates_south$Longitude) 
mydates_south$Latitude <- as.numeric(mydates_south$Latitude)
mydates_sicily$Longitude <- as.numeric(mydates_sicily$Longitude) 
mydates_sicily$Latitude <- as.numeric(mydates_sicily$Latitude)
mydates_sardinia$Longitude <- as.numeric(mydates_sardinia$Longitude) 
mydates_sardinia$Latitude <- as.numeric(mydates_sardinia$Latitude)
# Then throw out any without coordinates
datesp<-mydates[!is.na(mydates$Longitude) & !is.na(mydates$Latitude),]
datesp_north <- mydates_north[!is.na(mydates_north$Longitude) & !is.na(mydates_north$Latitude),]
datesp_central <- mydates_central[!is.na(mydates_central$Longitude) & !is.na(mydates_central$Latitude),]
datesp_south <- mydates_south[!is.na(mydates_south$Longitude) & !is.na(mydates_south$Latitude),]
datesp_sicily <- mydates_sicily[!is.na(mydates_sicily$Longitude) & !is.na(mydates_sicily$Latitude),]
datesp_sardinia <- mydates_sardinia[!is.na(mydates_sardinia$Longitude) & !is.na(mydates_sardinia$Latitude),]
# Then designate the coordinate columns
coordinates(datesp) <- ~Longitude+Latitude
lonlat <- CRS("+init=epsg:4326") # LatLon WGS84
proj4string(datesp) <- lonlat
countries<-readOGR("shp/eur_states_lo.shp", layer="eur_states_lo")
datesp <- spTransform(datesp, proj4string(countries)) # UTM WGS84, zone 37 North

coordinates(datesp_north) <- ~Longitude+Latitude
coordinates(datesp_central) <- ~Longitude+Latitude
coordinates(datesp_south) <- ~Longitude+Latitude
coordinates(datesp_sicily) <- ~Longitude+Latitude
coordinates(datesp_sardinia) <- ~Longitude+Latitude
# Then stipulate that the coordinate system of this data is the same as the polygon data we have (i.e. unprojected latlon)
proj4string(datesp_north) <- proj4string(datesp_central)<-proj4string(datesp_south)<-proj4string(datesp_sardinia)<-proj4string(datesp_sicily)

#select radiocarbon dates from short-lived samples 
shortlived<-mydates[grep(paste(c("antler","bone","seed","seeds", "tissue","fruit", "bome", "tooth","horn", "twig","grain", "collagen", "hair", "leather"),collapse="|"), mydates$Material),] # subselect shortlived samples


#Plot spatial distribution radiocarbon dates and palaeoclimate proxies
pdf(file="Fig1.pdf", width=8, height=7)
par(mar=c(0, 0, 0, 0)) #c(bottom, left, top, right)
plot(countries,col="grey75", border="white", lwd=0.2, xlim = c(11, 15),ylim = c(36,47))
dem<-raster("raster/dem_ita.tif")
lakes<-readOGR("shp/lakes.shp", layer="lakes")
image(dem, col= colorRampPalette(c("grey60", "grey36", "grey18", "grey12", "grey0"))(20), add=T, legend=F)
plot(lakes, col="white", border="NA", add=T)
points(datesp[datesp$Region==1,], col="red", pch=19, cex=0.25)
points(datesp[datesp$Region==2,], col="cyan", pch=19, cex=0.25)
points(datesp[datesp$Region==3,], col="green2", pch=19, cex=0.25)
points(datesp[datesp$Region==4,], col="brown", pch=19, cex=0.25)
points(datesp[datesp$Region==5,], col="blue", pch=19, cex=0.25)
legend(5.50,41.0, legend=c("North","Central","South","Sicily", "Sardinia"), pch=c(19,19,19,19,19,17), col=c("red","cyan","green2", "brown","blue"), bty="n", cex=0.8)
xpos <- 19
ypos <- 37.5
scalesize <- 0.5
lines(c(xpos,xpos),c(ypos-scalesize,ypos+scalesize),col="black")
polygon(c(xpos,xpos-(scalesize/12),xpos,xpos+(scalesize/12),xpos),c((ypos+scalesize/1.5),(ypos+scalesize/1.5),(ypos+scalesize),(ypos+scalesize/1.5),(ypos+scalesize/1.5)), col="black")
text(xpos, ypos, "N", cex=0.7, col="black", font=2)
map.scale(x=16, y=37, ratio=FALSE, relwidth=0.10,cex=0.5)
dev.off()


############################# SPD radiocarbon dates ###############################################
## General SPD parameters
nsim <- 1000 # number of actual simulations 
ncores <- 6 # multi-core processing (set higher if available)
runm <- 50 #smoothing of SPDs
binh <- 50 #bin clustering
realstartBP <- 10000
realendBP <-2500
bracket <- 1000
workingstartBP <- realstartBP+bracket
workingendBP <- realendBP-bracket
if (workingendBP<0){ workingendBP <- 0 }


## Calibrate dates 
alldates <- calibrate(x=mydates$CRA, errors=mydates$Error, calCurves='intcal20', method="standard", normalised=FALSE, ncores=ncores, calMatrix=TRUE)
shortlivedates <- calibrate(x=shortlived$CRA, errors=shortlived$Error, calCurves='intcal20', method="standard", normalised=FALSE, ncores=ncores, calMatrix=TRUE)

## SUMMED PROBABILITY DISTRIBUTIONS (SPDs)
#The whole Italy
bins <- binPrep(sites=mydates$SiteID, ages=mydates$CRA, h=binh)
allspd<- spd(x=alldates, bins=bins, timeRange=c(workingstartBP,workingendBP), datenormalised=FALSE, runm=runm)

#short-lived samples
bins_shortlived <- binPrep(sites=shortlived$SiteID, ages=shortlived$CRA, h=binh)
allspd_shortlived <- spd(x=shortlivedates, bins=bins_shortlived, timeRange=c(workingstartBP,workingendBP), datenormalised=FALSE, runm=runm)

#Calculate the Pearson correlation coefficient between the SPD (unnormalised) obtained calibrating all radiocarbon samples and the SPD from short-lived radiocarbon dates
cor.test(allspd$grid$PrDens, allspd_shortlived$grid$PrDens, method = "pearson")
#the two curves are highly correlated. r=0.92, p-value <0.01


## Plot SPDs 
pdf(file="Fig2.pdf", width=8, height=5)
plot(allspd)
plot(allspd_shortlived, type="standard", fill.p="darkgreen", add=T)
dev.off()

