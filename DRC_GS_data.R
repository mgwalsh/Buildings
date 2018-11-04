# DRC GeoSurvey building assessment data setup
# M. Walsh, October 2018

# Required packages
# install.packages(c("downloader","rgdal","jsonlite","raster","leaflet","htmlwidgets","wordcloud")), dependencies=TRUE)
suppressPackageStartupMessages({
  require(downloader)
  require(rgdal)
  require(jsonlite)
  require(raster)
  require(leaflet)
  require(htmlwidgets)
  require(wordcloud)
})

# Data downloads -----------------------------------------------------------
# set working directory
dir.create("DRC_GS", showWarnings = F)
setwd("./DRC_GS")

# download GeoSurvey data
download("https://www.dropbox.com/s/upoylz7dyyjzbkg/DRC_GS_combine.csv.zip?raw=1", "DRC_GS_combine.csv.zip", mode = "wb")
unzip("DRC_GS_combine.csv.zip", overwrite = T)
geos <- read.table("DRC_GS_combine.csv", header = T, sep = ",")
geos <- geos[!duplicated(geos[4:5]), ] ## removes spatial (lat/lon pair) duplicates
geos$BIC <- as.factor(ifelse(geos$CP == "Y" & geos$BP == "Y", "Y", "N")) ## identifies croplands with buildings

# download GADM-L2 shapefile (courtesy: http://www.gadm.org)
download("https://www.dropbox.com/s/ufvu0a6oou1xwhh/DRC_GADM_L2.zip?raw=1", "DRC_GADM_L2.zip", mode = "wb")
unzip("DRC_GADM_L2.zip", overwrite = T)
shape <- shapefile("gadm36_COD_2.shp")

# download raster stack
download("https://www.dropbox.com/s/4ft2u3tumkqc77r/DRC_250m.zip?raw=1", "DRC_250m.zip", mode = "wb")
unzip("DRC_250m.zip", overwrite = T)
glist <- list.files(pattern="tif", full.names = T)
grids <- stack(glist)

# Data setup --------------------------------------------------------------
# attach GADM admin unit names from shape
coordinates(geos) <- ~lon+lat
projection(geos) <- projection(shape)
gadm <- geos %over% shape
geos <- as.data.frame(geos)
geos <- cbind(gadm[ ,c(4,7)], geos)
colnames(geos) <- c("region","territory","survey","time","observer","lat","lon","BP","CP","bloc","BIC")

# Coordinates and number of buildings per quadrat -------------------------
bp <- geos[which(geos$BP == "Y"), ] ## identify quadrats with buildings
bp$bloc <- as.character(bp$bloc)

# coordinates of tagged building locations from quadrats with buildings
c <- fromJSON(bp$bloc[1])
bcoord <- do.call("rbind", c$feature$geometry$coordinates)
for(i in 2:nrow(bp)) {
  c <- fromJSON(bp$bloc[i])
  bcoord_temp <- do.call("rbind", c$feature$geometry$coordinates)
  bcoord <- rbind(bcoord, bcoord_temp)
}
bcoord <- as.data.frame(bcoord) ## vector of coordinates per quadrats with buildings
colnames(bcoord) <- c("lon","lat")

# number of tagged building locations from quadrats with buildings
bcount <- rep(NA, nrow(bp))
for(i in 1:nrow(bp)) {
  t <- fromJSON(bp$bloc[i])
  bcount[i] <- nrow(t$features)
}
bcount ## vector of number of buildings per quadrats with buildings
ba <- geos[which(geos$BP == "N"), ]
ba$bcount <- 0
bp <- cbind(bp, bcount)
geos <- rbind(ba, bp)
geos <- geos[order(geos$time),] ## sort in original sample order

# project GeoSurvey coords to grid CRS
geos.proj <- as.data.frame(project(cbind(geos$lon, geos$lat), "+proj=laea +ellps=WGS84 +lon_0=20 +lat_0=5 +units=m +no_defs"))
colnames(geos.proj) <- c("x","y")
geos <- cbind(geos, geos.proj)
coordinates(geos) <- ~x+y
projection(geos) <- projection(grids)

# extract gridded variables at GeoSurvey locations
geosgrid <- extract(grids, geos)
gsdat <- as.data.frame(cbind(geos, geosgrid)) 
# gsdat <- gsdat[!duplicated(gsdat), ] ## removes any duplicates ... if needed
gsdat <- gsdat[complete.cases(gsdat[ ,c(1:8,12:24)]),] ## removes incomplete cases

# Write data frame --------------------------------------------------------
dir.create("Results", showWarnings = F)
write.csv(bcoord, "./Results/DRC_bcoord.csv", row.names = F)
write.csv(gsdat, "./Results/DRC_gsdat.csv", row.names = F)

# GeoSurvey map widgets ---------------------------------------------------
w <- leaflet() %>%
  setView(lng = mean(gsdat$lon), lat = mean(gsdat$lat), zoom = 6) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addCircleMarkers(gsdat$lon, gsdat$lat, clusterOptions = markerClusterOptions())
w ## plot widget 
saveWidget(w, 'DRC_GS18.html', selfcontained = T) ## save widget

# points of current vaccination
w1 <- leaflet() %>%
  setView(lng = mean(vacc$lon), lat = mean(vacc$lat), zoom = 6) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addCircleMarkers(vacc$lon, vacc$lat, clusterOptions = markerClusterOptions())
w1 ## plot widget 
saveWidget(w1, 'DRC_vacc_pts.html', selfcontained = T) ## save widget

# GeoSurvey contributions -------------------------------------------------
gscon <- as.data.frame(table(gsdat$observer))
set.seed(1235813)
wordcloud(gscon$Var1, freq = gscon$Freq, scale = c(4,0.1), random.order = T)

