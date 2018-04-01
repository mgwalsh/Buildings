# Malawi 250m resolution building distribution map data setup 
# M. Walsh, March 2018

# Required packages
# install.packages(c("downloader","rgdal","raster","leaflet","htmlwidgets","wordcloud")), dependencies=TRUE)
suppressPackageStartupMessages({
  require(downloader)
  require(rgdal)
  require(raster)
  require(jsonlite)
  require(leaflet)
  require(htmlwidgets)
  require(wordcloud)
})

# Data downloads -----------------------------------------------------------
# set working directory
dir.create("MW_GS250", showWarnings = F)
setwd("./MW_GS250")

# download GeoSurvey data
download("https://www.dropbox.com/s/utnsml3szjlyg5b/mw_buildings_2018.csv.zip?raw=1", "mw_buildings_2018.csv.zip", mode = "wb")
unzip("mw_buildings_2018.csv.zip", overwrite = T)
geos <- read.table("mw_buildings_2018.csv", header = T, sep = ",")
geos$BIC <- as.factor(ifelse(geos$CP == "Y" & geos$BP == "Y", "Y", "N")) ## identifies croplands with buildings

# download GADM-L3 shapefile (courtesy: http://www.gadm.org)
download("https://www.dropbox.com/s/o5g7lk5669d55eb/MWI_adm3.zip?raw=1", "MWI_adm3.zip", mode = "wb")
unzip("MWI_adm3.zip", overwrite = T)
shape <- shapefile("MWI_adm3.shp")

# download Malawi Gtifs and stack in raster
download("https://www.dropbox.com/s/sl8yogkpqan0qr9/MW_250m_2018.zip?raw=1", "MW_250m_2018.zip", mode = "wb")
unzip("MW_250m_2018.zip", overwrite = T)
glist <- list.files(pattern="tif", full.names = T)
grids <- stack(glist)

# Count number of buildings per quadrat -----------------------------------
bp <- geos[which(geos$BP == "Y"), ] ## identify quadrats with buildings
bp$bloc <- as.character(bp$bloc)

# Counting tagged building locations from quadrats with buildings
n <- rep(NA, nrow(bp))
for(i in 1:nrow(bp)) {
  t <- fromJSON(bp$bloc[i])
  n[i] <- nrow(t$features)
}
n ## vector of number of buildings per quadrat with buildings
ba <- geos[which(geos$BP == "N"), ]
ba$n <- 0
bp <- cbind(bp, n)
geos <- rbind(ba, bp)

# Data setup ---------------------------------------------------------------
# attach GADM-L3 admin unit names from shape
coordinates(geos) <- ~lon+lat
projection(geos) <- projection(shape)
gadm <- geos %over% shape
geos <- as.data.frame(geos)
geos <- cbind(gadm[ ,c(5,7)], geos)

# project GeoSurvey coords to grid CRS
geos.proj <- as.data.frame(project(cbind(geos$lon, geos$lat), "+proj=laea +ellps=WGS84 +lon_0=20 +lat_0=5 +units=m +no_defs"))
colnames(geos.proj) <- c("x","y")
geos <- cbind(geos, geos.proj)
coordinates(geos) <- ~x+y
projection(geos) <- projection(grids)

# extract gridded variables at GeoSurvey locations
geosgrid <- extract(grids, geos)
gsdat <- as.data.frame(cbind(geos, geosgrid)) 
gsdat <- gsdat[!duplicated(gsdat), ] ## removes any duplicates
gsdat <- gsdat[which(gsdat$DOWS > 0), ] ## select observations located on land
gsdat$user <- sub("@.*", "", as.character(gsdat$user)) ## shortens observer ID's

# Write output file -------------------------------------------------------
dir.create("Results", showWarnings = F)
write.csv(gsdat, "./Results/MW_gsdat.csv", row.names = F)

# GeoSurvey map widget ----------------------------------------------------
# render map
w <- leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addCircleMarkers(gsdat$lon, gsdat$lat, clusterOptions = markerClusterOptions())
w ## plot widget 

# save widget
saveWidget(w, "MW_GS250.html", selfcontained = T)

# GeoSurvey contributions -------------------------------------------------
gscon <- as.data.frame(table(gsdat$user))
set.seed(1235813)
wordcloud(gscon$Var1, freq = gscon$Freq, scale = c(3,0.1), random.order = T)
