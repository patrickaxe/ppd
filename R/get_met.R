# SILO met producer 
# Author: Di, Binyin

# 
# https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?format=near&station=15540&radius=100000
# http://www.bom.gov.au/climate/data/lists_by_element/stations.txt
# 
# 
# 
# library dependency: patrickaxe/ppd
# Example input
# lon <- 152.34
# lat <- -27.54
# startdate<-20200409
# enddate<-20200527


get_met <- function(lat, lon, startdate, enddate) {
  require(devtools)
  # install_github("patrickaxe/ppd")
  require(ppd)
  stationID<-nearestStations(lon = lon, lat = lat)[1,1]
  df<-getPPD(as.character(stationID),as.character(startdate) ,as.character(enddate), 
             "vagoja9737@firmjam.com")
  stationName<-STATIONS[which(STATIONS$SITE_NO == as.character(stationID)),3 ]
  fileName<-paste(stationName,"_", stationID,"_",startdate,"_",enddate,"_",".met")
  ppd2apsimMET(stationID,df,fileName)
}





