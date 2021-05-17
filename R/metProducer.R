#' Simple SILO met producer
#' Simple SILO met producer
#' 
#' 
#' @include apsimMET.R STATIONS.R
#' @param lat is the latitude of a location
#' @param lon is the longtitude of a location
#' @param startdate date format in YYYYmmdd
#' @param enddate date format in YYYmmdd
#' 
#' @author patrickaxe
#' 
#' 
#' @export


metProducer <- function(lat, lon, startdate, enddate) {
  stationID<-nearestStations(lon = lon, lat = lat)[1,1]
  df<-getPPD(as.character(stationID),as.character(startdate) ,as.character(enddate), 
             "vagoja9737@firmjam.com")
  stationName<-STATIONS[which(STATIONS$SITE_NO == as.character(stationID)),3 ]
  fileName<-paste(stationName,"_", stationID,"_",startdate,"_",enddate,"_",".met")
  ppd2apsimMET(stationID,df,fileName)
}





