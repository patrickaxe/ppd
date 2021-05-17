#' West Dale (WA) met producer
#' 
#' Designed for West Dale (WA) dataset (up to date) from avachat.info
#' 
#' 
#' @param get in "get" to execute the function
#' 
#' 
#' @Depends data.table, tidyverse, plyr
#' 
#' @export
#' @author patrickaxe




westdale_met<-function(get) {
  if (as.character(get) == "get") {
    require(data.table)
    require(tidyverse)
    require(plyr)
    df<-fread("https://avachat.info/WDDAILY.txt", header = FALSE)
    geoinfo<-data.frame(head(read.delim("https://avachat.info/WDDAILY.txt"))[2,])
    df<-df[!(df$V19=="24"),]
    df$maxt <- as.numeric(df$V4) 
    df$mint <- as.numeric(df$V3)
    meanT<-aggregate(df[, maxt:mint], list(df$V2), mean)
    meanT$avgt<-(meanT$maxt+meanT$mint)/2
    amp <-max(meanT$avgt)-min(meanT$avgt)
    df$day<-row.names(df)
    df$year<-"2021"
    df$V9<-as.numeric(df$V9)
    df$radn <- apply(df[,9,drop=F], 1, function(x) x/1000)
    df$rain <- df$V10
    dfsub <- df %>%
      select(year,day, radn, maxt,mint, rain)
    tav<-data.frame(apply(dfsub[,4:5, drop = F],2, mean)) 
    tav<-(tav[1,1]+tav[2,1])/2
    unit<-cbind(year="()", day="()", radn = "MJ/m^2", maxt = "(oC)", mint = "(oC)", rain ="mm")
    dfsub<-rbind(unit,dfsub)
    geoinfo<-tail(data.frame(unlist(lapply(geoinfo, strsplit, split = " ", fixed = T))))
    Latitude <- paste("Latitude = ", geoinfo[4,])
    Longitude <-paste("Longitude = ",  geoinfo[6,])
    tav <-paste("tav = ", tav , "(oC) ! annual average ambient temperature")
    amp <- paste("amp = ", amp, "(oC) ! annual amplitude in mean monthly temperature")
    header<-rbind("!title = WestDale", NA, "[weather.met.weather]",Latitude, Longitude,tav, amp)
    rownames(header) <- NULL
    colnames(header) <-"X1"
    header<-data.frame(header)
    coldfsub<-data.frame(t(colnames(dfsub)))
    out<-rbindlist(list(coldfsub,dfsub))
    header[nrow(header)+1,]<-NA
    out<-rbind.fill(header, out) 
    write.table(out, file = "Westdale.met", na="",
            row.names = FALSE,
            col.names = FALSE,
            append = TRUE, quote=FALSE)
  }
}
