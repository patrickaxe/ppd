#' Callington (SA) met producer
#' 
#' Designed for Callington dataset (up to date) from Wildeye API
#' 
#' 
#' @param UserName as "QLDUni" by default
#' @param PassWord as "Q1dUn!" by default
#' @param startdate as "17/May/2021%2000:00:00" by default
#' 
#' @return output
#' 
#' @export
#' 
#' @author patrickaxe


callington_met<-function(UserName,PassWord,startdate){
  
  require(tidyverse)
  require(xml2)
  require(rccdates)
  require(gsubfn)
  require(XML)
  require(plyr)
  require(data.table)
  
  
  url<-paste0("https://www.outpostcentral.com/api/2.0/dataservice/mydata.aspx","?userName=",UserName, "&password=",PassWord,"&dateFrom=", startdate)
  # httr::set_config( config( ssl_verifypeer = 0L ))
  # g<-GET(paste0(url, "?userName=", UserName,  "&password=", PassWord, "&dateFrom=", startdate))
  xml_df <- read_xml(url, encoding = "UTF-8")
  
  namelist<-xml_text(xml_find_all(xml_df, "//opdata:input/name"))
  # https://cran.r-project.org/web/packages/xml2/vignettes/modification.html
  grandchildren<-xml_children(xml_children(xml_children(xml_children(xml_df)[[1]])[[1]])[8])
  
  names(grandchildren) <- namelist
  
  temp<-grandchildren[[ namelist[!grepl("#", namelist) & !grepl("Pressure", namelist) & !grepl("Humidity", namelist)& !grepl("Signal", namelist) & !grepl("Delta T", namelist) & !grepl("Counter 2", namelist) & !grepl("Rainfall", namelist) & !grepl("Battery", namelist) & !grepl("ADC1", namelist) & !grepl("Decagon VP4", namelist)]
  ]]
  
  
  temp<-xml_children(temp)[[6]]
  temp <- xmlParse(temp) 
  # rootnode<-xmlRoot(test) 
  # rootnode[1]
  temp<-xmlToDataFrame(temp)
  
  
  rain<-grandchildren[["Rainfall"]]
  rain<-xml_children(rain)[[6]]
  rain <- xmlParse(rain) 
  rain<-xmlToDataFrame(rain)
  
  
  radn<-grandchildren[["#0 Solar Radiation W/m2"]]
  radn<-xml_children(radn)[[6]]
  radn <- xmlParse(radn) 
  radn<-xmlToDataFrame(radn)
  
  
  systime<-as.Date(Sys.time())
  daydif<-as.numeric(systime-as.Date(paste0(as.year(Sys.time()),"-01-01")))
  
  
  temp<-temp %>% 
    mutate(Date = as.Date(date)) %>%
    mutate(day_til_now = dense_rank(desc(Date))) %>%
    mutate(day = daydif+2-day_til_now)
  
  rain<-rain %>% 
    mutate(Date = as.Date(date)) %>%
    mutate(day_til_now = dense_rank(desc(Date))) %>%
    mutate(day = daydif+2-day_til_now)
  
  
  radn<-radn %>% 
    mutate(Date = as.Date(date)) %>%
    mutate(day_til_now = dense_rank(desc(Date))) %>%
    mutate(day = daydif+2-day_til_now)
  
  
  
  
  temp<-temp[!(temp$Date== "2021-05-17" | temp$Date==as.character(systime)),]
  
  
  mint<-aggregate(value ~ day, temp, min) %>%
    dplyr::rename("mint" = "value")
  maxt<-aggregate(value ~ day, temp, max) %>%
    dplyr::rename("maxt" = "value")
  
  
  
  rain<-rain[!(rain$Date== "2021-05-17" | rain$Date==as.character(systime)),]
  raindata <-aggregate(as.numeric(value)~day, rain, sum) %>%
    dplyr::rename("rain" = "as.numeric(value)")
  
  
  radn<-radn[!(radn$Date== "2021-05-17" | radn$Date==as.character(systime)),]
  radn$value<-as.numeric(radn$value)*15*60/1000000
  radndata<-aggregate(radn$value,by = list(radn$day), FUN = sum) %>%
    dplyr::rename(c("radn" = "x", "day"="Group.1"))
  
  
  dfsub<-cbind(year="2021",radndata, maxt,mint,raindata) %>%
    subset(, select=which(!duplicated(names(.)))) 
  
  
  meanT<-(as.numeric(dfsub$maxt)+as.numeric(dfsub$mint))/2
  tav<-mean(meanT)
  
  amp <- max(as.numeric(dfsub$maxt))-min(as.numeric(dfsub$mint))
  
  
  
  
  unit<-cbind(year="()", day="()", radn = "MJ/m^2", maxt = "(oC)", mint = "(oC)", rain ="mm")
  dfsub<-rbind(unit,dfsub)
  
  Latitude <- paste("Latitude = ", "-35.141070")
  Longitude <-paste("Longitude = ",  "139.073090")
  tav <-paste("tav = ", tav , "(oC) ! annual average ambient temperature")
  amp <- paste("amp = ", amp, "(oC) ! annual amplitude in mean monthly temperature")
  
  
  
  header<-rbind("!title = Callington, South Australia", NA,
                "[weather.met.weather]",
                Latitude,
                Longitude,
                tav, 
                amp)
  rownames(header) <- NULL
  colnames(header) <-"X1"
  header<-data.frame(header)
  
  
  
  
  header[nrow(header)+1,]<-NA
  
  coldfsub<-data.frame(t(colnames(dfsub)))
  out<-rbindlist(list(coldfsub,dfsub))
  
  
  out<-rbind.fill(header, out) 
  
  
  
  write.table(out, file = "Callington.met", na="",
              row.names = FALSE,
              col.names = FALSE,
              append = TRUE, quote=FALSE)
  
  
}