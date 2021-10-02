#' Callington (SA) met producer
#' 
#' Designed for Callington dataset (up to date) from Wildeye API
#' 
#' 
#' @param UserName as "QLDUni" by default
#' @param PassWord as "Q1dUn!" by default
#' 
#' @return output
#' 
#' @export
#' 
#' @author patrickaxe





callington_met<-function(UserName,PassWord){
  
  require(tidyverse)
  require(xml2)
  require(rccdates)
  require(gsubfn)
  require(XML)
  require(plyr)
  require(data.table)
  require(lubridate)
  
  # startdate <- format(Sys.time(),"%Y-%m-%d %T")
  
  mth<-format(Sys.time(),"%m")
  
  url_list <- as.character()
  for (month in 5:mth ){
    link<-paste0("https://www.outpostcentral.com/api/2.0/dataservice/mydata.aspx","?userName=",UserName, "&password=",PassWord,"&dateFrom=",paste0(paste("2021-"),month,paste("-01 00:00:00")))
    url_list <- c(url_list,link)
     print(url_list)
  } 
  temp2 <- NA
  rain2 <- NA
  radn2 <- NA
  for (link in url_list) {
    xml_df<-url(link,"rb")
    xml_df<-read_xml(xml_df)
    print(xml_df)
    namelist<-xml_text(xml_find_all(xml_df, "//opdata:input/name"))
    grandchildren<-xml_children(xml_children(xml_children(xml_children(xml_df)[[1]])[[1]])[8])
    names(grandchildren) <- namelist
    
    temp<-grandchildren[[ namelist[!grepl("#", namelist) & !grepl("Pressure", namelist) & !grepl("Humidity", namelist)& !grepl("Signal", namelist) & !grepl("Delta T", namelist) & !grepl("Counter 2", namelist) & !grepl("Rainfall", namelist) & !grepl("Battery", namelist) & !grepl("ADC1", namelist) & !grepl("Decagon VP4", namelist)]
    ]]
    
    temp<-xml_children(temp)[[6]]
    temp <- xmlParse(temp) 
    # rootnode<-xmlRoot(test) 
    # rootnode[1]
    temp<-xmlToDataFrame(temp)
    temp2 <- rbind(temp2,temp)
    
    rain<-grandchildren[["Rainfall"]]
    rain<-xml_children(rain)[[6]]
    rain <- xmlParse(rain) 
    rain<-xmlToDataFrame(rain)
    rain2 <- rbind(rain2,rain)
    
    radn<-grandchildren[["#0 Solar Radiation W/m2"]]
    radn<-xml_children(radn)[[6]]
    radn <- xmlParse(radn) 
    radn<-xmlToDataFrame(radn)
    radn2 <- rbind(radn2,radn)
  }
  
  colnames(temp2) <- c("Temp","Date")
  temp2$Date <- as.POSIXct(temp2$Date, format = "%Y-%m-%dT%H:%M:%S")
  temp2$Date <- round(temp2$Date,units="mins")
  temp2$Temp <- as.numeric(temp2$Temp)
  colnames(rain2) <- c("Rain","Date")
  rain2$Date <- as.POSIXct(rain2$Date, format = "%Y-%m-%dT%H:%M:%S")
  rain2$Date <- round(rain2$Date,units="mins")
  rain2$Rain <- as.numeric(rain2$Rain)
  colnames(radn2) <- c("Radn","Date")
  radn2$Date <- as.POSIXct(radn2$Date, format = "%Y-%m-%dT%H:%M:%S")
  radn2$Date <- round(radn2$Date,units="mins")
  radn2$Radn <- as.numeric(radn2$Radn)
  
  fullvals <- temp2 %>% full_join(rain2,by="Date") %>% 
    full_join(radn2,by="Date") %>% 
    na.omit() %>%
    mutate(day_til_now = floor(as.double(difftime(now(),Date,units = "day")))) %>%
    mutate(day = floor(as.double(difftime(Date,"2021-01-01 00:00:00",units = "day")))) 
  
  maxt<-as.data.frame(aggregate(Temp~day,data=fullvals,FUN=max)) %>% 
    dplyr::rename("maxt"="Temp")
  mint<-as.data.frame(aggregate(Temp~day,data=fullvals,FUN=min)) %>%
    dplyr::rename("mint"="Temp")
  rain<-as.data.frame(aggregate(Rain~day,data=fullvals,FUN=sum)) %>%
    dplyr::rename("rain"="Rain")
  radi<-as.data.frame(aggregate(Radn~day,data=fullvals,FUN=sum)) %>% 
    mutate(radn = Radn*15*60/1000000)
  
 

  dfsub<-cbind(year="2021", maxt,radi,mint,rain) %>%
    subset(, select=c(year, day, radn, maxt, mint,rain ))
  

  
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
