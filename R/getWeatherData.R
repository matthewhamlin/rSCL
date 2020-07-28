# library(tidyverse)
# library(data.table)
# library(lubridate)
# library(dplyr)
# library(timeDate)
# library(RCurl)
# library(zoo)

#' @export
getWeatherData <- function(date1,date2, station_id = "727930-24233",Interp = FALSE){

  years <- seq(lubridate::year(as.Date(date1)),lubridate::year(as.Date(date2)))
  dFile <- "Weather.gz"
  weatherData <- data.frame()

  for(year in years){
    url <- paste0("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/",
                  as.character(year), '/', station_id, '-', as.character(year), '.gz')

    download.file(url = url,destfile = dFile)
    gz <- gzfile(dFile, open = "rt")
    x <- readLines(gz)
    #close(dFile)

    tempTable <- read.table(text=x,col.names=c('Year','Month','Day','Hour','Temperature',
                                               'Dew_Point_Temperature','Sea_Level_Pressure',
                                               'Wind_Direction','Wind_Speed','Cloud_Coverage',
                                               'Precipitation_Depth_1Hr','Precipitation_Depth_6Hr'))

    tempTable <- tempTable %>%
      dplyr::mutate(datetime = make_datetime(year = tempTable$Year,
                                      month = tempTable$Month,
                                      day = tempTable$Day,
                                      hour = tempTable$Hour))
    tempTable <- data.table::setDT(tempTable)
    tempTable[,c(5:12):=lapply(.SD,function(x){ifelse(x==-9999,NA,x)}), .SDcols=5:12]
    tempTable[,c(5:12):=lapply(.SD,function(x){x/10}), .SDcols=5:12]
    tempTable[,c(5:6):=lapply(.SD,function(x){x*(9/5)+32}), .SDcols=5:6]

    weatherData <- rbind(weatherData,tempTable)
  }
  weatherData <- weatherData %>%
    dplyr::filter(datetime >= as.Date(date1) & datetime < (as.Date(date2)+1))

  if(Interp){
    cols<-c('Temperature','Dew_Point_Temperature','Sea_Level_Pressure','Wind_Speed')
    for(col in cols){
      weatherData[col] <-zoo::na.approx(weatherData[col])
    }
  }

  weatherData$datetime <- lubridate::with_tz(weatherData$datetime, tz="US/Pacific")
  return(weatherData)
}
#getWeatherData('2020-01-01','2020-01-02')
