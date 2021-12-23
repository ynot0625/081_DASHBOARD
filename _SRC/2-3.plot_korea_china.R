library(tidyverse)
library(leaflet)
library(reshape2)
library(htmlwidgets)

rm(list=ls())
#httr::set_config(config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))
edate <- Sys.time()-3600

#CHINA
df <- data.frame()
for (idate in  as.Date(edate)) {
  issued_date <- as.Date(idate, origin = "1970-01-01")
  fn <- paste0("DAIO/CHINA/china_",format(issued_date,"%Y%m%d"),".csv")
  dummy<-read.csv(fn,header = T,sep=",")
  df <- plyr:: rbind.fill(df, dummy)
}

china_PM2.5 <- df %>%
  filter(type == "PM2.5" & hour == max(unique(df$hour)))

china_PM2.5$date1 <- with(china_PM2.5, as.POSIXct(paste(date, hour), format="%Y%m%d %H", tz="GMT")+3600)
yyyymmddhh <- format(china_PM2.5$date1, "%Y%m%d%H")

melt_china_PM2.5 <- melt(china_PM2.5, id = c("date","hour","date1","type"))
head(melt_china_PM2.5)
tail(melt_china_PM2.5)

names(melt_china_PM2.5) <- c("date","hour","date1","TYPE","STNID","PM2.5")
melt_china_PM2.5$STNID <- substr(melt_china_PM2.5$STNID, 2,6)

station_info <- read.csv("DABA/monitoring site_2021.01.01.csv",sep=",")
station_info$longitude <- as.double(as.character(station_info$longitude))
station_info$latitude <- as.double(as.character(station_info$latitude))

names(station_info)[1:2] <- c("STNID","NAME")
new_china_PM2.5 <- merge(melt_china_PM2.5, station_info, by ="STNID")

dat1 <- data.frame(new_china_PM2.5$longitude, new_china_PM2.5$latitude, new_china_PM2.5$PM2.5)
names(dat1) <- c("longitude","latitude","PM2.5")


#KOREA
korea<-read.csv(paste0("DAIO/KOREA/",yyyymmddhh,".txt"),sep=",", header = F)
korea <- unique(korea)
dat2 <- data.frame(korea$V4, korea$V5, korea$V11)
names(dat2) <- c("longitude","latitude","PM2.5")

dat <- rbind(dat1, dat2)
dat$PM2.5 <- as.numeric(dat$PM2.5)

pal2 <- colorBin("RdYlBu", domain = dat$PM2.5, bins = c(seq(0,75,5),1000),
                 na.color = "transparent", reverse = T)

image3 <- dat %>%
  filter(PM2.5 > 0 ) %>%
  leaflet() %>%
  setView(lng=120.9784, lat=37.566, zoom=4) %>%
  #addProviderTiles('CartoDB.Positron') %>%
  addProviderTiles('Stamen.TonerBackground') %>% 
  addCircles(lng=~longitude, lat=~latitude, color=~pal2(PM2.5)) %>%
  addLegend(position = 'bottomright', title = 'PM2.5(ug/m3)', 
            pal = pal2, values = ~PM2.5, opacity = 1, na.label="")


saveWidget(image3, file="contents_of_frame4.html")

