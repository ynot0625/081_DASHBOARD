library(tidyverse)
library(reshape2)

rm(list=ls())

dates <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by=1)

df <- data.frame()
for (idate in  dates ) {
  issued_date <- as.Date(idate, origin = "1970-01-01")
  print(issued_date)
  fn <- paste0("DAIO/CHINA/china_",format(issued_date,"%Y%m%d"),".csv")
  if (file.exists(fn)) {
    dummy <- read.csv(fn, header = T, sep=",")
    df <- plyr:: rbind.fill(df, dummy)
  }
}

china_PM2.5 <- df %>%
  filter(type == "PM2.5")

melt_china_PM2.5 <- melt(china_PM2.5, id = c("date","hour","type"))
names(melt_china_PM2.5) <- c("date","hour","TYPE","STNID","PM2.5")
melt_china_PM2.5$STNID <- substr(melt_china_PM2.5$STNID, 2,6)

station_info <- read.csv("DABA/monitoring site_2021.01.01.csv",sep=",")
station_info$longitude <- as.double(as.character(station_info$longitude))
station_info$latitude <- as.double(as.character(station_info$latitude))

names(station_info)[1:2] <- c("STNID","NAME")


new_china_PM2.5 <- merge(melt_china_PM2.5, station_info, by ="STNID")

avg_china_PM2.5 <- new_china_PM2.5%>%
  group_by(region) %>%
  mutate(avg_PM2.5 = mean(PM2.5, na.rm=TRUE))

china_F <- data.frame(distinct(avg_china_PM2.5, avg_PM2.5 ))
china_F[china_F$region == "Tibet",]$region <- "Xizang"


write.csv(china_F, file="DAIO/china_2021.csv",row.names=FALSE)

avg_china_PM2.5_point <- new_china_PM2.5 %>%
  group_by(STNID) %>%
  mutate(avg_PM2.5 = mean(PM2.5, na.rm=TRUE))

avg_china_PM2.5_point <- distinct(avg_china_PM2.5_point, STNID, TYPE, longitude, latitude, avg_PM2.5)

write.csv(avg_china_PM2.5_point, file="DAIO/china_2021_point.csv",row.names=FALSE)

