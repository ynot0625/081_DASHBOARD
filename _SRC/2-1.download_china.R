library(tidyverse)
library(leaflet)
library(reshape2)
library(htmlwidgets)
library(RCurl)

rm(list=ls())

edate <- Sys.time()
edate

base_url <- "http://quotsoft.net/air/data/china_sites_"
df <- data.frame()

for (idate in  as.Date(edate)) {
  issued_date <- as.Date(idate, origin = "1970-01-01")
  url_F <- paste0(base_url,format(issued_date, "%Y%m%d"),".csv")
  url_F
  fn <- paste0("DAIO/CHINA/china_",format(issued_date,"%Y%m%d"),".csv")
  download.file(url_F, fn, method="libcurl")
  dummy<-read.csv(fn,header = T,sep=",")
  df <- plyr:: rbind.fill(df, dummy)
}