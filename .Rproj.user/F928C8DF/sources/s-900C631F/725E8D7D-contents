library(openair)
library(tidyverse)
library(dplyr)
library(magick)
library(purrr)

# cleaning up environment
rm(list=ls())
setwd("C:/RWORK/081_DASHBOARD/")
#source("_SRC/openair_hysplit.R")
source("_SRC/run_hysplit.R")



#library(devtools)
#source_gist("https://gist.github.com/davidcarslaw/c67e33a04ff6e1be0cd7357796e4bdf5",
#            filename = "run_hysplit.R")


data_out <- run_hysplit(
  latitude = 37.5,
  longitude = 127,
  runtime = -96,
  start_height = 500,
  model_height = 10000,
  start = 2021,
  end = "2021-11-30",
  hysplit_exec = "C:/hysplit4/exec",
  hysplit_input = "D:/RWORK/DATA/NCEP/",
  hysplit_output = "C:/RWORK/081_DASHBOARD/DAIO/hysplit_out",
  site = "seoul")

head(data_out)

traj <- subset(data_out, select = -c(site, start_height))
out = "DAIO/hysplit_out/"
name = "seoul"
Year = "202101_202111"
file.name <- paste(out, name, Year, ".RData", sep = "")
save(traj, file = file.name)


png(filename=paste0("DAGR/image6_2.png",sep=""),
    width = 1920, height = 1080, res=200)
cluster<-trajCluster(traj,
                     #selectByDate(traj, start = "1/1/2017", end = "30/9/2021"),
                     method = "Euclid", n.cluster= 5, col = "Set1",
                     #col = c("grey35","grey35","grey35","grey35","grey35","grey35"),
                     projection="lambert",orientation=c(0,90,0),
                     #xlim=c(110,150),ylim=c(30,50),parameters=c(127.5,37.5),
                     xlim=c(60,150),ylim=c(22,65),parameters=c(127.5,37.5),# 20211019
                     #xlim=c(65,145),ylim=c(20,70),parameters=c(127.5,37.5),
                     map.alpha=0.4, map.fill=FALSE,grid.col="transparent",map.cols="gray40",
                     border = "NA") #,key.position = "top",key.columns = 5)

dev.off()

image_read("DAGR/image6_2.png") %>%
  image_trim() %>%
  image_write(paste0("DAGR/image6.png"))



png(filename=paste0("DAGR/image5_2.png",sep=""),
    width = 1920, height = 1080, res=200)

trajPlot(traj,#col="black", 
         projection="lambert",orientation=c(0,90,0),
         xlim=c(90,145),ylim=c(20,55),parameters=c(127.5,37.5),
         map.alpha=0.4,map.fill=FALSE,grid.col="transparent",map.cols="gray40",
         method = "hexbin",
         col = "jet", xbin = 100, npoints = 12) 

dev.off()

image_read("DAGR/image5_2.png") %>%
  image_trim() %>%
  image_write(paste0("DAGR/image5.png"))

