library(magick)
library(purrr)
#library(tidyverse)

Sys.setenv(tz="GMT")
rm(list=ls())
Sys.time()
date <- format(Sys.time()- 86400, "%Y-%m-%d 00:00:00") 
#sdate <- format(Sys.time() - 12*3600, "%Y-%m-%d %H:00:00")

#dates <- seq(as.POSIXct(sdate), as.POSIXct(edate), by = 1200)

vTime <- strptime(date, "%Y-%m-%d %H:%M:%S")
YYYY <- format(vTime, "%Y")
MM <- format(vTime, "%m")
DD <- format(vTime, "%d")
HH <- format(vTime, "%H")
Mi <- format(vTime, "%M")
YYYYMMDD <- format(as.POSIXct(date), "%Y%m%d")
YYYYMMDDHH <- format(as.POSIXct(date), "%Y%m%d%H")
YYYYMMDDHH



base_url <- "https://www.weather.go.kr/w/repositary/image/cht/img/"
base_fn <- c("kim_gdps_erly_asia_surfce_ft06_pa4_s")
TargetFile_1 <- paste0(base_fn,sprintf('%0.3d', seq(0,72,6)),"_",YYYYMMDDHH,".png")
TargetURL_1 <- paste0(base_url,TargetFile_1)

SaveDir <- paste0("./DAGR/",unique(YYYYMMDD))
dir.create(SaveDir)
#for (ix in 1:length(SaveDir)) { dir.create(SaveDir[ix])}

download_size <- function(url) as.numeric(httr::HEAD(url)$headers$`content-length`)

for (ix in 1:length(TargetURL_1)) {
  size_check <- download_size(url=TargetURL_1[ix])
  print(size_check)
  if(size_check > 10000) {
    tryCatch(
      download.file(url = TargetURL_1[ix],destfile=paste0(SaveDir,"/",TargetFile_1[ix]), mod="wb",quiet=F),
      error = function(e) print(paste(TargetURL_1[ix], 'did not work out'))
    )}
}
TargetURL_1[1]

list.files(path=paste0(SaveDir,"/"),pattern = substr(TargetFile_1,1,35),full.names=T) %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps=1, loop=0) %>%
  image_trim() %>%
  image_write(paste0("DAGR/image3.gif"))

library(htmlwidgets)
#saveWidget(map(a), file="image3.html")
