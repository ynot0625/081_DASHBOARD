library(magick)
library(purrr)
#library(tidyverse)

Sys.setenv(tz="GMT")
rm(list=ls())
Sys.time()
edate <- format(Sys.time(), "%Y-%m-%d %H:00:00")
sdate <- format(Sys.time() - 12*3600, "%Y-%m-%d %H:00:00")
#12시간 간격 설정

dates <- seq(as.POSIXct(sdate), as.POSIXct(edate), by = 1200)
#10분 간격 설정

vTime <- strptime(dates, "%Y-%m-%d %H:%M:%S")
YYYY <- format(vTime, "%Y")
MM <- format(vTime, "%m")
DD <- format(vTime, "%d")
HH <- format(vTime, "%H")
Mi <- format(vTime, "%M")
YYYYMMDD <- format(as.POSIXct(edate), "%Y%m%d")

#edate <- format(Sys.time(), "%Y%m%d%H", tz="GMT")
#sdate <- format(Sys.time()-24*3600, "%Y%m%d%H", tz="GMT")
#https://nmsc.kma.go.kr/IMG/GK2A/AMI/PRIMARY/L1B/COMPLETE/KO/202112/14/06/gk2a_ami_le1b_rgb-s-true_ko020lc_202112140658.png

base_url <- "https://nmsc.kma.go.kr/IMG/GK2A/AMI/PRIMARY/L1B/COMPLETE/KO/"
base_fn <- c("gk2a_ami_le1b_rgb-s-true_ko020lc_", "gk2a_ami_le1b_rgb-dust_ko020lc_")
TargetFile_1 <- paste0(base_fn[1],YYYY,MM,DD,HH,Mi,".png")
TargetURL_1 <- paste0(base_url,YYYY,MM,"/",DD,"/",HH,"/",TargetFile_1)

TargetFile_2 <- paste0(base_fn[2],YYYY,MM,DD,HH,Mi,".srv.png")
TargetURL_2 <- paste0(base_url,YYYY,MM,"/",DD,"/",HH,"/",TargetFile_2)

#https://nmsc.kma.go.kr/IMG/GK2A/AMI/PRIMARY/L1B/COMPLETE/KO/202112/14/06/gk2a_ami_le1b_rgb-s-true_ko020lc_202112140600.png
#SaveDir <- paste0("./DAGR/",unique(format(as.POSIXct(edate, origin = "1970-01-01"), "%Y%m%d", tz="GMT")))

# 이미지 다운로드 폴더 설정
SaveDir <- paste0("./DAGR/",unique(YYYYMMDD))
dir.create(SaveDir)

# 파일유무 확인
# 위성자료의 경우 자료가 비는 경우가 존재
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

for (ix in 1:length(TargetURL_2)) {
  size_check <- download_size(url=TargetURL_1[ix])
  print(size_check)
  if(size_check > 10000) {
    tryCatch(
      download.file(url = TargetURL_2[ix],destfile=paste0(SaveDir,"/",TargetFile_2[ix]), mod="wb",quiet=F),
      error = function(e) print(paste(TargetURL_2[ix], 'did not work out'))
    )}
}

#1번 이미지 생성
list.files(path=paste0(SaveDir,"/"),pattern = substr(TargetFile_1,1,35),full.names=T) %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps=2, loop=0) %>%
  image_write(paste0("DAGR/image1.gif"))

#2번 이미지 생성
list.files(path=paste0(SaveDir,"/"),pattern = substr(TargetFile_2,1,35),full.names=T) %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps=2, loop=0) %>%
  image_write(paste0("DAGR/image2.gif"))

