library(rvest)
library(XML)
library(data.table)
#install.packages("XML")

aqi_info='DABA/station_202107.csv'
station_info<-read.table(aqi_info,header=TRUE,sep=",")

for (ix in 1:length(station_info$Sigungu)) {
  print(station_info$Sigungu[ix])
  base_url <- "http://apis.data.go.kr/B552584/ArpltnInforInqireSvc/getMsrstnAcctoRltmMesureDnsty?"
  station_url<-paste0("stationName=",URLencode(iconv(station_info$Sigungu[[ix]], from="cp949", to="UTF-8")))
  dataTerm_url<-paste0("dataTerm=","DAILY")
  key_url<-paste0("ServiceKey=","")
  return_url <- paste0("returnType=","xml")
  page_url<-paste0("pageNo=",1)
  num_url<-paste0("numOfRows=",25)
  ver_url<-paste0("ver=","1.0")
  
  post_url<-paste(station_url,key_url, return_url, num_url, page_url, station_url, dataTerm_url, ver_url,sep = "&")
  post_url<-paste0(base_url,post_url)
  
  xmldat <- xmlTreeParse(post_url,useInternalNodes = T, encoding = "UTF-8")
  rootNode <- xmlRoot(xmldat)
  
  items = rootNode[[2]][["items"]]
  size=xmlSize(items)
  data_subtotal <- list()  

  test2 <- data.frame()
  test3 <- data.frame()
  test4 <- data.frame()
  
  if(is.null(items[[1]])){
    print('did not work out')}
  else{
    
    test <- xmlSApply(items[[1]],xmlValue)  
    test[[1]]
    
    for(i in 1:size)
    {
      test <- xmlSApply(items[[i]],xmlValue)  
      
      ttime=as.POSIXct(test[[17]], "%Y-%m-%d %H", tz="GMT")
      ttime
      yyyymmddhh <- format(ttime, "%Y%m%d%H")
      
      test2 <- data.table(Sigungu=station_info$Sigungu[[ix]],
                          S_code=station_info$code[[ix]],
                          date=test[[17]],
                          lon=station_info$lon[[ix]],
                          lat=station_info$lat[[ix]],
                          so2=test[[4]],
                          co=test[[5]],
                          o3=test[[20]],
                          no2=test[[19]],
                          pm10=test[[8]],
                          pm25=test[[11]]
      )
      
      outfile<-'DAIO/KOREA/'
      outfile<-paste0(outfile,yyyymmddhh)
      outfile<-paste(outfile,'.txt',sep="")
      
      write.table(test2,outfile,col.names=F,row.names=F,append = T, quote=F,sep=",")    
      
      
    }
  }
  
  test3
  
  
}
