library(rgdal)
library(htmlwidgets)
library(raster)
library(leaflet)

#CHINA
avg_china_PM2.5 <- read.csv("DAIO/china_2021.csv",sep=",")
avg_china_PM2.5$region <- as.character(avg_china_PM2.5$region)

cnty <- readOGR("DABA/gadm_CHN_shp/gadm36_CHN_1.shp")
cnty$NAME_1 <- as.character(cnty$NAME_1)

cnty_pm2.5 <- merge(cnty, avg_china_PM2.5, by.x="NAME_1", by.y="region", all.x=TRUE)

#KOREA
avg_kor_PM2.5 <- read.csv("DAIO/korea_2021.csv",sep=",")
head(avg_kor_PM2.5)
names(avg_kor_PM2.5) <- c("CTP_KOR_NM","avg_PM2.5")

kor_cnty <- readOGR("DABA/KOR_shp/TL_SCCO_CTPRVN.shp")
to_crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
kor_cnty_lonlat <- spTransform(kor_cnty, to_crs)
#kor_cnty_lonlat$avg_PM2.5 <- "NA"
#[1] 서울특별시     부산광역시     대구광역시     인천광역시     광주광역시     대전광역시     울산광역시     세종특별자치시
#[9] 경기도         강원도         충청북도       충청남도       전라북도       전라남도       경상북도       경상남도      
#[17] 제주특별자치도
kor_cnty_lonlat_pm2.5 <- merge(kor_cnty_lonlat, avg_kor_PM2.5, by="CTP_KOR_NM", all=TRUE)


#MERGE
east_cnty <- bind(cnty_pm2.5, kor_cnty_lonlat_pm2.5)
east_cnty$avg_PM2.5 <- as.numeric(east_cnty$avg_PM2.5)

pal2 <- colorBin("RdYlBu", bins = c(seq(0,50,5),100), na.color = "transparent", reverse = T)
#http://leaflet-extras.github.io/leaflet-providers/preview/index.html

image4 <- leaflet(east_cnty) %>%
  setView(lng=120.9784, lat=37.566, zoom=4) %>%
  addProviderTiles('Stamen.TonerBackground') %>% 
  addPolygons(color='#444', weight=1, fillColor=~pal2(avg_PM2.5), fillOpacity = 0.5) %>%
  addLegend(position = 'bottomright', title = 'PM2.5(ug/m3)', 
            pal = pal2, values = ~avg_PM2.5, opacity = 1, na.label="") 
image4

saveWidget(image4, file="contents_of_frame5.html")