


avg_china_PM2.5_point <- read.csv("DAIO/china_2021_point.csv",sep=",")


pal2 <- colorBin("RdYlBu", domain = avg_china_PM2.5_point$avg_PM2.5, bins = c(seq(0,50,5),100),
                 na.color = "transparent", reverse = T)

image6 <- avg_china_PM2.5_point  %>%
  filter(avg_PM2.5 > 0 ) %>%
  leaflet() %>%
  setView(lng=120.9784, lat=37.566, zoom=4) %>%
  #addProviderTiles('CartoDB.Positron') %>%
  addProviderTiles('Stamen.TonerBackground') %>% 
  addCircles(lng=~longitude, lat=~latitude, color=~pal2(avg_PM2.5)) %>%
  addLegend(position = 'bottomright', title = 'PM2.5(ug/m3)', 
            pal = pal2, values = ~avg_PM2.5, opacity = 1, na.label="")


saveWidget(image4, file="contents_of_frame6.html")