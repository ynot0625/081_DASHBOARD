library(ggplot2)
#windowsFonts(Times=windowsFont("TT Times New Roman"))

#install.packages("ggplot2")
rm(list=ls())
fn <- "DAIN/PM2.5_20010101_항목별_일평균_기간별 자료.csv"

df_pm2.5 <- read.table(fn, header=TRUE, sep=",")[,c("년","월","일","평균")]
head(df_pm2.5)
names(df_pm2.5) <- c("Year","Month","Day","PM2.5")
head(df_pm2.5)
df_pm2.5$date <- with(df_pm2.5, as.POSIXct(paste(Year,Month,Day),format="%Y%m%d"),tz="GMT")
df_pm2.5$Jul <- as.integer(format(df_pm2.5$date, "%j"))
head(df_pm2.5)

cols <- c("#e7f0fa","#c9e2f6","#95cbee","#0099dc","#4ab04a","#ffd73e",
          "#eec73a","#e29421","#f05336","#ce472e")

#install.packages("extrafont")
extrafont::loadfonts()

df <- df_pm2.5 
head(df)
tail(df)

gg <- ggplot()
gg <- gg + theme(text = element_text(size=35))
gg <- ggplot(subset(df, Year >= 2001 & Year <= 2021) , aes(x=Jul, y=Year)) +
  geom_tile(aes(fill=PM2.5), colours="transparent",
            width=1, height=0.9) + theme_minimal() +
  scale_fill_gradientn(colours = cols, limits=c(1,75),
                       breaks =c(0,15,35,50,75,Inf),
                       na.value = "red",
                       guide=guide_colorbar(ticks=T, nbin=50,
                                            barheight=.5, label = T,
                                            barwidith=10))
gg

gg <- gg +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(2001,2004,2007,2010,2013,2016,2019,2021),
                     labels = c("2001","2004","2007","2010","2013","2016","2019","2021")) +
  #  scale_y_continuous(expand = c(0,0),
  #                     breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019),
  #                     labels = c("2010","2011","2012","2013","2014","2015","2016",
  #                                "2017","2018","2019")) +
  
  scale_x_continuous(expand = c(0,0),
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = c("JAN","FEB","MAR","APR","MAY","JUN",
                                "JUL","AUG","SEP","OCT","NOV","DEC")) +
  geom_segment(x=0,xend=366,y=2020.5,yend=2020.5, size=.9, colors="grey") +
  labs(x="",y="", fill="")

gg

gg <- gg + 
  theme(legend.position = c(0.5, -0.13),
        legend.direction = "horizontal",
        legend.text = element_text(colour="black"),
        plot.margin = grid::unit(c(.5,.5,1.5,.5),"cm"),
        axis.text.y=element_text(size=8,
                                 hjust=1.5),
        axis.text.x=element_text(size=8),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank(),
        title=element_text(hjust=-0.7, vjust=1))
#text=element_text(family="TT Times New Roman"))

gg

png(filename="DAGR/image7.png",
    width=1920, height = 1080, res = 200)

gg

dev.off()