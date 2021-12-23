library(lubridate)
#library(dplyr)
#library(tidyr)
library(tidyverse)
# cleaning up environment
rm(list=ls())
#setwd("D:/RWORK/2019_study/")


eom <- function(date) {
  # date character string containing POSIXct date
  date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
  mon <- date.lt$mon + 2 
  year <- date.lt$year
  year <- year + as.integer(mon==13) # if month was December add a year
  mon[mon==13] <- 1
  iso = ISOdate(1900+year, mon, 1, hour=0, tz=attr(date,"tz"))
  result = as.POSIXct(iso) - 86400 # subtract one day
  result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
} #last day in month

fn <- "DAIN/PM2.5_20010101_항목별_일평균_기간별 자료.csv"

df_pm2.5<-read.table(fn,header=TRUE,sep=",")
df_pm2.5<-df_pm2.5[-c(5:33)]
df_pm2.5 <-df_pm2.5[-c(4)]
head(df_pm2.5)
df_pm2.5 <- df_pm2.5[c("년","월","일","평균")]
names(df_pm2.5) <-c("Year","Month","Day","PM2.5")
df_pm2.5$date <- with(df_pm2.5, as.POSIXct(paste(Year,Month,Day),format="%Y%m%d",tz="GMT"))
head(df_pm2.5)

data <- df_pm2.5 %>%
  #filter(PM2.5 > 0 & Hour == "06" ) %>%
  select(year=Year, Month, PM2.5,date) %>%
  filter(PM2.5 > 0 ) %>%
  mutate(date2=format(date,"%Y-%m-%d"),
         decade = (as.integer(format(date,"%Y%m"))),
         start = as.Date(format(date,"%Y-%m-01")),
         end = as.Date(eom(as.POSIXct(format(date,"%Y-%m-01"),"GMT"))), 
         start2 = as.Date(format(date,"%Y-01-01")), 
         end2 = as.Date(format(date,"%Y-12-31"))) %>%
  group_by(decade) %>%
  mutate(decade_mean=mean(PM2.5)) %>%
  group_by(year) %>%
  mutate(annum_mean=mean(PM2.5)) %>%
  group_by(year,Month) %>%
  mutate(month_mean=mean(PM2.5)) %>%
  ungroup -> data

head(data,20)
tail(data,10)
data$date2 <- as.Date(data$date2)
data$start <- as.Date(data$start)
data$end <- as.Date(data$end)


head(data)

trans <- function(x){pmin(x,100) + 0.25*pmax(x-100,0)}

data$tPM2.5 <- data$PM2.5
data$tdecade_mean <- data$decade_mean
data$tannum_mean <- data$annum_mean

data$PM2.5 <- trans(data$PM2.5)
data$decade_mean <- trans(data$decade_mean)
data$annum_mean <- trans(data$annum_mean)



data2 <- data


head(data2)
tail(data2)

library(ggplot2)

i=2021
data <- subset(data2, year <= 2021 & year >= 2001)
head(data)     
tail(data)


data$PM2.5[data$year > i] <- NA
data$decade_mean[data$year > i] <- NA
data$annum_mean[data$year > i] <- NA
data$month_mean[data$year > i] <- NA


# start plot --------------------------------------------------------------
gg <- ggplot()
gg <- gg + theme_bw()
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(legend.position=c(0.85, 0.9))
gg <- gg + theme(legend.key=element_blank())
gg <- gg + theme(legend.background=element_blank())
gg <- gg + theme(text = element_text(size=17))

# daily data ------------------------------------------------------------
gg <- gg + geom_line(data=data2, aes(date2, y=PM2.5, color = "daily mean"),
                     size=0.35, alpha=0.5)
#gg <- gg + geom_point(data=data, aes(date2, y=PM2.5, color = "daily mean"),
#                      size=0.35, alpha=0.25)

# decade vertical markers -------------------------------------------------
gg <- gg + geom_vline(data=data %>% select(year),
                      aes(xintercept=as.Date(sprintf("%s-12-31", year))),
                      size=1., color="#4575b4", linetype="dotted", alpha=0.5)

# annual mean ------------------------------------------------------------
gg <- gg + geom_segment(data=data %>% distinct(annum_mean,start2, end2),
                        aes(x=start2,xend=end2,y=annum_mean,yend=annum_mean,
                            color = "annual mean"),  size=1.0)


# monthly data -------------------------------------------------------------
gg <- gg + geom_line(data=data %>% distinct(year, Month, decade,decade_mean),
                     aes(x=as.Date(sprintf("%s-%s-15",year,Month)),
                         y=decade_mean, color = "monthly mean"),
                     size=0.5 )

gg <- gg + geom_point(data=data %>% distinct(year, Month, decade,decade_mean),
                      aes(x=as.Date(sprintf("%s-%s-15",year,Month)),
                          y=decade_mean, color = "monthly mean"),
                      size=1.5)

# additional annotations --------------------------------------------------
# max monthly mean horizontal marker/text
gg <- gg + geom_hline(yintercept=max(data$decade_mean),  alpha=0.9,
                      color="#d73027", linetype="dashed", size=1)

gg <- gg + annotate("text",
                    x=as.Date(sprintf("%4.0f-12-31", mean(range(as.integer(data$year))))),
                    y=max(data$decade_mean),
                    color="#d73027", alpha=0.9,
                    hjust=0.25, vjust=-1, size=5,
                    label=sprintf("Max monthly mean PM2.5 %2.1f ug/m3", max(data$tdecade_mean)))

# max daily mean horizontal marker/text
gg <- gg + geom_hline(yintercept=max(data$PM2.5),  alpha=0.9,
                      color="#7f7f7f", linetype="dashed", size=1)

gg <- gg + annotate("text",
                    x=as.Date(sprintf("%4.0f-12-31", mean(range(as.integer(data$year))))),
                    y=max(data$PM2.5)-20,
                    color="#7f7f7f",  alpha=0.9,
                    hjust=0.25, vjust=-1, size=5,
                    label=sprintf("Max daily mean PM2.5 %2.1f ug/m3", max(data$tPM2.5)))


# set colors --------------------------------------------------------------

gg <- gg + scale_color_manual(name="", values=c("#4575b4","#7f7f7f","#d73027"))

# set x axis limits -------------------------------------------------------
gg <- gg + scale_x_date(expand=c(0, 0),
                        limits=c(as.Date(sprintf("%4.0f-01-01", range(as.integer(data$year))[1])),
                                 as.Date(sprintf("%4.0f-12-31", range(as.integer(data$year))[2]))),
                        date_breaks = "2 year",
                        date_labels = "%Y")

yticks <- c(0,25,50,75,100,200,300,400)
#yticks <- c(0,25,50,75,100,125, 150)

gg <- gg + #geom_rect(aes(xmin=as.Date(sprintf("%4.0f-01-01", range(as.integer(data$year))[1])),
  #               xmax=as.Date(sprintf("%4.0f-12-31", range(as.integer(data$year))[2])),
  #               ymin=85, ymax=90), fill="white") +
  scale_y_continuous(limits=c(0,NA),
                     breaks=  trans(yticks),
                     #breaks = yticks,
                     labels = yticks)
trans(yticks)

#gg <- gg + xlab("")
# add labels --------------------------------------------------------------

gg <- gg + labs(x=NULL, y="PM2.5 concentration(ug/m3)",
                title=sprintf("PM-2.5 Long-Term Trend(%d to %d)\n",
                              range(as.integer(data$year))[1], range(as.integer(data$year))[2]))
head(data)
#library(gganimate)
#library(transformr)

#gg +  transition_reveal(date2)
gg
png(filename=paste0("DAGR/image8_",i,".png",sep=""),
    width = 1920, height = 1080, res=200)
gg 
dev.off()

library(magick)
library(purrr)

list.files(path = "./dagr/fig1/", pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=0.5, loop = 0) %>% # animates, can opt for number of loops
  image_write("./dagr/6.gif") # write to current dir