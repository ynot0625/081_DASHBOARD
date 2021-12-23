rm(list=ls())
setwd("C:/RWORK/081_DASHBOARD/")


################### REALTIME ################################
# 기상청 이미지 수집
## 위성(RGB 천연(AI), 황사)
source("_SRC/1-1.download_sat_image.R")
source("_SRC/1-2.download_sfc_image.R")


### 동북아시아 PM2.5 관측(우리나라 + 중국)
source("_SRC/2-1.download_china.R")
source("_SRC/2-2.download_korea.R")
source("_SRC/2-3.plot_korea_china.R")

################### STATIC ################################
source("_SRC/3-1.prepare_china_year.R")
#source("_SRC/3-2.prepare_korea_year.R")
source("_SRC/3-3.plot_korea_china_year.R")
source("_SRC/3-4.plot_china_year_point.R")

source("_SRC/4-1.chart.R")
source("_SRC/4-2.plot_pm2.5.R")

source("_SRC/5-1.run_hysplit.R")
##################
