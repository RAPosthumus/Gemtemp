library(tidyverse)
library(readr)
library(lubridate)
library(readr)
KNMI_20190830 <- read_csv("KNMI_20190830.csv", 
                          col_types = cols(
                             NG = col_double(),
                             PG = col_double(), 
                             PN = col_double(), 
                             PNH = col_double(), 
                             PX = col_double(), 
                             PXH = col_double(), 
                             VVN = col_double(), 
                             VVNH = col_double(), 
                             VVX = col_double(), 
                             VVXH = col_double()
                             )
                          )

data <- KNMI_20190830 %>% select(STN,YYYYMMDD,TN,TNH,TX,TXH,T10N,T10N) %>% arrange(YYYYMMDD)
#set units rights and add mean temp for the day
data <- data %>% mutate(TN=0.1*TN,TX=0.1*TX) %>% mutate(Temp=(TX+TN)/2)
#remove last data just for completeness
data <- data %>% filter(YYYYMMDD != "20190830")
#remove records with NA values for TN,TX
data <- data %>% filter(!is.na(TN)) %>% filter(!is.na(TX))
#add year/mont/day
data <- data %>% mutate(Year=as.double(substr(YYYYMMDD,1,4)),Month=as.double(substr(YYYYMMDD,5,6)),Day=as.double(substr(YYYYMMDD,7,8)))
#calculate mean over all stations
data <- data %>% group_by(YYYYMMDD) %>% summarise(TN=mean(TN),TX=mean(TX),GT=mean(Temp))
GTq=quantile(data$GT,c(0,0.1,0.9,1),na.rm=FALSE,type = 8)
#GTq[1]=GTq[1]-.00005
data <- data %>% mutate(category=cut(GT, breaks=GTq, labels=c("Laag","Middel","Hoog")))
data <- data %>% mutate(Datum=ymd(YYYYMMDD),Year=year(Datum))
p <- ggplot(data,aes(x=yday(Datum),y=GT, color=category))+geom_point()+facet_wrap('Year')
p <- p+scale_color_manual(breaks=c(GTq[1],GTq[2],GTq[3]),values=c('#52a1cc','#7b8736','#d62e01'))
p <- p+ggtitle('Gemiddelde temperatuur over alle weerstations')+labs(subtitle = "color based of terciles 10%,90%",caption = "Datasource: KNMI")+ xlab("Dag")+ylab("Gem.temp")
p


data <- data1 %>% group_by(YYYYMMDD,category)
data_tally <- data %>% group_by(Year) %>% summarise(Count=n())
