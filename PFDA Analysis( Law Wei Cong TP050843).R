#Law Wei Cong
#TP050843
setwd('D:/APU/degree/pfda/PFDA Individual Assignment')
data <- read.csv("4. Hourly weather data.csv")
 View(data)

data$time_hour = strptime(data$time_hour,'%d/%m/%Y %H:%M')
library(ggplot2)
# install.packages("magrittr") # package installations are only needed the first time you use it
# install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)

#To check the dataset before starting any analysis
summary(data)

#Analysis 1
#Tibble is used to analyse the mean of monthly temperature of both airports 
data  %>% 
  group_by(month,origin)%>% 
  summarize(Mean = mean(temp, na.rm=TRUE)) %>% 
  print(n = Inf)

#Analysis2
# find out the wind speed experienced by both airports in 2013 using historgram
ggplot(data = data, mapping = aes(x = wind_speed, na.rm = TRUE,fill=origin)) + 
  geom_histogram(colour ="black") +
  labs(title = 'Histogram of Wind Speed',x = 'Wind speed (mph)') +
  facet_wrap(~origin)+theme(plot.title =element_text(hjust=0.5,face="bold",colour="black"))

#Analysis 3
#tibble and subset fucntion are used to list out the counts of wind speed above 34mph.
tibble2 = data %>%
  group_by(month,origin)%>%
  subset(wind_speed>34)%>%
  tally()%>%
  print(n=Inf)

#Analysis 4
#Frequency of fog formation is found by filtering the matched weather condtions.
data%>%
  group_by(month)%>%
  summarise(fog_formation=temp-dewp)%>%
  filter(fog_formation<4.5)%>%
  tally()%>%
  ggplot(aes(x=month,y=n))+geom_freqpoly(stat="identity") +
  labs(title = 'Frequency Polygon of month against fog ',x = 'Month', y = 'Freq of Fog')+
  theme(plot.title =element_text(hjust=0.5,face="bold",colour="black"))  

#Analysis 5
#Frequency of fog formation is found by filtering the matched weather condtions. The data is categorised
#based on the location of airports 
data%>%
  group_by(month,origin)%>%
  summarise(fog_formation=temp-dewp)%>%
  filter(fog_formation<4.5)%>%
  tally()%>%
  ggplot(aes(x=month,y=n,color=origin))+geom_freqpoly(stat="identity") +
  labs(title = 'Frequency Polygon of month against fog For both airport ',x = 'Month', y = 'Freq of Fog')+
  theme(plot.title =element_text(hjust=0.5,face="bold",colour="black"))

#Analysis6
#boxplot of humidity against visibility is plotted
ggplot(data = data, mapping = aes(x = factor(visib), y = humid)) + 
  geom_boxplot(color="black",fill="darkorange") +
  labs(title = 'Boxplot of Humid against Visibility',x = 'Visible (Miles)', y = 'Humidity(RH)')

#Analsis 7
#To find out frequency of bad visibility times encountered by both airports
data%>%
  group_by(month)%>%
  filter(visib<1.1)%>%
  tally()

#Analysis8
#boxplot of humidity against bad visibility (precip<1.1) is plotted
data%>%
  filter(visib<1.1)%>%
  ggplot( mapping = aes(x = factor(visib), y = humid)) + 
  geom_boxplot(color="black",fill="darkorange") +
  labs(title = 'Boxplot of Humid against Bad Visibility',x = 'Visible (Miles)', y = 'Humidity(RH)')

#Analysis9
#Frequency of snow of both airports during winter. The necessary conditions of forming snow is filtered
winter<-c(12,1,2,3)
data%>%
  group_by(month,origin)%>%
  filter(temp<41,humid<30,month %in% winter)%>%
  tally()%>%
  ggplot(aes(x=month,y=n,color=origin))+geom_freqpoly(stat="identity") +
  labs(title = 'Frequency Polygon of Snow of JFK & LGA during Winter ',x = 'Month', y = 'Freq of Fog')+
  theme(plot.title =element_text(hjust=0.5,face="bold",colour="black")) 

#Analysis10
#Histogram of precipitation throughout the year using ggplot2 categorising them by locations
data %>%
  filter(precip>0) %>%
  ggplot(mapping = aes(x = precip,fill=origin)) + 
  geom_histogram(bins =45,colour="black") +
  labs(title = 'Histogram of Precipitate',x = 'Precipitate (Inch)')+
  theme(plot.title =element_text(hjust=0.5,face="bold",colour="black"))

#Analysis 11
#Histogram of precipitation greater than 0.1 throughout the year
data %>%
  filter(precip>0) %>%
  ggplot(mapping = aes(x = precip,fill=origin)) + 
  geom_histogram(bins =45,colour="black") +
  labs(title = 'Histogram of Precipitate',x = 'Precipitate (Inch)')+
  theme(plot.title =element_text(hjust=0.5,face="bold",colour="black"))

#Analysis 12
#Scatter Plot of Precipitate against Humid using ggplot2
ggplot(data = data, mapping = aes(x = humid, y = precip, color = origin)) + 
  geom_point() + stat_smooth(method = "lm",size=1.0,colour="black") +
  facet_wrap(~origin)+
  labs(title = 'Scatter Plot of Precipitate against Humid',x = 'Humid ', y = 'Precipitate (Inch)')+
  theme(plot.title =element_text(hjust=0.5,face="bold",colour="black"))

#Analysis 13
#A basic windrose is produced using coord_polar function
ggplot(data = data) +
  geom_bar(mapping = aes(x = wind_dir, na.rm = TRUE,color=origin,fill=origin)) +
  coord_polar() +
  labs(title = 'Polar Bar Plot of Wind Direction',x = 'Wind Direction ')

#Analysis 14
#to discover how humidity varies from differnt hours of the day
ggplot(data=data,mapping=aes(x=factor(hour),y=humid))+
  geom_boxplot(color="red", fill="orange", alpha=0.2)+
  labs(title = 'Boxplot of hours against humidity',x = 'Hour(24hour-format)', y = 'Humidity(RH)')

#Analysis 15
#Line graph of pressure over time using as.Date() function and annotations to indicate normal sea level
data %>%
  ggplot( aes(x=as.Date(time_hour), y=pressure,color=origin)) +
  geom_line(color="dodgerblue") +
  ylim(975,1050) +
  annotate(geom="text", x=as.Date("2014-01-01"), y=1045, 
           label="normal sea level pressure") +
  annotate("segment",x=as.Date("2014-01-01"),xend=as.Date("2014-01-01"), y = 1018, yend = 1039, colour = "black", size=.3, alpha=0.6, arrow=arrow())+
  geom_hline(yintercept=1013.2, color="black", size=.5) +                                                               
  labs(title = 'Line Graph of pressure over time ',x = 'Month', y = 'Pressure(mb)')+
  theme(plot.title =element_text(hjust=0.5,face="bold",colour="black"))

#Analysis 16
#Box Plot of Wind Gust against month
data%>%
  filter(wind_gust>0)%>%
  ggplot(aes(x=factor(month),y=wind_gust,color=wind_gust))+geom_boxplot(fill="light blue",color="dark blue")+
  labs(title = 'Box Plot of Wind Gust against month',x = 'Month ', y = 'Wind Gust(MPH)')+
  theme(plot.title =element_text(hjust=0.5,face="bold",colour="black"))

#Analysis 17
#Boxplot of Dew Points during summer
data %>%
  filter(month>5,month<9)%>%
  ggplot( mapping = aes(x = factor(month), y = dewp)) + 
  geom_boxplot(fill="light blue",color="dark blue")+ stat_smooth(method = "lm") +
  geom_hline(yintercept=65, color="red", size=.5) +   
  labs(title = 'Boxplot of Dew Points during summer',x = 'Month ', y = 'Dew Point (F)')+
  theme(plot.title =element_text(hjust=0.5,face="bold",colour="black")) 



data%>%
  group_by(month)%>%
  summarize(avg_windir=mean(wind_dir,na.rm=T))%>%
  ggplot()+geom_bar(mapping = aes(avg_windir, na.rm = TRUE)) + coord_polar() +
  labs(title = 'Bar Chart of Monthly Temperature (Mean)', x = 'Month', y = 'Temperature (F)') + 
  facet_wrap(~month) + theme_bw()+ theme(plot.title = element_text(hjust = 0.5, face = "bold", colour = "black"), 
                                         panel.background = element_rect(fill = "lightcyan1"), plot.background = element_rect(fill = "lightblue1"))



















