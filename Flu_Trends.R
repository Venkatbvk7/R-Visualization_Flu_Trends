library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)

#1.a 

flu.us <-read.table("https://www.google.org/flutrends/about/data/flu/us/data.txt",sep = ",", header = TRUE, skip = 11)
wday(flu.us$Date, label=TRUE) # Check if every week begins with Sunday
which(wday(flu.us$Date, label=TRUE)== "Sun") # Rechecking
flu.states<- flu.us[,c(1,3:53)] # Select Date and States
flu.states<-gather(flu.states,States,Value,-Date) # reshape the dataset


#1.b

flu.states$Date <- ymd(flu.states$Date)
flu.states<- cbind(flu.states,Year = year(flu.states$Date), Month = month(flu.states$Date))
flu.states %>% filter (Year==2014 & States=="Iowa") %>% tally(Value) # Flu Cases in Iowa in 2014
flu.states %>% filter (Year==2014 & States=="Iowa") %>% 
  group_by(Month)%>% summarise(Sum=sum(Value))%>% filter(Sum==max(Sum)) # Month with Maximum Number of flu cases in 2014


#1.c

flu.states$Year.Month<-round_date(flu.states$Date,unit = "month")
class(flu.states$Year.Month)
flu.states<-flu.states[!(is.na(flu.states$Value)),]
flu.states.tally<-flu.states %>% group_by(States,Year.Month)%>%summarise(Sum=sum(Value))


library(scales)

flu.states %>% filter (States=="Iowa") %>% group_by(Year.Month)%>%summarise(Sum=sum(Value))%>%
  ggplot(aes(x=Year.Month, y=Sum))+geom_line()+
  scale_x_date(breaks =date_breaks("12 weeks"),labels = date_format("%Y-%m"))+theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Date displayed in 12 week intervals")+ylab("Number of Flu Cases")+
  labs(title="Flu Cases in Iowa State Plotted on a monthly basis") 

# From the graph it can be seen that there is a sawtooth pattern, the flu cases are high mostly 
# during the 12th and 1st month of each year and low during the middle of each year. It is an indication of flu
# cases being high during the winter season and low in the spring and summer seasons. (in Iowa)

#1.d

Iowa<-flu.states %>% filter (States=="Iowa") %>% group_by(Year,Month)%>%summarise(Sum=sum(Value))
Iowa<-data.frame(Iowa)
Iowa$Year<-as.factor(Iowa$Year)

library(directlabels) # used this package to mark labels in graph.

ggplot(data=Iowa,aes(Month,Sum,color=Year))+geom_point()+geom_line()+
  scale_x_continuous(breaks = seq(1,12,by=1))+scale_y_continuous(breaks = seq(0,30000,by=5000))+
  geom_dl(aes(label = Year), method = list(dl.combine("first.points", "last.points"), cex = 0.7))+
  xlab("Months Represented as Number")+ylab("Number of Flu Cases")+
  labs(title="Flu Cases in Iowa State for each month from 09/2003 to 08/2015")+theme(legend.position="bottom") 

# ggplot(data=Iowa,aes(Month,Sum,color=Year))+geom_point()+geom_line()+ facet_wrap(~Year)+
#   scale_x_continuous(breaks = seq(1,12,by=1))+scale_y_continuous(breaks = seq(0,30000,by=5000))+
#   geom_dl(aes(label = Year), method = list(dl.combine("first.points", "last.points"), cex = 0.7))+
#   xlab("Months Represented as Number")+ylab("Number of Flu Cases")+
#   labs(title="Flu Cases in Iowa State for each month from 09/2003 to 08/2015")

# In Iowa, the flu cases are usually high during the 1st and last months
# indicated by the overall parabolic structure of the graph. It can also be seen that 
# there were lesser flu cases from 2004-2007 and in 2010. There is also an odd peak 
# in the year 2009.


#1.e

flu.us.2014<-flu.states %>% filter (Year==2014)%>%group_by(States)%>%summarise(Sum=sum(Value))
flu.us.2014$States<-tolower(flu.us.2014$States)
flu.us.2014$States<-gsub("."," ",flu.us.2014$States,fixed = TRUE)

states <- map_data("state")
flu.map<- merge(states, flu.us.2014, by.x="region", by.y="States")
flu.map <- flu.map %>% arrange(order)
flu.map %>% ggplot(aes(x = long, y=lat, group=group)) + 
  geom_polygon(aes(fill =Sum),colour="black")+
  scale_fill_gradient2(midpoint = min(flu.map$Sum))+
  xlab("Longitude")+ylab("Latitude")+
  labs(title="Total number of flu cases for all US states in 2014") 

flu.map %>% distinct(region) # To show there are 49 states








