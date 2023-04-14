library(dplyr)
library(ggplot2)
library(skimr)
library(DataExplorer)
library(psych)
library(tidyverse)
Total = 129880
df = read.csv("airline_passenger_satisfaction.csv",header = T)
df = data.frame(df)

#Type of travel -> Medium positive
#Class -> Medium-strong positive
#Distance -> Medium positive

#Online Booking -> Medium positive
#Seat comfort -> Medium positive
#Leg Room -> Medium positive
#Wifi -> Medium-strong positive
#Entertainment -> 


#New column classifying the customer in Age Ranges
df = df %>% mutate(Age_Ranges = if_else(18<= Age & Age <= 25,"Young Adult",
                                       if_else(26 <= Age & Age <= 35,"Adult",
                                               if_else(36 <= Age & Age <= 45,"Middle Age",
                                                       if_else(46 <= Age & Age <= 55,"Mature",
                                                          if_else(55 < Age,"Senior", "Kid"))))))


#New column classifying the customer in Flight Distance Ranges
df = df %>% mutate(Distance_Range = if_else(0<= Flight.Distance & Flight.Distance <= 1000,"0-800",
                                        if_else(801 <= Flight.Distance & Flight.Distance <= 1600,"801-1600",
                                                if_else(1601 <= Flight.Distance & Flight.Distance <= 2400,"1601-2400",
                                                        if_else(2401 <= Flight.Distance & Flight.Distance <= 3200,"2401-3200",
                                                                if_else(3200 < Flight.Distance,"3200+", "NULL"))))))

#Satisfaction per age range
ggplot(df, aes(x = Age_Ranges, fill = Satisfaction)) +     
  geom_bar( position = 'dodge')+
  geom_text(stat='count', aes(label=..count..), vjust=0.5,
            position = position_dodge(0.9), size=3.5)+
  labs(title = "bar plot", subtitle =" ",caption = '', 
       x="Age Ranges", y="Count")+theme_bw()

#Satisfaction per distance range
ggplot(df, aes(x = Distance_Range, fill = Satisfaction)) +     
  geom_bar( position = 'dodge')+
  geom_text(stat='count', aes(label=..count..), vjust=0.5,
            position = position_dodge(0.9), size=3.5)+
  labs(title = "bar plot", subtitle =" ",caption = '', 
       x="Distance Ranges", y="Count")+theme_bw()



#Gender vs Satisfaction
satisfied_gender = df %>% filter(Satisfaction == 'Satisfied') %>% group_by(Gender) %>% 
  summarize(satisfied = n())
dissatisfied_gender = df %>% filter(Satisfaction == 'Neutral or Dissatisfied') %>% group_by(Gender) %>% 
  summarize(dissatisfied = n())


satisfaction_gender = data.frame(satisfied_gender, dissatisfied_gender)
rownames(satisfaction_gender) = c('Female','Male')
satisfaction_gender = satisfaction_gender %>% select(satisfied, dissatisfied)
satisfaction_gender

chisq.test(satisfaction_gender)
#The p-value is less than 0.5 so the null hypothesis is rejected
#There is a relationship between gender and satisfaction
phi(satisfaction_gender)
#The phi coefficient indicates there is little association between these two variables


#Age Range vs Satisfaction
satisfied_Age = df %>% filter(Satisfaction == 'Satisfied') %>% group_by(Age_Ranges) %>% 
  summarize(satisfied = n())
dissatisfied_Age = df %>% filter(Satisfaction == 'Neutral or Dissatisfied') %>% group_by(Age_Ranges) %>% 
  summarize(dissatisfied = n())


satisfaction_Age = data.frame(satisfied_Age, dissatisfied_Age)
rownames(satisfaction_Age) = c('Adult','Kid', 'Mature', 'Middle Age', 'Senior', 'Young Adult')
satisfaction_Age = satisfaction_Age %>% select(satisfied, dissatisfied)
satisfaction_Age

chisq.test(satisfaction_Age)
X_squared = 6999.9
#The p-value is less than 0.5 so the null hypothesis is rejected
#There is a relationship between Age Range and satisfaction
sqrt(X_squared/Total)
#The phi coefficient indicates there is little association between these two variables


#Customer Type vs Satisfaction
satisfied_Type = df %>% filter(Satisfaction == 'Satisfied') %>% group_by(Customer.Type) %>% 
  summarize(satisfied = n())
dissatisfied_Type = df %>% filter(Satisfaction == 'Neutral or Dissatisfied') %>% group_by(Customer.Type) %>% 
  summarize(dissatisfied = n())


satisfaction_Type = data.frame(satisfied_Type, dissatisfied_Type)
rownames(satisfaction_Type) = c('First-Time', 'Returning')
satisfaction_Type = satisfaction_Type %>% select(satisfied, dissatisfied)
satisfaction_Type

chisq.test(satisfaction_Type)
#The p-value is less than 0.5 so the null hypothesis is rejected
#There is a relationship between Customer Type and satisfaction
phi(satisfaction_Type)
#The phi coefficient indicates there is little association between these two variables


#Type of travel vs Satisfaction
satisfied_Travel = df %>% filter(Satisfaction == 'Satisfied') %>% group_by(Type.of.Travel) %>% 
  summarize(satisfied = n())
dissatisfied_Travel = df %>% filter(Satisfaction == 'Neutral or Dissatisfied') %>% group_by(Type.of.Travel) %>% 
  summarize(dissatisfied = n())


satisfaction_Travel = data.frame(satisfied_Travel, dissatisfied_Travel)
rownames(satisfaction_Travel) = c('Business', 'Personal')
satisfaction_Travel = satisfaction_Travel %>% select(satisfied, dissatisfied)
satisfaction_Travel

chisq.test(satisfaction_Travel)
#The p-value is less than 0.5 so the null hypothesis is rejected
#There is a relationship between Type of travel and satisfaction
phi(satisfaction_Travel)
#The phi coefficient indicates there is a medium positive association between these two variables


#Class vs Satisfaction
satisfied_Class = df %>% filter(Satisfaction == 'Satisfied') %>% group_by(Class) %>% 
  summarize(satisfied = n())
dissatisfied_Class = df %>% filter(Satisfaction == 'Neutral or Dissatisfied') %>% group_by(Class) %>% 
  summarize(dissatisfied = n())


satisfaction_Class = data.frame(satisfied_Class, dissatisfied_Class)
satisfaction_Class
rownames(satisfaction_Class) = c('Business', 'Economy', 'Economy Plus')
satisfaction_Class = satisfaction_Class %>% select(satisfied, dissatisfied)
satisfaction_Class

chisq.test(satisfaction_Class)
X_squared = 32906
#The p-value is less than 0.5 so the null hypothesis is rejected
#There is a relationship between Class and satisfaction
sqrt(X_squared/Total)
#The phi coefficient indicates there is medium-strong positive association between these two variables

#Distance vs Satisfaction
satisfied_Distance = df %>% filter(Satisfaction == 'Satisfied') %>% group_by(Distance_Range) %>% 
  summarize(satisfied = n())
dissatisfied_Distance = df %>% filter(Satisfaction == 'Neutral or Dissatisfied') %>% group_by(Distance_Range) %>% 
  summarize(dissatisfied = n())


satisfaction_Distance = data.frame(satisfied_Distance, dissatisfied_Distance)
rownames(satisfaction_Distance) = c('0-800', '1601-2400', '2401-3200', ' 3200+', '801-1600')
satisfaction_Distance = satisfaction_Distance %>% select(satisfied, dissatisfied)
satisfaction_Distance

chisq.test(satisfaction_Distance)
X_squared = 12145
#The p-value is less than 0.5 so the null hypothesis is rejected
#There is a relationship between Class and satisfaction
sqrt(X_squared/Total)
#The phi coefficient indicates there is medium positive association between these two variables

#Ease of Online Booking vs Satisfaction
satisfied_Online = df %>% filter(Satisfaction == 'Satisfied') %>% group_by(Ease.of.Online.Booking) %>% 
  summarize(satisfied = n())
dissatisfied_Online = df %>% filter(Satisfaction == 'Neutral or Dissatisfied') %>% group_by(Ease.of.Online.Booking) %>% 
  summarize(dissatisfied = n())


satisfaction_Online = data.frame(satisfied_Online, dissatisfied_Online)
rownames(satisfaction_Online) = c('0', '1', '2', '3', '4','5')
satisfaction_Online = satisfaction_Online %>% select(satisfied, dissatisfied)
satisfaction_Online

chisq.test(satisfaction_Online)
X_squared = 12847
#The p-value is less than 0.5 so the null hypothesis is rejected
#There is a relationship between Online Booking and satisfaction
sqrt(X_squared/Total)
#The phi coefficient indicates there is medium positive association between these two variables


#Seat comfort vs Satisfaction

satisfied_Seat = df %>% filter(Satisfaction == 'Satisfied') %>% group_by(Seat.Comfort) %>% 
  summarize(satisfied = n())
dissatisfied_Seat = df %>% filter(Satisfaction == 'Neutral or Dissatisfied') %>% group_by(Seat.Comfort) %>% 
  summarize(dissatisfied = n())

satisfied_Seat <- rbind(c(0,0), satisfied_Seat)

satisfaction_Seat = data.frame(satisfied_Seat, dissatisfied_Seat)
rownames(satisfaction_Seat) = c('0', '1', '2', '3', '4','5')
satisfaction_Seat = satisfaction_Seat %>% select(satisfied, dissatisfied)
satisfaction_Seat

chisq.test(satisfaction_Seat)
X_squared = 19539
#The p-value is less than 0.5 so the null hypothesis is rejected
#There is a relationship between Seat comfort and satisfaction
sqrt(X_squared/Total)
#The phi coefficient indicates there is medium positive association between these two variables


#Leg Room vs Satisfaction

satisfied_Leg = df %>% filter(Satisfaction == 'Satisfied') %>% group_by(Leg.Room.Service) %>% 
  summarize(satisfied = n())
dissatisfied_Leg = df %>% filter(Satisfaction == 'Neutral or Dissatisfied') %>% group_by(Leg.Room.Service) %>% 
  summarize(dissatisfied = n())

satisfaction_Leg = data.frame(satisfied_Leg, dissatisfied_Leg)
rownames(satisfaction_Leg) = c('0', '1', '2', '3', '4','5')
satisfaction_Leg = satisfaction_Leg %>% select(satisfied, dissatisfied)
satisfaction_Leg

chisq.test(satisfaction_Leg)
X_squared = 15201
#The p-value is less than 0.5 so the null hypothesis is rejected
#There is a relationship between Leg Room and satisfaction
sqrt(X_squared/Total)
#The phi coefficient indicates there is medium positive association between these two variables


#WiFi vs Satisfaction

satisfied_Wifi = df %>% filter(Satisfaction == 'Satisfied') %>% group_by(In.flight.Wifi.Service) %>% 
  summarize(satisfied = n())
dissatisfied_Wifi = df %>% filter(Satisfaction == 'Neutral or Dissatisfied') %>% group_by(In.flight.Wifi.Service) %>% 
  summarize(dissatisfied = n())


satisfaction_Wifi = data.frame(satisfied_Wifi, dissatisfied_Wifi)
satisfaction_Wifi
rownames(satisfaction_Wifi) = c('0', '1', '2', '3', '4','5')
satisfaction_Wifi = satisfaction_Wifi %>% select(satisfied, dissatisfied)
satisfaction_Wifi

chisq.test(satisfaction_Wifi)
X_squared = 35891
#The p-value is less than 0.5 so the null hypothesis is rejected
#There is a relationship between Wifi and satisfaction
sqrt(X_squared/Total)
#The phi coefficient indicates there is medium positive association between these two variables


#Entertainment vs Satisfaction

satisfied_TV = df %>% filter(Satisfaction == 'Satisfied') %>% group_by(In.flight.Entertainment) %>% 
  summarize(satisfied = n())
dissatisfied_TV = df %>% filter(Satisfaction == 'Neutral or Dissatisfied') %>% group_by(In.flight.Entertainment) %>% 
  summarize(dissatisfied = n())

satisfied_TV <- rbind(c(0,0), satisfied_TV)

satisfaction_TV = data.frame(satisfied_TV, dissatisfied_TV)
satisfaction_TV
rownames(satisfaction_TV) = c('0', '1', '2', '3', '4','5')
satisfaction_TV = satisfaction_TV %>% select(satisfied, dissatisfied)
satisfaction_TV

chisq.test(satisfaction_TV)
X_squared = 23072
#The p-value is less than 0.5 so the null hypothesis is rejected
#There is a relationship between entertainment and satisfaction
sqrt(X_squared/Total)
#The phi coefficient indicates there is medium positive association between these two variables




