
library(tidyverse)
setwd("C:/Users/alexs/Documents/plan372-sp23")

res=read_csv("restaurant_inspections.csv")

#1 insepction scores
ggplot(res, aes(x=SCORE)) + #ggplot inspection score 
  geom_histogram() +
  xlab("Inspection Score") +
  ggtitle("Inspection Scores") +
  theme_minimal() #GGplot histogram with inspection scores


#Mean average of oldest half, mean average of younger half
res=res[order(res$RESTAURANTOPENDATE),] #orders the restaurant open date from oldest to newest
nrow(res)#count rows to divide the data in half with 1938 for older restaurants and 1937 for newer restaurants
older=res[1:1937,] #create new data frame with oldest restauraunts
newer=res[1938:3875,] #create new data frame with newest restaurants
mean(older$SCORE) #mean score of 97.10764
median(older$SCORE)
mean(newer$SCORE) #mean score of 96.74071
median(newer$SCORE)

#Older facilities have a better inspection score average


#3
unique(res$CITY) #check how many city names
res$newcity= str_to_upper(res$CITY) #create new column with upper case cities                 
res$newcity = recode(res$newcity, 
                  "FUQUAY-VARINA" ="FUQUAY VARINA", "Fuquay-Varina" = "FUQUAY VARINA", "Fuquay Varina"="FUQUAY VARINA", "Fuquay-Varina" = "FUQUAY VARINA",
                  "RTP"= "RESEARCH TRIANGLE PARK",
                  "HOLLY SPRING"="HOLLY SPRINGS",
                  "MORRISVILE" = "MORRISVILLE",
                  ) #recode Fuquay Varina, RTP, Holly Springs, and Morrisville

unique(res$newcity) #check to see if there are duplicates of cities

city_score_mean= group_by(res, newcity) %>% #groups the cities and finds the mean score of each city
  summarize(average_score=mean(SCORE))

city_score_mean=city_score_mean[order(-city_score_mean$average_score),] #orders the average inspections scores of each city

#4
unique(res$INSPECTOR) #count how many inspectors
#find the mean average of scores from inspectors 
inspector_score=group_by(res,INSPECTOR)%>%
  summarize(inspector_score_average=mean(SCORE))


inspector_score=inspector_score[order(-inspector_score$inspector_score_average),] #order average inspection score from highest average given to lowest


#could also look at the number of description per inspector
res$empty_description<-is.na(res$DESCRIPTION) 
inspector_descriptions=group_by(res, INSPECTOR)%>%
  summarize(description_count=sum(empty_description)/(sum(empty_description)+sum(!empty_description))) #Divides number of empty descriptions from the total possible number of descriptions



inspector_descriptions=inspector_descriptions[order(inspector_descriptions$description_count),] #ranks the inspectors with the least descriptions with NA to most

#5
#Look at the number of inspectors, restaurants, inspections in each time period, inspections in each city

#average score for restaurants and compare to others, filter only restaurants data
#find sample size for each city

city_inspections_total=res %>% #counts the number of inspections per city
  count(newcity) 

colnames(city_inspections_total)[2]="Number of inspections" #renames column name
city_inspections_total=city_inspections_total[order(-city_inspections_total$`Number of inspections`),] #sorts the data

city_inspections_total <- merge(city_inspections_total,
                 city_score_mean,
                 by.x="newcity")

city_inspections_total=city_inspections_total[order(-city_inspections_total$`Number of inspections`),] #sorts the data

#6
#Average score of the different facilities, ggplot

facility_mean = group_by(res, FACILITYTYPE) %>% #grouped by restaurant and facility type to find mean 
  summarize(Average_Score=mean(SCORE))

ggplot(facility_mean, aes(x=FACILITYTYPE,y=Average_Score)) + #ggplot facility plot average
  geom_col()

#7, restaurant repeat questions 1-5

res_only<-res[res$FACILITYTYPE=="Restaurant",]


#7-1 inspection scores
ggplot(res_only, aes(x=SCORE)) + #ggplot inspection score 
  geom_histogram() +
  xlab("Inspection Score") +
  ggtitle("Restauraunt Inspection Scores") +
  theme_minimal() #GGplot histogram with inspection scores


#7-2, Mean average of oldest half, mean average of younger half
res_only=res_only[order(res_only$RESTAURANTOPENDATE),] #orders the restaurant open date from oldest to newest
nrow(res_only)#count rows to divide the data in half with 1938 for older restaurants and 1937 for newer restaurants
old=res_only[1:1022,] #create new data frame with oldest restaurants
new=res_only[1023:2044,] #create new data frame with newest restaurants
mean(old$SCORE) #mean score of 96.56703
mean(new$SCORE) #mean score of 96.7818

#Newer restaurants have a better inspection score average


#7-3
unique(res_only$CITY) #check how many city names
res_only$newcity= str_to_upper(res_only$CITY) #create new column with upper case cities                 
res_only$newcity = recode(res_only$newcity, 
                     "FUQUAY-VARINA" ="FUQUAY VARINA", "Fuquay-Varina" = "FUQUAY VARINA", "Fuquay Varina"="FUQUAY VARINA", "Fuquay-Varina" = "FUQUAY VARINA",
                     "RTP"= "RESEARCH TRIANGLE PARK",
                     "HOLLY SPRING"="HOLLY SPRINGS",
                     "MORRISVILE" = "MORRISVILLE",
) #recode Fuquay Varina, RTP, Holly Springs, and Morrisville

unique(res_only$newcity) #check to see if there are duplicates of cities

res_city_score_mean=group_by(res_only, newcity) %>% #groups the cities and finds the mean score of each city
  summarize(average_score=mean(SCORE))

res_city_score_mean=res_city_score_mean[order(-res_city_score_mean$average_score),] #orders the average inspections scores of each city

#7-4
unique(res_only$INSPECTOR) #count how many inspectors
#find the mean average of scores from inspectors 
res_inspector_score=group_by(res_only,INSPECTOR)%>%
  summarize(inspector_score_average=mean(SCORE))


res_inspector_score=res_inspector_score[order(-res_inspector_score$inspector_score_average),] #order city average score from highest average given to lowest


#could also look at the number of description per inspector
res_only$empty_description<-is.na(res_only$DESCRIPTION) 
res_inspector_descriptions=group_by(res_only, INSPECTOR)%>%
  summarize(description_count=sum(empty_description)/(sum(empty_description)+sum(!empty_description))) #Divides number of empty descriptions from the total possible number of descriptions


res_inspector_descriptions=res_inspector_descriptions[order(res_inspector_descriptions$description_count),] #ranks the inspectors with the least descriptions with NA to most

#7-5
#Look at the number of inspectors, restaurants, inspections in each time period, inspections in each city

#average score for restaurants and compare to others, filter only restaurants data
#find sample size for each city

res_city_inspections_total=res_only %>% #counts the number of inspections per city
  count(newcity) 

colnames(res_city_inspections_total)[2]="Number of inspections" #renames column name
res_city_inspections_total=res_city_inspections_total[order(-res_city_inspections_total$`Number of inspections`),] #sorts the data

res_city_inspections_total <- merge(res_city_inspections_total,
                                city_score_mean,
                                by.x="newcity")

res_city_inspections_total=res_city_inspections_total[order(-res_city_inspections_total$`Number of inspections`),] #sorts the data

