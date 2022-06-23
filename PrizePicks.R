install.packages("httr")
install.packages("jsonlite")

library(httr)
library(jsonlite)
library(dplyr)
library(tidyverse)

## USE THIS TO GET LEAGUE DATA
leagues<- GET('http://partner-api.prizepicks.com/leagues')
leaguesdata<-fromJSON(rawToChar(leagues$content),flatten = TRUE,simplifyDataFrame = TRUE)
df<-leaguesdata$data

str(leaguesdata$included)

df$relationships.projection_filters.data<-NULL
str(df)

write.csv(df,'league.csv')
            

## USE THIS TO GET PROJECTION DATA

proj<-GET('https://partner-api.prizepicks.com/projections?league_id=9&per_page=1000&single_stat=true')
projdata<-fromJSON(rawToChar(proj$content),flatten = TRUE,simplifyDataFrame = TRUE)
dataobject<-projdata$data

includedobject<-projdata$included

str(includedobject)


str(dataobject)
write.csv(dataobject, "projections.csv")

str(projdata)

str(proj$content)
str(proj)
str(projdata)


 include_new_player_attributes = TRUE

##write.csv(leaguesdata$data,'league.csv')
##leagues<- GET('http://partner-api.prizepicks.com/leagues')
##leaguesdata<-fromJSON(rawToChar(leagues$content),flatten = TRUE)