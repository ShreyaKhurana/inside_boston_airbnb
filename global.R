### Wordcloud packages
# install.packages("tm")
# install.packages("SnowballC")
# install.packages("RColorBrewer")
# install.packages("wordcloud")
# install.packages("biclust")
# install.packages("cluster")
# install.packages("igraph")
# install.packages("fpc")
# install.packages("ggplot2")
# install.packages('memoise')
# 
# ### R shiny packages
# install.packages('shiny')
# install.packages('shinydashboard')
# install.packages("corrplot")

# Libs for map plotting
# install.packages("leaflet")

# Checking if all packge are installed

list.of.packages <- c("ggmap","knitr","dplyr","ggplot2","tidyverse","stringr","lubridate","DT",
                      "caret","leaflet","corrplot","boot", "tm", "SnowballC", "RColorBrewer", "wordcloud",
                      "biclust", "cluster", "igraph", "fpc", "memoise", "shiny", "shinydashboard",
                      "tidytext")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; 
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org"); 
lapply(list.of.packages, library, character.only=T)

#  Importing libraries

library(shiny)
library(shinydashboard)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(biclust)
library(cluster)
library(igraph)
library(fpc)
library(ggplot2)
library(memoise)

library(leaflet)

#  For EDA: Corrplot and fancy houses

library(knitr)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(DT)
library(caret)
library(leaflet)
library(corrplot)
library(boot) 

# For sentiment analysis
library(tidytext)
library(dplyr)
library(tidyverse)

# final = merge(airbnb, listing, by.x ='listing_id', by.y = 'id', all.x = TRUE)
# write.csv(final, 'reviews_all.csv')

############################################
#             WORD CLOUD                   #
############################################

airbnb = read.csv('reviews_all.csv', stringsAsFactors=F)

myfunction <- memoise(function(nbr, minRating, maxRating, host){
  
  if (host == 'Yes') {
    host = 't'
  } else {
    host = 'f'
  }
  
  airbnb <- subset(airbnb, (neighbourhood_cleansed %in% nbr) & 
                     (review_scores_rating >= minRating) & 
                     (review_scores_rating <= maxRating) &
                     (host_is_superhost == host))
  #create a corpus
  airbnbcorp <- Corpus(VectorSource(airbnb$comments))
  #start removing non essentials, lowering capital letters, and getting rid of stop words
  airbnbcorp <- tm_map(airbnbcorp, removePunctuation)
  airbnbcorp <- tm_map(airbnbcorp, removeNumbers)
  airbnbcorp <- tm_map(airbnbcorp, tolower)
  airbnbcorp <- tm_map(airbnbcorp, removeWords, stopwords('english'))
  airbnbcorp <- tm_map(airbnbcorp, removeWords, c("boston", "room", "location",
                                                  "bed","home","two" , 
                                                  "need", "bedroom",
                                                  "apartment", "like",
                                                  "street", "one", 
                                                  "kitchen", "bathroom",
                                                  "stay", "place",
                                                  "house", "will",
                                                  "felt", "time", "well",
                                                  "around", "airbnb", "day",
                                                  "next", "time", "just", "also"))
  airbnbcorp <- tm_map(airbnbcorp, stemDocument)
  tdm1<- DocumentTermMatrix(airbnbcorp, control = list(weighting = weightTf, stopwords = FALSE, minWordLength = 2))
  tdm1 <- removeSparseTerms(tdm1, .99)
  freq <- colSums(as.matrix(tdm1))
  # print(freq)
  # print(names(freq))
  sort(freq, decreasing = TRUE)
  
})


#################################
# Now function for map plotting #
#################################


listing<- read.csv("listings_v1.csv") 
library(leaflet)
attach(listing)

map_data <- function(nbr2, minPrice, maxPrice, property,
                             room, superhost, hostverified,
                             accom, bathroom, instant,
                             cancel, wifi, chk, telev, airc, 
                             family, pet, parking, brkfst){
  
  # Coding the variables as in the data file
  
  if (superhost == 'Yes') { 
    superhost = 't'
  } else { 
      superhost = c('f', 't')
  }
  
  if (hostverified == 'Yes') { 
    hostverified = 't'
  } else { 
    hostverified = c('f', 't')
  }
  
  if (instant == 'Yes') { 
    instant = 't'
  } else { 
    instant = c('f', 't')
  }
  
  if (wifi == 'Yes') { 
    wifi = 1
  } else { 
    wifi = c(0, 1)
  }
  # print('Internet')
  # print(internet)
  if (chk == 'Yes') { 
    chk = 1
  } else { 
    chk = c(0, 1)
  }
  
  if (telev == 'Yes') { 
    telev = 1
  } else { 
    telev = c(0, 1)
  }
  
  if (airc == 'Yes') { 
    airc = 1
  } else { 
    airc = c(0, 1)
  }
  
  if (family == 'Yes') { 
    family = 1
  } else { 
    family = 0
  }
  
  if (pet == 'Yes') { 
    pet = 1
  } else { 
    pet = 0
  }
  
  if (parking == 'Yes') { 
    parking = 1
  } else { 
    parking = c(0, 1)
  }
  
  if (brkfst == 'Yes') { 
    brkfst = 1
  } else { 
    brkfst = c(0, 1)
  }
  
  if (TRUE %in% (cancel %in% 'Super Strict')) {
    cancel = 'super_strict_30'
  } else {
    cancel = tolower(cancel)
  }
  
  #  Subsetting the data file according to filters
  
  df = data.frame(latitude = listing$latitude,longitude = listing$longitude)
  dff <- subset(listing, (neighbourhood_cleansed %in% nbr2) & (price >= minPrice) & (price <= maxPrice) & 
                  (property_type %in% property) & (room_type %in% room) & (host_is_superhost %in% superhost) & 
                  (host_identity_verified %in% hostverified) & (accommodates >= accom) & 
                  (bathrooms >= bathroom) & (instant_bookable %in% instant) & (cancellation_policy %in% cancel) & 
                  (internet %in% wifi) & (TV %in% telev) & (AC %in% airc) & (free.parking %in% parking) & 
                  (checkin %in% chk) & (family.kid.fiendly == family) & (pets == pet) & 
                  (breakfast %in% brkfst))
  # dff <- airbnb[(listing$neighbourhood_cleansed == nbr2) & (listing$price >= minPrice) & (listing$price <= maxPrice) &
  #              (listing$property_type == property) & (listing$room_type == room) & (listing$host_is_superhost == superhost) & 
  #              (listing$host_identity_verified == hostverified) & (listing$accommodates >= accomodates) &
  #              (listing$bathrooms == bathroom) & (listing$instant_bookable == instant) & (listing$cancellation_policy %in% cancel) & 
  #              (listing$internet == internet) & (listing$TV == tv) & (listing$AC == ac) & (listing$free.parking == parking) & 
  #              (listing$checkin == checkin) & (listing$family.kid.fiendly == family) & (listing$pets == pets) &
  #              (listing$breakfast %in% breakfast)]
  
  ##explanation setting
  return(dff)
}
##map
# 
# install.packages('devtools')
# devtools::install_github('ayayron/shinydnd')

# map_data('East Boston', 10, 4000, 'House', 'Private room', 'Yes', 'Yes', 2, 2, 'Yes', 'Flexible', )

############################################
#              Global datasets             #
############################################

mydata<-read.csv("Mydata.csv")
mydata<-mydata[,-1]
attach(mydata)


#  Find set of numerical and factor variables for different boxplots

namec<-c()
named<-c()
for (i in 1:dim(mydata)[2]){
  if (as.numeric(is.numeric(mydata[1,i]))==1) {namec<-c(namec,colnames(mydata)[i])}
  else{named<-c(named,colnames(mydata)[i])}
}

### For Correlation Plot

namec<-c(namec[20],namec[-c(1,20,34:37)])
mydata0<-mydata[,namec]


mydatad <- mydata[,named]
mydata$log.Price <- log(mydata$price)
