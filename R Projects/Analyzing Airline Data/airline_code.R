################################################
# IST387, Standard Heading

# Student name: Alejandro Pesantez

# Final

# Date due: 12/04/20

# Attribution statement: (choose only one)

# 1. I did this final by myself, with help from the book and the professor

# Run these three functions to get a clean test of homework code

dev.off() # Clear the graph window

cat('\014')  # Clear the console

rm(list=ls()) # Clear user objects from the environment

# Set working directory 

setwd("~/Desktop/IST387/Final") # Change to the folder containing your homework data files

#1

library(tidyverse)

airData <- read_csv("IST387-Fall2020-airData-aepesant.csv")

#2

str(airData)

hist(airData$age)
#Creates histogram of ages
#the histogram produced is the ages of the people that rode this airline. The majority of the passengers are between
#the ages of 40 and 50 with the minority being between 10 and 20. The histogram is close to being normally distributed
#however, it is skewed a tiny bit to the left.
hist(airData$departure_delay_in_minutes)
#Creates histogram of departure delays
#this histogram shows the frequency of the departure delays in minutes. This histogram shown is very much skewed to the right
#and the majority of departure flights are delayed between 0 and 50 minutes, while departure flights that are delayed between 300 and 350 minutes
#don't exist in the dataset.
hist(airData$arrival_delay_in_minutes)
#Creates histogram of delays on arrival flights 
#this histogram shows the frequency of arrival delays in minutes. This is also very much skewed to the right and shows that the majority of arrival
#flights that are delayed is between 0 to 50 minutes, while arrival flights that are delayed between 250 and 300 minutes don't exist.
hist(airData$flight_time_in_minutes)
#Creates histogram of how long flights were
#this histogram shows the frequency of flight times in minutes. The histogram seems to have a bimodal distribution, since
#there are two peaks. The histogram also shows that most flight durations are between 50 to 100 minutes while the least amount of flight durations
#are between 0 to 50 minutes.
hist(airData$flight_distance)
#creates histogram of flight distances
#this histogram shows the frequency of flight distances. In this case there really isn't any pattern of the distribution of the data given.
#It shows that most flights are around 2000 to 2500 miles, while the least amount of flights are around 3000 to 3500 miles.
hist(airData$likelihood_to_recommend)
#creates histogram of how passangers rated how likely they were to recommend this airline
#this histogram shows the frequency of how people rated their likelihood of recommending this airline to someone. It's hard to tell if there is a pattern
#in this distribution, if anything it seems as if it is skewed to the left. It also shows that the majority of people gave a rating of a 9 which means it is 
#probably a very good airline, while the least amount of people gave a rating of 1.

table(airData$gender)
#Creates table of how many males and females were in dataset
#When looking at this table it produces how many male and female passengers there were. It seems as if they were pretty
#even with 75 females and 74 males.
table(airData$destination_city)
#Creates table of how what cities people traveled to, and the amount of occurrences of each
#This shows where people traveled to the most. It seems as if the most common destinations were San Francisco and New York City
#which were tied at 14.
table(airData$origin_city)
#Creates table showing the occurrences of where people traveled from
#This shows where most people traveled from. This table shows that the most common place people traveled from was New York City with 17.
table(airData$type_of_travel)
#Creates table showing occurrences of what type of travel passengers were doing
#This table shows what was the most common type of travel. It shows that most people traveled for business purposes with 75, and the next
#most common was for personal travel with 60, and the least common way of travel was with mileage tickets with 14.
table(airData$flight_cancelled)
#Creates table of how many flights were canceled and not
#This is a table showing how many flights were cancelled in the data set. This shows that out of the 149 flights in the dataset only 
#15 were cancelled while the other 134 were not.
table(airData$free_text)
#this doesn't show anything of value.

#3

boxplot(airData$likelihood_to_recommend~airData$gender)
#Creates boxplot of genders compared to the likelihood of recommending this airline
#This shows a boxplot of how genders gave their likelihood of recommending this airline. The median of both 
#female and male is 8, however the minimum for females was lower than that of males, meaning the worst rating a female
#gave was worse than the worst rating a male gave.
boxplot(airData$likelihood_to_recommend~airData$type_of_travel)
#This creates a boxplot of the type of travel passengers did and comparing it to their likelihood to recommend this airline
#This boxplot shows how the difference in travel effected the ratings of how likely people were to reccommend this airline.
#This shows that people who traveled for business were most likely to recommend this airline because the median of the boxplot is 
#higher than the rest, while people who traveled using mileage tickets were second most likely to reccommend this airline because there
#median was only 1 rating lower than business travel. Last but not least people who traveled for vacations were least likely to
#recommend this airline because their boxplot is lower than both of the others and their median rating is 4 ratings below those who
#traveled using mileage tickets.
boxplot(airData$likelihood_to_recommend~airData$flight_cancelled)
#This creates a boxplot of whether or not a passengers flight was cancelled, and their likelihood of recommending the airline
#This boxplot shows how people with their flight being canceled versus it not being csnceled rated their likelihood
#of recommending this airline. People that didn't get their flight canceled had a median rating of 8 and have a generally
#higher up boxplot than those who did get their flight cancelled. People that did get their flight cancelled had a median rating of
#6 and is lower than the other boxplot. This surprised me bcause I thought their median rating would be much lower.

#4

airData <- na.omit(airData)
#eliminates missing data

#5

library(ggplot2)
library(ggmap)
usmap <- get_stamenmap(bbox=c(left=-167.50, bottom=7.45, right=-50.19, top=71.58),
                       zoom=3, maptype = "toner")
#gets map of the usa
ggmap(usmap) + geom_point(data=airData, aes(x=dlong, y=dlat,
                                            color=likelihood_to_recommend))
#creates the map we want with the coordinates of the destination cities, with the coordinates(dots) being shaded by how that place
#is likely to recommend the airline

#new data sets

detractors <- airData %>% filter(likelihood_to_recommend < 7)
#creates new data frame with only rows that have a likelihood to recommend that are less than 7

promoters <- airData %>% filter(likelihood_to_recommend > 8)
#creates new data frame with only rows that have a likelihood to recommend that are greater than 8

#detractor map

usmap <- get_stamenmap(bbox=c(left=-167.50, bottom=7.45, right=-50.19, top=71.58),
                       zoom=3, maptype = "toner")
ggmap(usmap) + geom_point(data=detractors, aes(x=dlong, y=dlat,
                                            color=likelihood_to_recommend))
#creates map of destination cities that are in the detractor dataset created above, and then is shaded by their likelihood to recommend

#promoter map

usmap <- get_stamenmap(bbox=c(left=-167.50, bottom=7.45, right=-50.19, top=71.58),
                       zoom=3, maptype = "toner")
ggmap(usmap) + geom_point(data=promoters, aes(x=dlong, y=dlat,
                                               color=likelihood_to_recommend))
#creates map of destination cities that are in the promoters dataset created above, and then is shaded by their likelihood to recommend


#detractor origin map

usmap <- get_stamenmap(bbox=c(left=-167.50, bottom=7.45, right=-50.19, top=71.58),
                       zoom=3, maptype = "toner")
ggmap(usmap) + geom_point(data=detractors, aes(x=olong, y=olat,
                                               color=likelihood_to_recommend))
#creates map of origin cities that are in the detractors dataset created above, and then is shaded by their likelihood to recommend
#This is a map of where our passengers who gave us a rating of 6 or below, on how likely they would recommend this airline, are from. 
#As you can see it seems as if these types of passengers are mostly located on the coasts and near the water, such as Hawaii, Florida, 
#New York and California. 

#promoter origin map

usmap <- get_stamenmap(bbox=c(left=-167.50, bottom=7.45, right=-50.19, top=71.58),
                       zoom=3, maptype = "toner")
ggmap(usmap) + geom_point(data=promoters, aes(x=olong, y=olat,
                                              color=likelihood_to_recommend))
#creates map of origin cities that are in the promoters dataset created above, and then is shaded by their likelihood to recommend
#This is a map of where our passengers who gave us a rating of 9 or above, on how likely they would recommend this airline, are from. 
#As you can see it seems as if these types of passengers are located in the big cities such as New York, Los Angeles, and Chicago.  



#6

library(quanteda)

#corpus
detractCorpus <- corpus(detractors$free_text)
#this creates a corpus of the text writen by detrators

promoteCorpus <- corpus(promoters$free_text)
#this creates a corpus of the text writen by promoters

#dfm
detract_dfm <- dfm(detractCorpus,remove_punct = TRUE, remove = stopwords('english'))
#makes a document feature matrix of the words in our detract corpus, and removes punctuation and stop words.

promote_dfm <- dfm(promoteCorpus,remove_punct = TRUE, remove = stopwords('english'))
#makes a document feature matrix of the words in our promote corpus, and removes punctuation and stop words.

#word clouds

textplot_wordcloud(detract_dfm, min_count = 1)
#this creates a word cloud of the document feature matrix from the detractors, with the word only having to appear once.
#It seems to me that most of the detractors are unhappy about the service they get, their flight might’ve been delayed, 
#or something went wrong with their luggage.


textplot_wordcloud(promote_dfm, min_count = 1)
#this creates a word cloud of the document feature matrix from the promoters, with the word only having to appear once.
#Here it seems as if the promoters really liked the comfortability they had on their flights. This includes their seating, 
#the food, and even the service.


#7

posWords <- scan("positive-words.txt", character(0), sep = "\n")
#reads in positive words from the positive words text given
negWords <- scan("negative-words.txt", character(0), sep = "\n")
#reads in negative words from the positive words text given

posWords <- posWords[-1:-34]
#removes first 34 rows from posWords that don't contain words
negWords <- negWords[-1:-34]
#removes first 34 rows from negWords that don't contain words

#per word

d <- as.matrix(detract_dfm)
#converts out detractor document feature matrix into a matrix

p <- as.matrix(promote_dfm)
#converts out promoter document feature matrix into a matrix

dPerWord <- colSums(d)
#this sums the columns of our new matrix d so we can see how many times each word occurs
pPerWord <- colSums(p)
#this sums the columns of our new matrix p so we can see how many times each word occurs


#matches

matchedPdetract <- match(names(dPerWord), posWords, nomatch = 0)
#this matches words from our dPerWord and posWords, if there is no match there will be a 0
dPerWord[matchedPdetract != 0]
#this shows the words that had a match between dPerWord and posWords by showing all the non-zeros in matchedPdetract within dPerWord
sum(matchedPdetract != 0)
#shows the amount words matched between dPerWord and posWords

matchedNdetract <- match(names(dPerWord), negWords, nomatch = 0)
#this matches words from our dPerWord and negWords, if there is no match there will be a 0
dPerWord[matchedNdetract != 0]
#this shows the words that had a match between dPerWord and negWords by showing all the non-zeros in matchedNdetract within dPerWord
sum(matchedNdetract != 0)
#shows the amount words matched between dPerWord and negWords
#As you can see there are a lot more negative words (87) than positive words (47) in the reviews from the detractors. 
#The most common positive words were like, great, well, good, work, and friendly. The most common negative words were worst, 
#rude, terrible, lost, delayed, bad, issues, and poor.


matchedPpromo <- match(names(pPerWord), posWords, nomatch = 0)
#this matches words from our pPerWord and posWords, if there is no match there will be a 0
pPerWord[matchedPpromo != 0]
#this shows the words that had a match between pPerWord and posWords by showing all the non-zeros in matchedPpromo within pPerWord
sum(matchedPpromo != 0)
#shows the amount words matched between pPerWord and posWords


matchedNpromo <- match(names(pPerWord), negWords, nomatch = 0)
#this matches words from our pPerWord and negWords, if there is no match there will be a 0
pPerWord[matchedNpromo != 0]
#this shows the words that had a match between pPerWord and negWords by showing all the non-zeros in matchedNpromo within pPerWord
sum(matchedNpromo != 0)
#shows the amount words matched between pPerWord and negWords
#Above, you can see that promoters had a lot more positive words (76) than negative words (26) in their reviews. Some of the most 
#common positive words were comfortable, good, best, pretty, nice, excellent, well, efficient, friendly, and great. Some of the most 
#common negative words were uneventful, delay, boring, and bad.


#8

airCat <- data.frame(Gender=as.factor(airData$gender),
                     type=as.factor(airData$type_of_travel),
                     old=as.factor(airData$age>median(airData$age)),
                     delay=as.factor(airData$departure_delay_in_minutes>15),
                     detractor=as.factor(airData$likelihood_to_recommend<7))
#this creates a data frame from columns in airData and turns them into factors. It also creates three new columns, such as delay
#which is defined as a delay in the flight departure by more than 15  minutes, and also a detractor column which is defined by
#a passanger giving a rating of less than 7 on the likelihood of recommending this airline. There's also an old column which is defined
#by if the age of the passenger is greater than the median of the ages of the passengers in our dataset.

library(arules)
library(arulesViz)

airCatX <- as(airCat,"transactions")
#this turns our airCat data frame into transactions so we can inspect it.

inspect(airCatX)
itemFrequency(airCatX)
itemFrequencyPlot(airCatX)

ruleset <- apriori(airCatX, parameter=list(support=0.005, confidence=0.5), appearance = list(rhs = "detractor=TRUE"))
#this creates a rule set where the support has to be above .005 and the confidence has to be above 0.5, that looks at the properties
#of being a detractor.
inspect(ruleset)
#this allows you to inspect the results of our ruleset.
inspectDT(ruleset)
#The two observations I looked at were lines 18 and 20. This is because their confidence was 100% which means that everyone in the data 
#who had those properties was a detractor. Line 18 shows that if the passenger was male, traveled for vacation, and their flight was delayed
#they were a detractor. Line 20 shows that if you traveled for a vacation, were not old, and there was a delay then you were also a detractor. 
#Both of these rules (lines) have a lift of around 2.19 which is greater than 1 which means that these two occurrences are dependent on one 
#another. The support of line 18 is .06 which means the combination in line 18 appears in 6% of the airline data. The support 
#of line 20 was .04 which means that the combination in line 20 appears in around 4% of the airline data.


#9

recommend_reg <- lm(likelihood_to_recommend ~ age + gender + type_of_travel + flight_distance + departure_delay_in_minutes, data = airData)
#this creates a regression with the dependent variable being likelihood to recommend, and the independent variables being age, gender, type of travel,
#flight distance, and departure delays.
summary(recommend_reg)
#this allows you to see the summary and output of the above regression.
#the r-squared is around .47 which isn't great but not terrible, and there is only one significant predictor which is personal travel, which
#is significant at the 1st percentile.
#There is only one significant predictor when looking at what predicts the likelihood of recommending this airline, which is people who 
#traveled for a vacation. This output says that if you traveled for a vacation you are expected to have the worst rating out of the three 
#types of travel. In fact you are expected to have a 3.68 less of a rating compared to someone that traveled for business.



#10
#Overall I think that this is a pretty good airline. I feel as if there are definitely things that could be improved on, however. 
#When looking at the detractors, their biggest complaints according to the word cloud and sentiment analysis was the service and 
#luggage situation. I would recommend to train the employees at the airport to maximize their friendliness and helpfulness so they 
#can help passengers with their luggage and show them where baggage claim is. I say this because the reason why the promoters gave 
#such a good rating was because of the service according to their word cloud. I think if this airline focuses a lot more on how 
#their employees treat their customers and improve their service then there will be even more promoters in the future.

