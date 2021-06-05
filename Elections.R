#' ---
#' title: "Exploring election results with few questions"
#' author: "Jibin"
#' date: "3/06/2020"
#' ---

#Loading libraries

library(ggplot2)
library(tidyverse)
library(readr)
library(readxl)

#Reading and viewing all files from the working directory


county_facts <- read_csv("county_facts.csv")
View(county_facts)

county_facts_dictionary <- read_excel("county_facts_dictionary.xlsx")
View(county_facts_dictionary)

primary_results <- read_csv("primary_results.csv")
View(primary_results)


#Q.1) Does higher level of education have a subsequent effect on the voting or the number of votes received?

#To answer this question, we need a graph which would compare the percent of degree holders and the votes received by a party for the respective state. 


# Creating a new dataset which represents only the percentage of bachelor degree holders in all the states and counties. Using varibale EDU685213 present in the county_facts dataset.

(degree_holders <- county_facts %>% select(fips,EDU685213) %>% arrange(desc(EDU685213)))  

(degree_holders <- degree_holders %>% rename("Percentage_Degree_Holders"=EDU685213 ))


#Calculating the total votes received by a party in a state and county using fips which serves as a unique identifier for a state and county

(total_votes <- primary_results %>% group_by(fips,party) %>% summarise(votes_total=sum(votes)))

# Merging datasets: degree_holders and total_votes by fips

merged_dataset <- merge(x = degree_holders ,y= total_votes, by = "fips")


#Plotting a graph to compare total votes received and the percentage of population with degree holders.

merged_dataset %>% ggplot(aes(x=Percentage_Degree_Holders,y=votes_total,color=party)) + geom_smooth() + xlab(label = "Bachelor's Degree Holders %") + ylab("Total Votes") + theme_minimal() + scale_colour_manual(values = c('blue', 'red')) 



#Answer: It can be observed that as the percentage of population with a bachelors degree increases, the votes   received by the Democratic party increases. 

#A possible likely explanation could be that a person with a higher level of education would have views that aligns with the liberal views and propaganda of the Democratic Party.





#Q.2) Compare votes received by a candidate with respect to the hispanic population in that region?



#Gathering percentage of hispanics from the county_facts dataset and storing it in a new dataframe

(hispanic_ds <- county_facts %>% select(fips,RHI725214) %>% arrange(desc(RHI725214)))


#Calculating the total votes received by a candidate

(total_votes1 <- primary_results %>% group_by(fips,party,candidate) %>% summarise(votes_total=sum(votes)))



#Merging the hispanic_ds with the total_votes datasets.

merged_dataset1 <- merge(x = hispanic_ds,y= total_votes1, by = "fips")

#Filtering the dataset to have a look of regions where the hispanic community is dominant.

merged_dataset1 <- merged_dataset1 %>% filter(RHI725214 > 50) %>% rename("Pop_Hispanic"=RHI725214)

#Plotting a graph for total votes received versus total population of hispanic community.


merged_dataset1 %>% ggplot(aes(x=Pop_Hispanic,y=votes_total,color=party))  + geom_line() + geom_smooth() + facet_wrap(~candidate)   +scale_y_log10() + theme_minimal() + scale_colour_manual(values = c('blue', 'red'))  


#Answer: So from the graph its clearly evident that as the population of Hispanic community increases, Democrats receive higher votes.

#It can be interpreted from the graph that the Hispanic community do have a preference for the Democratic Party.



#Q.3) Does the female population have an inclination towards female candidates?

#To answer this question, we need a graph which would compare the percent of female population and the votes received by a candidate for the respective state. 


#Gathering percentage of females from the county_facts dataset and storing it in a new dataframe

(female_ds <- county_facts %>% select(fips,SEX255214) %>% arrange(desc(SEX255214)))

(female_ds <- female_ds %>% rename("Female_pop"=SEX255214))

#Calculating the total votes received by a candidate

(total_votes2 <- primary_results %>% group_by(fips,party,candidate) %>% summarise(votes_total=sum(votes)))

#Merging the total votes received by a candidate and the respective female population in that region

merged_dataset2 <- merge(x = female_ds,y= total_votes2, by = "fips")

#Facetting based on votes received by each candidate

merged_dataset2 %>% ggplot(aes(x=Female_pop,y=votes_total,color=party))  + geom_line() + facet_wrap(~candidate)   +scale_y_log10() + theme_minimal() + scale_colour_manual(values = c('blue', 'red'))  



#Answer: There is no evident pattern as male candidates have received similar number of votes as female candidates.

#The notion that female voters are more likely to vote for a female candidate was not accurate as per the results of the election. (In the above graph, Hilary Clinton and Carly Fiorina were the female candidates)

