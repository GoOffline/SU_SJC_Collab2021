
#Setting the Working Directory
setwd("//Users//alexandra//Desktop//Super Senior Year//BUAN 4310 01 Data Mining and Big Data")
#Load in EDA libraries
library(ggplot2) 
library(tidyverse)
## Reading and Loading in the Data and Removing Unnecessary Variables
mydata <- read.csv("patents_4.csv",header = TRUE) #Reads and loads in the data set

#To learn more about each of the variables, use the str(structure)function. The structure function puts each of the variables as a row,
#with its name, followed by its data type, and then followed by the first several values.
# character strings, are like factors except its common for them to take a unique value
# logi (aka logical variables), are a simple case of a categorical variable, where there are only two levels( TRUE/FALSE, YES/NO)
#int (integer) is discrete, num(numerical) is usually continous. Both are considered numerical variables. If there is an integer, but only a few values it can take, it behaves like a categorical variable
str(mydata)
ls(mydata)
mydata['years_elapsed'] = mydata['grantyear'] - mydata['applyear']
mydata

#List numerical data
#years_elapsed, backward_cities, forward_cities 

#Determine the categorical variables and convert them all into factors
#applnum, applyear, ee_city, ee_country, ee_ind_fname, ee_ind_lnname, ee_name, ee_number, ee_role, ee_role_desc, ee_state

mydata$application_number_factor <- as.factor(mydata$applnum)
mydata$application_year_factor <- as.factor(mydata$applyear)
mydata$assignee_city_factor <-as.factor(mydata$ee_city)
mydata$assignee_country_factor <-as.factor(mydata$ee_country)
mydata$assignee_individual_firstname_factor <-as.factor(mydata$ee_ind_fname)
mydata$assignee_individual_lastname_factor <- as.factor(mydata$ee_ind_lname)
mydata$assignee_company_name_factor <-as.factor(mydata$ee_name)
mydata$assignee_number_factor <-as.factor(mydata$ee_number)
mydata$assignee_type_factor <-as.factor(mydata$ee_role)
mydata$assignee_state_factor <-as.factor(mydata$ee_state)
mydata$grant_year_factor <-as.factor(mydata$grantyear)
mydata$patent_number_factor <- as.factor(mydata$patnum)
mydata$patent_type_factor <-as.factor(mydata$ptype)
mydata$company_nationality_factor <-as.factor(mydata$ee_role_desc)
mydata <- mydata%>%relocate(application_number_factor:company_nationality_factor,.before = patnum)
mydata <- subset(mydata, select = -c(X))
mydata <- mydata%>%relocate(years_elapsed,.after = company_nationality_factor)
mydata <- mydata%>% relocate(backward_cites, .after = years_elapsed)
mydata<- mydata%>% relocate(forward_cites,.after = backward_cites)
write.csv(mydata,'patentdatarevised.csv')

## First, let's deal with the qualitative/categorical variables


#Create a contingency table to look at the relationship between two categorical/qualitative variables
#This table looks at the relationship between number of each type of patent per US state
table(mydata$patent_type_factor, mydata$assignee_state_factor)

#Are the row and column variables independent from each other?
chisq.test(mydata$patent_type_factor,mydata$assignee_state_factor)
#p-value < 2.2e^-16. We were given a warning message saying that chi-squared approximation may be incorrect. We will use G-test to verify results
install.packages("DescTools")
library(DescTools)
GTest(mydata$patent_type_factor,mydata$assignee_state_factor) #p-value is the same
#Measure the association between the two variables
CramerV(mydata$patent_type_factor,mydata$assignee_state_factor) # A V value of .1 indicates that there is a weak correlation between the two variables.


##Second, lets deal with the numerical/quantiative variables. We will create a second data frame where we have only numerical columns to make running summary statistics easier..
#Create a seperate data frame for numerical data. Call the data frme numerical_df
library(tidyverse)
Numerical_df <- mydata %>% select(forward_cites, backward_cites, years_elapsed)
                       


#Preform summary statistics for numerical/quantitative variables
summary(Numerical_df)

#We will be using the package ggpubr to help us create visualizations. ggpubr works off off ggplot2

install.packages("ggpubr")
library(ggpubr)
library(ggplot2)

ggboxplot(mydata, y = "years_elapsed", width = 0.5)


gghistogram(Numerical_df, x = "years_elapsed", bins = 9,
            add = "mean")

ggecdf(Numerical_df, x = "years_elapsed")
# We want to see if the data is normally distributed so we will use QQ plots
ggqqplot(Numerical_df, x = "years_elapsed")

#Descriptive statistics by groups
library(dplyr)
group_by(mydata,assignee_state_factor) %>%
  summarise(
    count = n(),
    mean = mean(Numerical_df$years_elapsed, na.rm = TRUE),
    sd = sd(Numerical_df$years_elapsed, na.rm = TRUE)
  )
#Create graphics for grouped data:
library(ggpubr)
#Box plot colored by state
ggboxplot(mydata, x = "assignee_state_factor", y = "years_elapsed",
          color = "assignee_state_factor")
#Stripchart colored by groups: States
ggstripchart(mydata, x = "assignee_state_factor", y = "years_elapsed",
             color = "assignee_state_factor",
             add = "mean_std")



#group data by country to find how many patents have each country
mydata_by_country <- mydata %>%
  group_by(ee_country) %>%
  summarize(count = n())

head(mydata_by_country)

#order the data in decreasing order
mydata_by_countrynew <- mydata_by_country[order(mydata_by_country$count, decreasing = TRUE), ]

#select the top 10 countries with the highest number of patents
top10 <- mydata_by_countrynew [1:10,]

#graph the top10 countries 
top10new <- data.frame(countries=c('US', 'JP', 'KR', 'DE', "TW", "CN", "FR", "CH", "GB", "CA"),
                       number_of_patents=c(14109, 5428, 1895, 1462, 1102, 903, 693, 434, 404, 397))

ggplot(top10new, aes(x=countries, y=number_of_patents))+
  geom_bar(stat='identity')+
  ggtitle("The 10 Countries with highest number of Patents ") +
  xlab("Countires") + ylab("Number of Patents")

#explore years patents granted 
#group data by year 
mydata_by_year_granted <- mydata %>%
  group_by(grant_year_factor) %>%
  summarize(count = n())

#graph number of patents by year 
ggplot(mydata_by_year_granted, aes(x=grant_year_factor, y=count))+
  geom_bar(stat = 'identity')+
  ggtitle("Patents Granted by Year") +
  xlab("Year Granted") + ylab("Number of Patents")

#explore top companies 
#group data by country to find how many patents have each country
mydata_by_company <- mydata %>%
  group_by(assignee_company_name_factor) %>%
  summarize(count = n())

#order the data in decreasing order
mydata_by_company <- mydata_by_company[order(mydata_by_company$count, decreasing = TRUE), ]

#select the top 10 companies with the highest number of patents
top10co <- mydata_by_company [1:10,]

#graph the top10 companies and their patents  
ggplot(top10co, aes(x=count, y=assignee_company_name_factor))+
  geom_bar(stat='identity')+
  ggtitle("The 10 Companies with highest number of Patents ") +
  xlab("Number of Patents") + ylab("Companies")+ 
  theme(axis.text.y = element_text(angle = 45))

