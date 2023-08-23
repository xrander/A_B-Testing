## AB TESTING
#Test for a new webpage by an ECommerce Website

# Loading Libraries
library(RPostgreSQL)
library(DBI)
library(dbplyr)
library(tidyverse)
library(ggthemes)

# Load Database Driver
# To practice connecting with a database, the following code was used
#drv <- dbDriver("PostgreSQL") #PostgreSQL Driver loaded

# Inserting PostgreSQL Credentials
#conn <- dbConnect(drv,
 #                dbname = "postgres",
  #               host = "localhost",
   #              port = "5432",
    #             password = "********",
     #            user = "postgres")

# Query DATABASE for the two tables
#ab_data <- dbGetQuery(conn, "SELECT * FROM ab_data")
#countries <- dbGetQuery(conn, "SELECT * FROM countries")

# Convert object type to tibble
#ab_data <- as_tibble(ab_data)
#countries <- as_tibble(countries)
## Disconnect the database
#dbDisconnect(conn)

# To ensure code reproducibility, the following will be used.

# Import the data
ab_data <- read_csv("https://raw.githubusercontent.com/xrander/A_B-Testing/master/ab_data.csv")
countries <- read_csv("https://raw.githubusercontent.com/xrander/A_B-Testing/master/countries.csv")

#Preview the data
head(ab_data)
head(countries)

# Data Structure and Validation
## Data Dimensions
dim(ab_data) # dimension of the ab_data: 294478 observations and 5 columns

dim(countries) # dimension of countries data: 290584 observations and 2 columns
### The data are not equal in length

## Data Structure
str(ab_data)

str(countries)

## Get the Mininum and Maximum timestamp
min(ab_data$timestamp)
max(ab_data$timestamp)

## Calculate the number of days the test lasted.
max(ab_data$timestamp) - min(ab_data$timestamp)

### Divide time stamp into am and pm to see if it will have an influence on conversion
ab_data <- ab_data %>%
  mutate(time = factor(ifelse(hour(timestamp) >= 12, "PM", "AM"), levels = c("AM", "PM"),
                       labels = c("AM", "PM")))

### Unique Values
#### Unique values for ab_data columns, except date user_id and timestamp
lapply(ab_data[,3:5], unique) # unique values of group, landing page and converted

# Generate Duplicate user id
dup_user_id <- ab_data %>%
  group_by(user_id) %>%
  count() %>%
  arrange(desc(n)) %>%
  filter(n > 1)

## Investigate to see if just the user_id is appearing twice or the observations are entered twice
ab_data %>%
  filter(user_id %in% dup_user_id$user_id) %>%
  arrange(user_id) #There are no duplicate values from the output

## Alternatively
unique(duplicated(ab_data)) # There are no duplicate observations

unique(countries$country) # unique values of country

## Missing Values
unique(is.na(ab_data))
unique(is.na(countries))
#### There are no missing values

# Data Preprocessing
## Coerce character to factor data type
countries <- countries %>%
  mutate(country = factor(country))

ab_data <- ab_data %>%
  mutate(group = factor(group),
         landing_page = factor(landing_page),
         converted_meaning = factor(converted,
                                    levels = c(0,1),
                                    labels = c("Not Converted", "Converted")))

# Join ab_data and countries data frame
exp_data <- ab_data %>%
  left_join(countries, by = "user_id")

### Data Summary
summary(ab_data)
summary(countries)
summary(exp_data)

## Generate a frequency table for the landing page
table(exp_data$landing_page)

## Generate a frequency table for the group
table(exp_data$group)

## Generate a frequency table for country of users
table(exp_data$country)

## Generate a frequency table for the landing page according to the group of user
table(exp_data$group, exp_data$landing_page)
# This shows group that saw pages they shouldn't see

# Exploratory Data Analysis
exp_data %>%
  filter()

### Filter to get the table expression above showing only the groups that are oddly placed.

## Check the number of people that converted that saw the new page
new_page <- exp_data %>%
  filter(landing_page == "new_page") %>%
  group_by(group, factor(converted_meaning)) %>%
  summarize(num = n()) %>%
  rename(converted = `factor(converted_meaning)`) %>%
  mutate(proportion = num/sum(num))

ggplot(new_page, aes(group, proportion, fill = converted))+
  geom_bar(stat = "identity",
           position = "dodge")+
  labs(y = "Convertion rate",
       fill = "Convert",
       title = "Convertion Rate Per Group on the New Page")+
  geom_text(aes(label = round(num,3)), vjust = "outward", hjust = "outward")+
  scale_fill_manual(values = c("turquoise2", "violetred2"),
                    label = c("Not Converted", "Converted"))+
  theme_bw()
# Control group are not supposed to see the new page, 

ggplot(exp_data, aes(country))+
  geom_bar(aes(fill = "darksalmon"), show.legend = F)+
  labs(title = "Number of Customers According to Country")+
  theme_bw()


