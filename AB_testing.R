## AB TESTING
#Test for a new webpage by an online store

# Loading Libraries
library(RPostgreSQL)
library(DBI)
library(dbplyr)
library(tidyverse)

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
         landing_page = factor(landing_page))

### Data Summary
summary(ab_data)
summary(countries)

## Generate a frequency table for the landing page
table(ab_data$landing_page)

## Generate a frequency table for the group
table(ab_data$group)

# Exploratory Data Analysis



##### Work with timestamp, divide into am and pm, see if it's having an influence on the users conversion