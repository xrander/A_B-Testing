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

# To ensure code reproducibility, the following will be used.

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

### Unique Values
#### Unique values for ab_data columns, except date user_id and timestamp
lapply(ab_data[,3:5], unique) # unique values of group, landing page and converted
unique(countries$country) # unique values of country

## Missing Values
unique(is.na(ab_data))
unique(is.na(countries))
#### There are no missing values
