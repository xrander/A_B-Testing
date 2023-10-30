## AB TESTING
#Test for a new webpage by an ECommerce Website

# Loading Libraries
library(RPostgreSQL)
library(dbplyr)
library(tidyverse)
library(ggthemes)

# Load Database Driver
drv <- dbDriver("PostgreSQL") #PostgreSQL Driver loaded

# Inserting PostgreSQL Credentials
conn <- dbConnect(drv,
                 dbname = "postgres",
                 host = "localhost",
                 port = "5432",
                 password = "******",
                 user = "postgres")

#Query DATABASE for the two tables
ab_data <- dbGetQuery(conn, "SELECT * FROM ab_data")
countries <- dbGetQuery(conn, "SELECT * FROM countries")

# Convert object type to tibble
ab_data <- as_tibble(ab_data)
countries <- as_tibble(countries)
# Disconnect the database
dbDisconnect(conn)

#Preview the data
head(ab_data, n = 10)
head(countries, n = 10)

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

# The test took about 22 days

view(ab_data)

### Divide time stamp into am and pm to see if it will have an influence on conversion
ab_data <- ab_data %>%
  mutate(time_day = factor(ifelse(hour(timestamp) >= 12, "PM", "AM"), levels = c("AM", "PM"),
                       labels = c("AM", "PM")))

### Unique Values
#### Unique values for ab_data columns group, landing page, and converted
lapply(ab_data[,3:6], unique) # unique values of group, landing page and converted

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
sum(duplicated(ab_data)) # There are no duplicate observations

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
# This shows group that saw pages they shouldn't see, control group are not supposed to see the new page
# and treatment group should not see old page

# Exploratory Data Analysis
## Estimate the group of control group that are wrongly placed
wrong_control_group <- exp_data %>%
  filter(landing_page == "new_page" & group == "control")

wrong_treatment_group <- exp_data %>%
  filter(landing_page == "old_page" & group == "treatment")

#wrongly placed group and landing page
sum(count(wrong_control_group), count(wrong_treatment_group))

### Filter to get the table expression above showing only the groups that are oddly placed.

## Check the number of people that converted that saw the new page
new_page <- exp_data %>%
  filter(landing_page == "new_page") %>%
  group_by(group, factor(converted_meaning)) %>%
  summarize(num = n()) %>%
  rename(converted = `factor(converted_meaning)`) %>%
  mutate(probability = num/sum(num))

ggplot(new_page, aes(group, probability, fill = converted))+
  geom_bar(stat = "identity",
           position = "dodge")+
  labs(y = "probability",
       fill = "Convert",
       title = "Convertion Rate Per Group on the New Page")+
  geom_text(aes(label = round(probability,3)), vjust = 0.07, hjust = 0.02)+
  scale_fill_manual(values = c("turquoise2", "violetred2"),
                    label = c("Not Converted", "Converted"))+
  expand_limits(y = c(0.00, 1.00))+
  theme_bw()

## Visualizing, the national count data
country_count <- exp_data %>%
  group_by(country) %>%
  summarize(total = length(country))
country_count

ggplot(country_count, aes(country, total))+
  geom_bar(aes(fill = "salmon"),
           stat = "identity",
           show.legend = F)+
  labs(title = "Number of Customers According to Country")+
  theme_bw()+
  geom_text(aes(label = total, y = total+5000))


## Filter the wrong group out of the data
clean_ab_data <- exp_data %>%
  filter(!timestamp %in% wrong_control_group$timestamp & !timestamp %in% wrong_treatment_group$timestamp)

## Given the new data, calculate the number unique group values and landing_page values
table(clean_ab_data$landing_page, clean_ab_data$group)
