#Question 1
#load in neccessary packages
library(dplyr)
library(readr)
airport_pairs <- read_csv("plan372-sp23/HW 4/airport_pairs.csv")

#create table that that filters out from the dataset and shows origin and dest as RDU. It filters to more than 10,000 passenger)
rdu_flights <- airport_pairs %>%
  filter(origin == "RDU" | dest == "RDU") %>%
  filter(passengers >= 10000) %>%
  select(-c(distancemiles, origin_name, origin_cbsa, origin_cbsa_name, dest_name, dest_cbsa, dest_cbsa_name)) %>% #it shows all columns except these
  arrange(desc(passengers)) #it arranged them in highest to lowest passengers

rdu_flights #the table name


#Question 2
#Load in the necessary libraries
library(tidycensus)
library(sf)
library(tidyverse)
airport_pairs <- read_csv("plan372-sp23/HW 4/airport_pairs.csv")
# This gets the total population for each CBSA
cbsa_vars <- c('total_pop' = "B01003_001",'median_income' = "B19013_001", "GEOID")
cbsa_data <- get_acs(geography = "cbsa", variables = cbsa_vars, output = 'wide', year = 2019)
cbsa_data <- cbsa_data[-4]
cbsa_data <- cbsa_data[-5]

# Create copies of cbsa_data
origin_cbsa_data <- cbsa_data %>% 
  rename(origin_pop = total_popE, origin_income = median_incomeE)

dest_cbsa_data <- cbsa_data %>% 
  rename(dest_pop = total_popE, dest_income = median_incomeE)
#Convert GEOID column to integer
origin_cbsa_data$GEOID <- as.integer(origin_cbsa_data$GEOID)
dest_cbsa_data$GEOID <- as.integer(dest_cbsa_data$GEOID)


# Join origin CBSA data to cbsa_volumes
cbsa_volumes <- airport_pairs %>%
  left_join(origin_cbsa_data, by = c("origin_cbsa" = "GEOID"))

# Join destination CBSA data to cbsa_volumes
cbsa_volumes <- cbsa_volumes %>%
  left_join(dest_cbsa_data, by = c("dest_cbsa" = "GEOID"))
#The operation uses the summarise() function to calculate summary statistics for each group. Specifically, it calculates the mean of the "origin_pop", "dest_pop", "origin_income", and "dest_income" columns, rounds the mean of the "distancemiles" column to the nearest integer using the round() function, and calculates the sum of the "passengers" column. The results of these operations are stored in a new dataset called "cbsa_volumes". The columns in this new dataset is renamed to reflect the summary statistics calculated in the summarise() function.
cbsa_volumes <- cbsa_volumes %>%
  group_by(origin_cbsa, dest_cbsa) %>%
  summarise(origin_pop = mean(origin_pop),
            dest_pop = mean(dest_pop),
            origin_income = mean(origin_income),
            dest_income = mean(dest_income),
            distancemiles = round(mean(distancemiles),digit=0),
            total_passengers = sum(passengers))

# Scatterplot of origin population and total passengers
ggplot(cbsa_volumes, aes(x = origin_pop, y = total_passengers)) +
  geom_point() +
  scale_y_continuous(labels = scales::label_number()) + #The geom_point() function creates a scatterplot, and the scale_y_continuous() and scale_x_continuous() functions set the labels on the x and y axes to have a comma separator for thousands.
  scale_x_continuous(labels = scales::label_number())

#scatterplot of destinitation population and total passengers
ggplot(cbsa_volumes, aes(x = dest_pop, y = total_passengers)) +
  geom_point() +
  scale_y_continuous(labels = scales::label_number()) +
  scale_x_continuous(labels = scales::label_number())

# Scatterplot of flight distance and total passengers
ggplot(cbsa_volumes, aes(x = distancemiles, y = total_passengers)) +
  geom_point() +
  scale_y_continuous(labels = scales::label_number()) +
  scale_x_continuous(labels = scales::label_number())


#EXTRA CREDIT 
#this creates a scatterplot for the origin income and total passengers
ggplot(cbsa_volumes, aes(x = origin_income, y = total_passengers)) +
  geom_point() +
  scale_y_continuous(labels = scales::label_number()) +
  scale_x_continuous(labels = scales::label_dollar())
#this creates a scatterplot for the destination income and total passenger
ggplot(cbsa_volumes, aes(x = dest_income, y = total_passengers)) +
  geom_point() +
  scale_y_continuous(labels = scales::label_number()) +
  scale_x_continuous(labels = scales::label_dollar())




#Question 3
# Linear regression
model <- lm(total_passengers ~ origin_pop + dest_pop + distancemiles + origin_income + dest_income, data = cbsa_volumes)
summary(model)

#Question 4
# Create a table for RDU, PDX, ELP, TLH, and SAN with origin, destination, origin_cbsa, dest_cbsa, and distance miles
destinations <- data.frame(
  origin = c("RDU","RDU","RDU","RDU","PDX", "ELP", "TLH", "SAN"),
  destination = c("PDX", "ELP", "TLH", "SAN","RDU","RDU","RDU","RDU"),
  origin_cbsa = c(39580,39580,39580,39580,38900,21340,45220,41740),
  dest_cbsa = c(38900,21340,45220,41740,39580,39580,39580,39580),
  distancemiles = c(2363,1606,496,2193,2363,1606,496,2193)
)


# Merge the two tables to get population data for each destination airport
# This code creates a new data frame called cbsa_volumess by merging two existing data frames destinations and origin_cbsa_data using a left join. The join is performed based on the columns "origin_cbsa" in the destinations data frame and "GEOID" in the origin_cbsa_data data frame. 
cbsa_volumess <- destinations %>%
  left_join(origin_cbsa_data, by = c("origin_cbsa" = "GEOID"))

# The data frame cbsa_volumess will have additional columns from dest_cbsa_data that match the dest_cbsa values in the original data frame. The rows in the original data frame that do not have matching values in dest_cbsa_data will have missing values in the new columns.
cbsa_volumess <- cbsa_volumess %>%
  left_join(dest_cbsa_data, by = c("dest_cbsa" = "GEOID"))


# Predict passengers using the regression model
cbsa_volumess$estimated_passengers <- predict(model, cbsa_volumess)