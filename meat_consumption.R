# Name: Eve Boswood-Clark
# Date: 21.11.2024
# Tutorial pre-requisites: A basic knowledge of data wrangling and how to code Generalised Linear Mixed Models (GLMMs)

# You are going to learn how the location of effects in GLMMs change the output 
# You can change whether an effect is fixed or random and you can change whether it is multiplied by or added to the predictor
# These are all dependent on your research question, but this tutorial will explain how the different placements will change the outcome

# Libraries
library(dplyr)

# Download dataset - meat consumption through time
meat_data <- read.csv("/Users/eveboswood/Downloads/meat_consumption.csv")

# Have to do some data wrangling to make the dataset usable
# The Poultry was measured in retail weight so we have to convert it back to carcass weight to make it comparable to the other data
meat_data <- meat_data %>%
  mutate(Value = if_else(SUBJECT == "POULTRY", Value * 1.12, Value))
# Filtering the data for 5 countries, because as it is the dataset is large
# You need at least five data sets to do random effects
# I chose the UK, USA, China, India and Ethiopia 
meat_data <- meat_data %>%
  filter(LOCATION %in% c("GBR", "USA", "CHN", "IND", "ETH"))
# Remove the years between 2020-2028 because this is predicted data
meat_data <- meat_data %>%
  filter(!(TIME >= 2020 & TIME <= 2028))

# Now we still start making and comparing the generalised linear models
# This histogram is to find the type of distribution that we are using for this model
hist(meat_data$Value)
# The simplest General Linear Model is a comparison between the Value and Time
mod_1 <- glm(Value ~ TIME, data = meat_data, family = Gamma())
summary(mod_1)
# This shows the basic relationship between the predictor (time) and the response variable (Value)
# However, this is not right because there are different countries and "SUBJECTS" which change the output
# We can account for each country having a different intercepts 
mod_2 <- glmer(Value ~ TIME + LOCATION, data = meat_data, family = Gamma())



