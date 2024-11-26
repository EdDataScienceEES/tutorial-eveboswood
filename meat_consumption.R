# Name: Eve Boswood-Clark
# Date: 21.11.2024
# Tutorial pre-requisites: A basic knowledge of data wrangling and how to code Generalised Linear Mixed Models (GLMMs)

# You are going to learn how the location of effects in GLMMs change the output 
# You can change whether an effect is fixed or random and you can change whether it is multiplied by or added to the predictor
# These are all dependent on your research question, but this tutorial will explain how the different placements will change the outcome

# Libraries
library(dplyr)
library(lme4)

# Download dataset - meat consumption through time
meat_data <- read.csv("/Users/eveboswood/Downloads/meat_consumption.csv")

# Have to do some data wrangling to make the dataset usable
# Each Country both has a measure of meat in thousand tonnes and kg per captia
# The Model won't work properly with both included, so we remove the thousand tonne measurements
meat_data <- meat_data %>%
  filter(MEASURE != "THND_TONNE")
# Filtering the data for 5 countries, because as it is the dataset is large
# You need at least five data sets to do random effects
# I chose Australia, USA, China, India and Ethiopia 
meat_data <- meat_data %>%
  filter(LOCATION %in% c("AUS", "USA", "CHN", "IND", "ETH"))
# Remove the years between 2020-2028 because this is predicted data
meat_data <- meat_data %>%
  filter(!(TIME >= 2020 & TIME <= 2028))

# Now we still start making and comparing the generalised linear models
# This histogram is to find the type of distribution that we are using for this model
#As the data is continuous, the data is distributed through gamma distribution
hist(meat_data$Value)
# The simplest General Linear Model is a comparison between the Value and Time

mean_value <- mean(meat_data$Value, na.rm = TRUE)
var_value <- var(meat_data$Value, na.rm = TRUE)
cat("Mean abundance:", mean_value, "\nVariance:", var_value, "\n")
# Very over dispersed
mod_1 <- glm(Value ~ TIME, data = meat_data, family = MASS::negative.binomial(theta = 1))
summary(mod_1)
# This shows the basic relationship between the predictor (time) and the response variable (Value)
# Shows the effect of TIME on Value is very small (negative effect) and statistically significant to 0.05 (only to 0.01)
# This difference between null and residual deviance is small meaning TIME doesnt explain a large amount of Value
# However, this is not right because there are different countries and "SUBJECTS" which change the output
# We can account for each country having a different intercepts 
mod_2 <- glm(Value ~ TIME + LOCATION, data = meat_data, family = MASS::negative.binomial(theta = 1))
summary(mod_2)
# Reduction in deviance shows the model explains more of the variability in Value 
meat_data$scaled_time <- meat_data$TIME - 1990 + 1
meat_data$scaled_value <- meat_data$Value / max(meat_data$Value)
# TIME and Value had to be scaled because their values were very large
# They were causing the error message : PIRLS loop resulted in NaN value
# They were both scaled without changing the relationship, and I kept the TIME and Value columns to allow interpretation
mod_3 <- glmer(scaled_value ~ scaled_time  + (1|LOCATION), data = meat_data, family = MASS::negative.binomial(theta = 1))
summary(mod_3)

mod_4 <- glmer(scaled_value ~ scaled_time + LOCATION + (1|SUBJECT), data = meat_data, family = MASS::negative.binomial(theta = 1))
summary(mod_4)

mod_5 <- glmer(scaled_value ~ scaled_time * LOCATION + (1|SUBJECT), data = meat_data, family = MASS::negative.binomial(theta = 1))
summary(mod_5)

mod_6 <- glmer(scaled_value ~ scaled_time * LOCATION + (LOCATION|SUBJECT), data = meat_data, family = MASS::negative.binomial(theta = 1))
summary(mod_6)

mod_7 <- glmer(scaled_value ~ scaled_time + LOCATION + (LOCATION|SUBJECT), data = meat_data, family = MASS::negative.binomial(theta = 1))
summary(mod_7)


library(ggplot2)

# Scatter plot with each country as a facet and color by SUBJECT
ggplot(meat_data, aes(x = scaled_time, y = scaled_value, color = SUBJECT)) +
  geom_point() +                            # Plot the data points
  facet_wrap(~ LOCATION, scales = "free") +  # Facet by LOCATION (country)
  theme_minimal() +                         # Clean theme
  labs(title = "Meat Consumption over Time", # Title of the plot
       x = "Year (Scaled)",                 # X-axis label
       y = "Scaled Meat Consumption",       # Y-axis label
       color = "Subject")                  # Legend title


