# Title: The Influence of Effect Placement on Generalised Linear Model Outputs
# Name: Your Name
# Date:

# Libraries
library(dplyr)
library(lme4)
library(ggplot2)
# If you've never used a given package, install it with `install.packages("name")'

# Download dataset - meat consumption through time
meat_data <- read.csv("/Users/eveboswood/Downloads/meat_consumption.csv")
# Remember to change the directory to fit where you have saved the dataset

# Removing all values measured in thousand tonnes
# This will leave us with one measurement per country, per year, in kg per capita
meat_data <- meat_data %>%
  filter(MEASURE != "THND_TONNE")
# Filter the data to remove all countries apart from Australia, USA, Norway, India and Ethiopia
meat_data <- meat_data %>%
  filter(LOCATION %in% c("AUS", "USA", "NOR", "IND", "ETH"))
# Remove the years between 2020-2028 because this is predicted data
meat_data <- meat_data %>%
  filter(!(TIME >= 2020 & TIME <= 2028))
# Scaling the data to make it more compatible for linear modelling 
meat_data$scaled_time <- meat_data$TIME - 1990 + 1   #Starting from year 1 rather than 1990
meat_data$scaled_value <- meat_data$Value / max(meat_data$Value) #Scaling the data around 0.5
# Visulising the distribution using a histogram
hist(meat_data$Value)
# Very right-skewed, looks like it could be gamma or negative binomial
# Let's check for overdispersion...
mean_value <- mean(meat_data$Value, na.rm = TRUE)
var_value <- var(meat_data$Value, na.rm = TRUE)
cat("Mean abundance:", mean_value, "\nVariance:", var_value, "\n")
# Variance is greater than the mean abundance meaning that the data is overdispersed
# Negative-Binomial it is 
# Scatter plot with each country as a facet and color by SUBJECT to help visualise our dataset
ggplot(meat_data, aes(x = scaled_time, y = scaled_value, color = SUBJECT)) +
  geom_point() +                            # Plots the data points
  facet_wrap(~ LOCATION, scales = "free") +  # Facet by LOCATION (country)
  theme_minimal() +                         # Clean theme
  labs(x = "Year (Scaled)",                 # X-axis label
       y = "Scaled Meat Consumption",       # Y-axis label
       color = "Subject")                  # Legend title

# A General Linear Model comparing the effect of time on value (value of meat consumption
# This ignores all other factors that may affect mean consumption except for time
mod_1 <- glm(scaled_value ~ scaled_time, data = meat_data, family = MASS::negative.binomial(theta = 1))
# A General Linear Model comparing the effect of time on value while allowing for variation in the starting value by country
mod_2 <- glm(scaled_value ~ scaled_time + LOCATION, data = meat_data, family = MASS::negative.binomial(theta = 1))
# A General Linear Model comparing the effect of time on value whilst allowing for variation both in intercept and gradient, dependent on country
mod_3 <- glm(scaled_value ~ scaled_time * LOCATION, data = meat_data, family = MASS::negative.binomial(theta = 1))
# A General Linear Mixed Model with LOCATION as a random effect
# Allows for variation in the intercept of each country, but not in the gradient
mod_4 <- glmer(scaled_value ~ scaled_time  + (1|LOCATION), data = meat_data, family = MASS::negative.binomial(theta = 1))
# A General Linear Mixed Model, again showing variation in intercept and graient, dependent on country
# This model also allows for variation in the intercept of different types of meat within the data
mod_5 <- glmer(scaled_value ~ scaled_time * LOCATION + (1|SUBJECT), data = meat_data, family = MASS::negative.binomial(theta = 1))
# A General Linear Mixed Model that allows for differences in intercept and gradient for both the fixed and the random effect
mod_6 <- glmer(scaled_value ~ scaled_time * LOCATION + (LOCATION|SUBJECT), data = meat_data, family = MASS::negative.binomial(theta = 1))
# Our first model summary
summary(mod_1)
# Summary for Model 2
summary(mod_2)
# Summary for Model 3
summary(mod_3)
# Summary for Model 4
summary(mod_4)
# Summary for Model 5
summary(mod_5)
# Summary for Model 6
summary(mod_6)

