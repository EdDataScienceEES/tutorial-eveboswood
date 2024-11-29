# Name: Eve Boswood-Clark
# Date: 21.11.2024
# Tutorial pre-requisites: A basic knowledge of data wrangling and how to code Generalised Linear Mixed Models (GLMMs)

# You are going to learn how the location of effects in GLMMs change the output 
# You can change whether an effect is fixed or random and you can change whether it is multiplied by or added to the predictor
# These are all dependent on your research question, but this tutorial will explain how the different placements will change the outcome

# Libraries
library(dplyr)
library(lme4)
library(ggplot2)

# Download dataset - meat consumption through time
meat_data <- read.csv("/Users/eveboswood/Downloads/meat_consumption.csv")

# Have to do some data wrangling to make the dataset usable
# Each Country both has a measure of meat in thousand tonnes and kg per captia
# The Model won't work properly with both included, so we remove the thousand tonne measurements
meat_data <- meat_data %>%
  filter(MEASURE != "THND_TONNE")
# Filtering the data for 5 countries, because as it is the dataset is large
# You need at least five data sets to do random effects
# I chose Australia, USA, Norway, India and Ethiopia 
meat_data <- meat_data %>%
  filter(LOCATION %in% c("AUS", "USA", "NOR", "IND", "ETH"))


# Now we still start making and comparing the generalised linear models
# This histogram is to find the type of distribution that we are using for this model
#As the data is continuous, the data is distributed through gamma distribution

hist(meat_data$scaled_value)

# The simplest General Linear Model is a comparison between the Value and Time

mean_value <- mean(meat_data$Value, na.rm = TRUE)
var_value <- var(meat_data$Value, na.rm = TRUE)
cat("Mean abundance:", mean_value, "\nVariance:", var_value, "\n")
# Very over dispersed
mod_1 <- glm(scaled_value ~ scaled_time, data = meat_data, family = MASS::negative.binomial(theta = 1))
summary(mod_1)
plot(mod_1)
# This shows the basic relationship between the predictor (time) and the response variable (Value)
# Shows the effect of TIME on Value is very small (negative effect) and statistically significant to 0.05 (only to 0.01)
# This difference between null and residual deviance is small meaning TIME doesnt explain a large amount of Value
# However, this is not right because there are different countries and "SUBJECTS" which change the output
# We can account for each country having a different intercepts 
mod_2 <- glm(scaled_value ~ scaled_time + LOCATION, data = meat_data, family = MASS::negative.binomial(theta = 1))
summary(mod_2)
# Reduction in deviance shows the model explains more of the variability in Value 
meat_data$scaled_time <- meat_data$TIME - 1990 + 1
meat_data$scaled_value <- meat_data$Value / max(meat_data$Value)
# TIME and Value had to be scaled because their values were very large
# They were causing the error message : PIRLS loop resulted in NaN value
# They were both scaled without changing the relationship, and I kept the TIME and Value columns to allow interpretation
mod_3 <- glm(scaled_value ~ scaled_time * LOCATION, data = meat_data, family = MASS::negative.binomial(theta = 1))

mod_4 <- glmer(scaled_value ~ scaled_time  + (1|LOCATION), data = meat_data, family = MASS::negative.binomial(theta = 1))

mod_5 <- glmer(scaled_value ~ scaled_time * LOCATION + (1|SUBJECT), data = meat_data, family = MASS::negative.binomial(theta = 1))

mod_6 <- glmer(scaled_value ~ scaled_time * LOCATION + (LOCATION|SUBJECT), data = meat_data, family = poisson())



# Scatter plot with each country as a facet and color by SUBJECT
ggplot(meat_data, aes(x = scaled_time, y = scaled_value, color = SUBJECT)) +
  geom_point() +                            # Plot the data points
  facet_wrap(~ LOCATION, scales = "free") +  # Facet by LOCATION (country)
  theme_minimal() +                         # Clean theme
  labs(x = "Year (Scaled)",                 # X-axis label
       y = "Scaled Meat Consumption",       # Y-axis label
       color = "Subject")                  # Legend title














# Generate a sequence of scaled_time values
time_values <- seq(0.1, 30, length.out = 100)

# Define different gradients for each subject within each location (country)
gradients_matrix <- matrix(c(0.1, 0.2, 0.3, -0.05,  # AUS gradients for Beef, Poultry, Sheep, Pig
                             0.15, 0.25, 0.35, 0.45, # USA gradients
                             0.05, 0.15, 0.25, 0.3,  # NOR gradients
                             -0.1, 0.3, 0.4, 0.01,    # IND gradients
                             0.25, 0.05, 0.45, 0.55),# ETH gradients
                           ncol = 4, byrow = TRUE)

# Define different intercepts for each subject (and vary them across locations)
intercepts_matrix <- matrix(c(0.1, 1.9, 5.5, 6.15,  # AUS intercepts
                              1.2, -0.1, 3.9, 4.22,  # USA intercepts
                              5.45, 1.14, 4.22, -2.2,  # NOR intercepts
                              3.55, 2.28, 3.3, 3.3,  # IND intercepts
                              0.3, 4.32, 2.35, 0.33), # ETH intercepts
                            ncol = 4, byrow = TRUE)

# Locations (countries) and Subjects
locations <- c("AUS", "USA", "NOR", "IND", "ETH")
subjects <- c("Beef", "Poultry", "Sheep", "Pig")

# Create an empty data frame to store all the lines
line_data <- data.frame()

# Generate data for each location (country)
for(i in 1:length(locations)) {
  # For each country, generate lines with different intercepts and gradients for each subject
  country_data <- data.frame(
    scaled_time = rep(time_values, times = length(subjects)),
    LOCATION = rep(locations[i], each = length(time_values) * length(subjects)),
    SUBJECT = rep(subjects, each = length(time_values)),
    scaled_value = unlist(lapply(1:length(subjects), function(j) {
      # Use the different intercept and gradient for each subject within the same country
      intercept <- intercepts_matrix[i, j]
      gradient <- gradients_matrix[i, j]
      intercept + gradient * time_values  # Add intercept and gradient for each subject
    }))
  )
  
  # Add the country data to the main data frame
  line_data <- rbind(line_data, country_data)
}

# Scale the scaled_value to fit between 0 and 1 for each country
line_data$scaled_value <- (line_data$scaled_value - min(line_data$scaled_value)) / 
  (max(line_data$scaled_value) - min(line_data$scaled_value))

# Plot the lines
ggplot(line_data, aes(x = scaled_time, y = scaled_value, color = SUBJECT, linetype = SUBJECT)) +
  geom_line(size = 1) +
  facet_wrap(~LOCATION) +  # Create separate plots for each location (country)
  labs(
    title = "Lines with Different Intercepts and Gradients for Each Location and Subject",
    x = "Time Scaled (years)",
    y = "Meat Consumption Value (scaled)",
    color = "Subject",
    linetype = "Subject"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +  # Optional: Customize color palette
  ylim(0, 1)  # Set y-axis limits between 0 and 1




