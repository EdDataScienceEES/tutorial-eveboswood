## The Influence of Effect Placement on Generalised Linear Model Outputs

### Tutorial aims
- Understand How the Placement of Effects and How They are Combined (* or +) Changes Your Model
- Decide When to Use Which Effect Placement and Whether to Multiply or Add
- Model Building and Performance Comparison through Result Interpretation

### Tutorial pre-requisites
- A basic knowledge of data wrangling
- How to code Generalised Linear Models and Generalised Linear Mixed Models including what distribution to use

### Introduction
Hello Linear Modellers! Welcome to this tutorial on how and why the placement of effects within your Generalised Linear (Mixed) Models influences the outputs of your models, what the outputs mean and in what scenario you should use each one. 
In this tutorial, we will use a dataset on meat consumption through time to examine how to account for variation in consumption across countries and subjects (eg, beef, poultry, etc.) within our model, and in what cases we would use each iteration of the model.
It is important to highlight that when modelling data, you should not try out different models and choose the one that best fits your data, as this leads to P-Hacking. However, in this tutorial, we are trying and testing the different models and comparing the outputs to demonstrate what each one means, and in what scenarios you would choose each one.

#### Why this dataset?
It is estimated that 30% of global greenhouse gas emissions are attributed to agriculture, with by far the largest proportion of that coming directly from raising and feeding livestock. Reducing our meat consumption is a vital, and feasible, component of reducing global warming, yet many countries meat consumption is still on the rise. This is why I chose to analyse this dataset, as I thought it would be interesting and meaningful to find the exact model outputs of how meat consumption in five major countries from different continents has changed since 1990, when controlling for the different types of meat.

### Some data set wrangling... (Boring but Necessary) 
#### Libraries and Datasets 
First we need to load the packages that we will be using in this tutorial. There are only two to load. Remember to install the packages if you have not done before on the version of R that you are using, if you try to add a package to the library that you haven't installed it will give you the error message: 
- "Error in library(dplyr) : there is no package called 'dplyr'

Make sure that you have already downloaded the dataset and know the directory of where it is saved. The data is available here or in the repository named "meat_consumption.csv"

```html
# Title: The Influence of Effect Placement on Generalised Linear Model Outputs
# Name: Your Name
# Date:

# Libraries
library(dplyr)
library(lme4)
# If you've never used a given package, install it with `install.packages("name")'

# Download dataset - meat consumption through time
meat_data <- read.csv("/Users/eveboswood/Downloads/meat_consumption.csv")
# Remember to change the directory to fit where you have saved the dataset
```
#### Let's make this data more Linear Model-friendly...
This dataset comes with some problems that would skew the results of our models. All datasets come with different levels of useability, and their own unique problems, it is just a matter of spotting them and fixing them before you begin modelling. A good place to start is with the units... are all of the variables measured and recorded in the same way? No...
- The website that the data is sourced from: https://www.kaggle.com/datasets/allenkong/worldwide-meat-consumption?resource=download gives us context about the data, and it says that there are two different metrics for meat consumption. There are measurements for every country in every year for both "thousand tonnes of carcass weight" and "kilograms of retail weight per capita". For the model to work effectively we cannot compare both of these units within the same model, so we have to remove one. The latter is more reflective as it is not skewed by population size and it gives us smaller numbers to work with (which models tend to prefer), so we will remove all values for carcass weight.

```html
# Removing all values measured in thousand tonnes
# This will leave us with one measurement per country, per year, in kg per capita
meat_data <- meat_data %>%
  filter(MEASURE != "THND_TONNE")
```

To use a parameter as a random effect, we need to have at least five distinct levels, so we will filter the data down to include only five countries. This is for ease as five countries are easier to work with than the current 39, but also makes sure it will work as a random effect. I chose one country from each continent to allow a global view of meat consumption.

```html
# Filter the data to remove all countries apart from Australia, USA, Norway, India and Ethiopia
meat_data <- meat_data %>%
  filter(LOCATION %in% c("AUS", "USA", "NOR", "IND", "ETH"))
```

Okay! So the final bit of data wrangling is again filtering, but this time years. This dataset contains data from 1990-2028, bit funny as four of those years haven't happened yet right? This is because the data for years between 2020-2028 are predictions based on the previous years. We are not interested in future predictions today, just in modelling what has already happened, so lets filter those years out!

```html
# Remove the years between 2020-2028 because this is predicted data
meat_data <- meat_data %>%
  filter(!(TIME >= 2020 & TIME <= 2028))
```
That's the data wrangled!! Well done!! Now onto the modelling, exciting stuff...

### What distribution is our data?
One more thing before we get onto the models themselves, we have to find out how our data is distributed. Let's make a histogram to allow us to visualise the distribution
```html
# Visulising the distribution using a histogram
hist(meat_data$Value)
# Very right-skewed, looks like it could be gamma or negative binomial
# Let's check for overdispersion...
mean_value <- mean(meat_data$Value, na.rm = TRUE)
var_value <- var(meat_data$Value, na.rm = TRUE)
cat("Mean abundance:", mean_value, "\nVariance:", var_value, "\n")
# Variance is greater than the mean abundance meaning that the data is overdispersed
# Negative-Binomial it is 
```

### The Influence of Effect Placement and Combination on the Meaning of Your Model
Okay! We made it through the coding admin, no onto the modelling! 
We are going to start with a very simple model and build complexity as we move through the tutorial. For each model there will be an explanation as to what it means, when it would be used and how to code it. We will later look at the outputs and plot the models residuals to gain an understanding as to how each model fits our data.
Some lingo before we start:
- The predictor: also known as the independent variable is the variable used to explain/predict the outcome of the response variable
- The response variable: also known as the dependent variable is the variable you are trying to model/predict.
- Fixed effect: A variable whose levels represent specific categories or values that affect the response variable in a way that we are interested in. 
- Random effect: A variable that accounts for variation within the data that is not explained by the fixed effect.

#### Model 1 - Simple General Linear Model
To begin with, we are just going to model the relationship between the predictor and the response variable alone. To do this we only need to use a Generalised Linear Model (not mixed) with no fixed or random effects. 
This models the effect of time on meat consumption, without considering that there are different types of meat and different countries.

```html
# A General Linear Model comparing the effect of TIME on Value (value of meat consumption
mod_1 <- glm(Value ~ TIME, data = meat_data, family = MASS::negative.binomial(theta = 1))
```










