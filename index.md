## The Influence of Effect Placement on Generalised Linear Model Outputs

### Tutorial aims
- Understand How the Placement of Effects and How They are Combined (* or +) Changes Your Model
- Decide When to Use Which Effect Placement and Whether to Multiply or Add
- Model Building and Performance Comparison through Result Interpretation

### Tutorial pre-requisites
- A basic knowledge of data wrangling
- A basic understanding of how to code Generalised Linear Models and Generalised Linear Mixed Models and how to interpret their outputs

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
library(ggplot2)
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

More filtering, but this time years. This dataset contains data from 1990-2028, bit funny as four of those years haven't happened yet right? This is because the data for years between 2020-2028 are predictions based on the previous years. We are not interested in future predictions today, just in modelling what has already happened, so lets filter those years out!

```html
# Remove the years between 2020-2028 because this is predicted data
meat_data <- meat_data %>%
  filter(!(TIME >= 2020 & TIME <= 2028))
```
Okay! So the final bit of data wrangling is scaling our time and value column. This makes them more compatible with our models, as Linear models perform better when predictors are on a similar scale and are smaller numbers. We are keeping the TIME and VALUE columns in our dataset to make interpretation easier, but will be using scaled_time and scaled_value for our models. 

```html
# Scaling the data to make it more compatible for linear modelling 
meat_data$scaled_time <- meat_data$TIME - 1990 + 1   #Starting from year 1 rather than 1990
meat_data$scaled_value <- meat_data$Value / max(meat_data$Value) #Scaling the data around 0.5
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
Okay! We made it through the coding admin, now onto the modelling! 
We are going to start with a very simple model and build complexity as we move through the tutorial. For each model there will be an explanation as to what it means, when it would be used and how to code it. We will later look at the outputs to gain an understanding as to how each model fits our data.
Here is a simple scatter plot for meat consumption over time in each of our countries, colour-coded to the "subject". This visualises our data distribution and will help give a general understanding of the data for when making our models.
```html
# Scatter plot with each country as a facet and color by SUBJECT to help visualise our dataset
ggplot(meat_data, aes(x = scaled_time, y = scaled_value, color = SUBJECT)) +
  geom_point() +                            # Plots the data points
  facet_wrap(~ LOCATION, scales = "free") +  # Facet by LOCATION (country)
  theme_minimal() +                         # Clean theme
  labs(x = "Year (Scaled)",                 # X-axis label
       y = "Scaled Meat Consumption",       # Y-axis label
       color = "Subject")                  # Legend title
```


Some lingo before we start:
- The predictor: also known as the independent variable is the variable used to explain/predict the outcome of the response variable
- The response variable: also known as the dependent variable is the variable you are trying to model/predict.
- Fixed effect: A variable whose levels represent specific categories or values that affect the response variable in a way that we are interested in. 
- Random effect: A variable that accounts for variation within the data that is not explained by the fixed effect.

#### Model 1 - Simple General Linear Model
To begin with, we are just going to model the relationship between the predictor and the response variable alone. To do this we only need to use a Generalised Linear Model (not mixed) with no fixed or random effects. 
This models the effect of time on meat consumption, without considering that there are different types of meat and different countries.

```html
# A General Linear Model comparing the effect of time on value (value of meat consumption
# This ignores all other factors that may affect mean consumption except for time
mod_1 <- glm(scaled_value ~ scaled_time, data = meat_data, family = MASS::negative.binomial(theta = 1))
```
This models that there is a single line with one intercept and one gradient that averages the value for meat consumption over time for all countries and all types of meat (as depicted in Figure 1). It assumes that the relationship between the value and time is the same across the board. But we know that this isn't right because we have seen in our scatter plot (above) that meat consumption varies between countries, and between subjects. In all of our models the predictor value 
You would use mod_1 if you had a simple relationship between the response variable and one predictor without accounting for any additional factors or grouping factors. This is a rare scenario and is not often seen in ecology because relationships in nature tend to be complex.

![Rplot_Mod_1](https://github.com/user-attachments/assets/2292a79f-cccc-459b-96e2-670570f11c9c)

Figure 1 - An example of what data plotted using the simplicity of Model 1 could look like on a log-log scale. The data used is not actual data from the dataset, this is just fictional to demonstrate what the model is assuming.

#### Model 2 - General Linear Model with two predictors
This model is slightly more advanced as it contains two predictor values, which each have their own individual effect on the response variable. It is still a General Linear Model and contains no mixed effects, but it does model the countries as fixed effects. This model accounts for time trends while recognises that meat consumption may vary by country.

```html
# A General Linear Model comparing the effect of time on value while allowing for variation in the starting value by country
mod_2 <- glm(scaled_value ~ scaled_time + LOCATION, data = meat_data, family = MASS::negative.binomial(theta = 1))
``` 
This model uses '+ LOCATION' to add a fixed effect to our model. Because location is added rather than multiplied it allows each coutry to have a different 'intercept', however it is saying that each countries line will have the same gradient, depicted in Figure 2. You would use this model if you believe that your fixed effects will all have different starting points (intercepts) but have the same trend in relation to the predictor.

![Rplot_mod_2](https://github.com/user-attachments/assets/21f038aa-8c87-4d4f-a15d-d503c8f15430)


Figure 2 - An example of what data plotted using the assumptions of Model 2 could look like on a log-log scale. Each countries data has different intercepts, however they all have the same rate of change of meat consumption over time (gradient). The data used is not actual data from the dataset, this is just fictional to demonstrate what the model is assuming.

#### Model 3 - General Linear Model with a multiplied fixed effect
In model 2 the fixed effect 'LOCATION' was added to our model. In model three the same fixed effect is present, but this time it is multiplied. The interaction term 'scaled_time * LOCATION' allows the model to estimate different slopes (gradients) for each country. This means that the effect of time on meat consumption can vary per country, so they can have different intercepts, and different gradients.

```html
# A General Linear Model comparing the effect of time on value whilst allowing for variation both in model intercept and gradient, dependent on country
mod_3 <- glm(scaled_value ~ scaled_time * LOCATION, data = meat_data, family = MASS::negative.binomial(theta = 1))
```
You would use a model with this format, containing a multiplicative interaction term, when you expect the effect of one predictor (in this case time) on the response variable (in this case meat consumption) to vary depending on the levels of another predictor (in this case country). Because location is a fixed effect (not random - we will learn more about these) the output of the model will tell us different values for each country, for both intercept and slope. We are telling the model that country affects meat consumption over time, and we want to know how it affects it, depending on the country.

![Rplot_mod_3](https://github.com/user-attachments/assets/27db3c08-c5d7-4a55-89fe-402f17fb3bf5)

Figure 3 - An example of what data plotted using the assumptions of Model 3 could look like on a log-log scale. The data had different intercepts and different rates of change over time depending on country. The data used is not actual data from the dataset, this is just fictional to demonstrate what the model is assuming.

#### Model 4 - General Linear Mixed Model 
Hurray! Onto our first Mixed Model! In mixed models, we get to include random effects. Random effects are used when you want to account for variability across groups or units, but you do not want to know in what way each group varies. This tells the model that there will be changes in the data between the groups specified within the random effect, and the model will account for this, but it will not tell you the variation within the output. A random effect, written as (1 | LOCATION) will allow variation in intercepts for each country's meat consumption (baseline consumption in 1990) but does not allow for varying gradients. This is similar to a fixed effect that is added rather than multiplied, except the output is different.

```html
# A General Linear Mixed Model with LOCATION as a random effect
# Allows for variation in the intercept of each country, but not in the gradient
mod_4 <- glmer(scaled_value ~ scaled_time  + (1|LOCATION), data = meat_data, family = MASS::negative.binomial(theta = 1))
```
You would use random effects in this way when you know that the effect changes the intercept, but you do not want to know HOW it changes the intercept. This just lets the model account for it, making the outputs more reliable, but it won't tell you each individual intercept for each country, like it would when LOCATION was a fixed effect (model 2)

#### Model 5 - General Linear Mixed Model with fixed and random effects
Still reading..? Well done! We're onto our penultimate model, so things are getting pretty accurate! This time we are going to have a fixed and a random effect within the same model. We are keeping our fixed effect as it was in model 3, with a multiplicative interaction term (* LOCATION), but this time we are getting involved with the different types of meat, or 'SUBJECTS'. The random effect will be treated the same as in model 4, but will be SUBJECT rather than LOCATION. This means we can work out what each bit means, as we already have learnt what both of these interactions do. Multiplying the fixed effect into the model is saying that the meat consumption will vary within each country, both in the intercept and the gradient, and the output will tell us how it varies. And adding the random effect in the (1 | Effect) way means that the random effect will change the intercept of the lines for each SUBJECT, but we don't what to know how this will change. And all of the random effect's groups (the different types of meat) will vary in the same way over time (the same gradient).

```html
# 
mod_5 <- glmer(scaled_value ~ scaled_time * LOCATION + (1|SUBJECT), data = meat_data, family = MASS::negative.binomial(theta = 1))
```
This model is saying: 'how does meat consumption change over time and how does this relationship differ across countries, while accounting for differences between meat types'. It should be used if you want to know how one effect (country) changes the output, you don't want to know how the other effect (meat type) does, but you want to tell the model that there will be variation due to meat type.

![Rplot_mod_5](https://github.com/user-attachments/assets/0e783031-0682-48f6-a631-9e5326a4a00a)

Figure 4 - An example of what data plotted using the data produced by Model 5 could look like on a log-log scale. The data is split into the countries, with each country having a different gradient on meat consumption change over time. There are different intercepts for each different Subject, but all of the subjects have the same rate of change over time (gradient). The data used is not actual data from the dataset, this is just fictional to demonstrate what the model is assuming.

#### Model 6 - Our Final General Linear Mixed Model
Hurray! You've made it to the last model! This time we are keeping our fixed effect exactly the same, but changing our random effect. The random effect is still going to be SUBJECT (type of meat), but, unlike model 5, we are going to allow for changes in the rate of change for each meat type, rather than assuming that they all follow the same gradient. To do this, we change the exchange the 1 that was inside the brackets with an effect of our choice. Here we are going to use LOCATION. This is then saying that each combination of meat type and county has its own baseline level of meat consumption (intercept) whilst also allowing for differences in how value changes over time between different combinations of LOCATION and SUBJECT.

```html
# A General Linear Mixed Model that allows for differences in intercept and gradient for both the fixed and the random effect
mod_6 <- glmer(scaled_value ~ scaled_time * LOCATION + (LOCATION|SUBJECT), data = meat_data, family = MASS::negative.binomial(theta = 1))
```
Remember that the model output still won't contain information about the random effect, we are just telling the model the the type of meat is going to change the rate of consumption over time, within each country. If we wanted to know HOW the different types of meat vary, we would take them out of the brakets and make SUBJECT a fixed effect.

![Rplot_mod_6](https://github.com/user-attachments/assets/ef6e4851-1883-4f97-8271-a14d08b40996)


Figure 5 - An example of what data plotted using the data produced by Model 6 could look like on a log-log scale. The data is again split into the countries, but this time the subjects within each country each have different intercepts AND different gradients. So each type of meat in each country can have a unique rate of change of consumption over time (gradient), and a unique starting point (intercept). The data used is not actual data from the dataset, this is just fictional to demonstrate what the model is assuming.

Well done! Do you think you get it? Now let's look at how the differences in these models change the outputs and the compatibility of each one to our specific dataset.


### Model Outputs 

Now we know what each different model means and in what scenario we would use them, we are going to have a look at the outputs and see how the differences in the modelling change the model's fit to the data. This is just an activity to help us understand what the different outputs mean which will help you gain a deeper understanding of when each model should be used, and what they are telling you. However, trying out each model and checking each output is NOT what you should do with your data. The model that you choose to use should be completely dependent on your data and research question, and you should not go around fitting many models and seeing which works best. With that said, let's do exactly that...

There are many ways that we can look at how each of our models fits the dataset. Today we are going to look at the most common and straightforward one, the model output itself. 

#### Model Outputs - Model 1

To code to get the model output of a Model is easy, to interpret it is less simple. Because we are more focused on what each model means and when we would use it, we are just going to look at a simple overview of each output. You should already have some understanding of how to interpret model outputs, so this is just a general overview to allow us to compare what each model is doing. All gradients and intercepts that are produced by these summaries are on a log scale. We will start with our simplest model (mod_1) and work our way up. To get our model output we use "summary(name_of_your_model)"

```html
# Our first model summary
summary(mod_1)
```
This model output gives the Intercept and Gradient (labelled scaled_time) of one line on a log scale. This makes sense from what we learnt about Model 1, demonstrated in Figure 1, with one straight line which has one intercept and one gradient. Both the intercept and the gradient have an estimate, a standard error a t-value and a p-value. To get the gradient and intercept of this line on a log-log scale (like it appears in Figure 1) you would calculate the exponential of each number, done in R using exp(). This is true for all proceeding models too, so keep that in mind.
The only number we are really that bothered about today is the 'scaled_time' P-Value ('Pr(>|t|)') which is 0.425, not significant.

#### Model 2
Again, a simple call to get the summary of model 2:

```html
# Summary for Model 2
summary(mod_2)
```
This summary is longer! Why? Remember back to Model 2 and Figure 2. Our General Linear Model had countries added as a fixed effect, which gave us five different intercepts BUT because LOCATION was added rather than multiplied, only one gradient. So we have only one value for 'scaled_time', which is the gradient for all of the countries on a log scale, but we have five values for intercept (the first is just called 'intercept' but refers to Australia). Each country's value is not its actual expenential gradient/intercept, but the difference between that specific country's value and the baseline country's (Australia) value. The P-values for each country's intercept and the overall effect of scaled_time are now significant, showing that the inclusion of LOCATION accounted for a significant portion of the variation in scaled_value.

#### Model 3 
You must be getting the hang of this by now...

```html
# Summary for Model 3
summary(mod_3)
```
A longer output again! This time, we have log-values for both the intercept and gradient (slope) of each country's line. This is not surprising given the structure of Model 3, as it accounts for the intercept and rate of change (gradient) for each country compared to Australia. Each country now has its own line with its own intercept and gradient, showing the starting point and the change over time for meat consumption. The intercepts are similar to those in Model 2, but now we also have the gradients (slopes). Remember, these values are on the log scale and can be converted to a more interpretable scale using exp(). The interaction between scaled_time and LOCATION allows for different slopes for each country, though some of these slopes are not statistically significant.

#### Model 4
Now, we have our first model with a random effect and our first Generalised Linear Mixed Model! When we add something as a random effect, we're telling the model that there will be variation due to this factor (in this case, LOCATION), but we don't need to know the exact effect of each level. So what will this do to the output? 
```html
# Summary for Model 4
summary(mod_4)
```
It makes it shorter! Actually, just the same length as Model 1. This is because we're not estimating the intercept for each country individually. Instead, the model accounts for differences between countries in a more general way by including a random effect for LOCATION. The model knows there will be variation in the intercept due to LOCATION, but it doesn't give us individual country intercepts.

#### Model 5
Back to bigger (and better?) models. Now we have fixed and random effects!
```html
# Summary for Model 5
summary(mod_5)
```
Now we have a more complex output due to the random effect of SUBJECT. The random effect doesn’t appear directly in the output as we don’t want to know how SUBJECT specifically affects the results—just that there is variation due to SUBJECT. In Figure 4, each SUBJECT (meat type) has the same gradient but a different intercept, so differences between types of meat now account for some of the variations that were attributed to LOCATION in previous models. This generally reduces the significance of the LOCATION fixed effects, leading to smaller P-values for some of the interactions. The correlations in this output show how the intercepts and slopes for each LOCATION relate within each SUBJECT. For example, a correlation of -0.9 means that if a subject (type of meat) from a location starts with a higher value, it is likely to have a more negative slope (declining over time).

#### Model 6
You made it, we are at our final model output! Model 6 is our most complex model we have fit, and perhaps over-complex for the data, but let's take a look nonetheless. 
```html
# Summary for Model 6
summary(mod_6)
```
This model allows for variations in both the intercepts and slopes for every SUBJECT within each LOCATION, effectively estimating 20 different slopes—one for each combination of SUBJECT and LOCATION. The model also estimates the correlations between the random effects, that is, whether a deviation in the intercept for a SUBJECT in a specific LOCATION is related to the deviation in the slope. For example, if a SUBJECT in a specific location has a higher intercept, does it also have a steeper gradient (faster change over time)?
Despite this complexity, there are still very few significant P-values, which suggests that accounting for all this variation (random effects) has explained much of the apparent significance seen in earlier models. This contrasts with the second model, where every coefficient showed significance. It also highlights how, by accounting for other sources of variation (such as SUBJECT), the apparent significance between LOCATION categories fades. However, there is a non-convergence warning, indicating that the random-effects structure might be too complex for the dataset.

### Limitations
Learning when and how to use and interpret Linear Models through these six models is useful as it offers helpful and simplified learning. However, there are also important limitations that should be kept in mind.

1. It is important to highlight that the progression in this tutorial from simpler to more complex models may create the illusion that adding complexity improves the model. This is not the case and there is a risk that more complex models might fit the data too well (overfitting) so you should always consider which model to use based on what understanding you are trying to gain from modelling your specific dataset for your specific question. It is also important to remember that trying lots of different models and picking the one that fits best or has the lowest P-Value is P-Hacking, compromising the reliability of your finding.
2. The explanations of how the models fit the data simply compare model summary outputs, but model diagnostics are not discussed. Model validity should be interpreted through a variety of techniques, such as residual plots, which aren't mentioned here. In your own modelling, simply summarising the model output would not be sufficient.
3. There are also limitations of the models themselves, such as model complexity, overfitting and convergence issues (particularly in Model 6).
4. The limitation of the assumptions of Generalised Linear Models, such as the normality of random effects, the assumption of a linear relationship between predictors and the assumption of independence of observations within each group.
5. There are only four categories in the random effect SUBJECT. For a Generalised Linear Mixed Model to successfully estimate meaningful effects, it typically requires at least five categories within a random effect. This is perhaps why there were convergence warnings when running Model 6. 




### Challenge
Which TYPE of model (between 1-6) should be used for each of the following scenarios:

#### Scenario 1:
You are investigating the general recovery rates of fish after a pollution event in a lake. You have data for the health status over time for several different fish species, but you don't want to know how each species responds, just the general trend. You expect that each fish species will start at a different health levels, but they will all follow the same general trend over time.

#### Scenario 2:
You are studying the population of an endangered bird species in several national parks over time. You expect that each national park might have a different rate of population growth (different slopes) over time.

### Challenge Answers:
#### Scenario 1:
Model 4 - Model 4 accounts for random intercepts for individual fish species, which allows for differences in the starting health levels of the fish within the lake while still modelling the fixed effects of time. This model captures individual variation in starting health while keeping the time trend consistent across all fish.

#### Scenario 2:
Model 3 - Model 3 includes an interaction between time and location (national parks), allowing each park to have its own rate of population growth. This model captures the varying growth slopes in different parks while still accounting for the overall effect of time.





