## The Influence of Effect Placement on Generalised Linear Model Outputs

### Tutorial aims
- Understand How the Placement of Effects and How They are Combined (* or +) Changes Your Model
- Decide When to Use Which Effect Placement and Whether to Multiply or Add
- Model Building and Performance Comparison through Result Interpretation

### Tutorial pre-requisites
- A basic knowledge of data wrangling
- How to code Generalised Linear Models and Generalised Linear Mixed Models

### Introduction
Hello Linear Modellers! Welcome to this tutorial on how and why the placement of effects within your Generalised Linear (Mixed) Models influences the outputs of your models, what the outputs mean and in what scenario you should use each one. 
In this tutorial, we will use a dataset on meat consumption through time to examine how to account for variation in consumption across countries and subjects (eg, beef, poultry, etc.) within our model, and in what cases we would use each iteration of the model.
It is important to highlight that when modelling data, you should not try out different models and choose the one that best fits your data, as this leads to P-Hacking. However, in this tutorial, we are trying and testing the effects of the different models to demonstrate what each one means, their outputs, and in what scenarios you would choose each one.

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
- The website that the data is sourced from: https://www.kaggle.com/datasets/allenkong/worldwide-meat-consumption?resource=download gives us context about the data, and it says that there are two different metrics for meat consumption. There are measurements for every country in every year for both "thousand tonnes of carcass weight" and "kilograms of retail weight per capita". For the model to work effectively we cannot compare both of them within the same model, so we have to remove one. The latter is more reflective and gives us smaller numbers to work with (which models tend to prefer), so we will remove all values for carcass weight.





### The Influence of Effect Placement and Combination on the Meaning of Your Model



