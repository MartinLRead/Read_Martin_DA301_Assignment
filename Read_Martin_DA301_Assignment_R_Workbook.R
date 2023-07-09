## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library('tidyverse')

# Import the data set.
turtlesales <- read.csv(file.choose(), header=T)

# Print the data frame.
head(turtlesales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales2 <- select(turtlesales, -Ranking, -Year, -Genre, -Publisher)
sales2$Product_factor <- factor(sales2$Product)
# View the data frame.
head(sales2)

column_names <- colnames(sales2)
print(column_names)

# Group product numbers by nearest 1000
# Create intervals of nearest thousand
intervals <- seq(0, max(sales2$Product), by = 1000)

# Group the factor numbers into the nearest thousand
grouped_products <- cut(sales2$Product, breaks = intervals, labels = intervals[-1], right = FALSE)

# Update the column in your data frame with the grouped numbers
sales2$GroupedProducts <- grouped_products
sales2$GroupedProduct_factor <- factor(sales2$GroupedProducts)

# View the descriptive statistics.
summary(sales2)
glimpse(sales2)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(GroupedProduct_factor, Global_Sales, colour=Platform, data=sales2,geom=c('point', 'jitter'))

## 2b) Histograms
# Create histograms.
qplot(GroupedProduct_factor, data = sales2, geom = 'bar') +
  ylab("Count")

grouped_sales <- aggregate(Global_Sales ~ Platform, data = sales2, sum)
ggplot(grouped_sales, aes(x = Platform, y = Global_Sales)) +
  geom_bar(stat = "identity") +
  ylab("Global Sales")

## 2c) Boxplots
# Create boxplots.
qplot(GroupedProduct_factor,Global_Sales, data=sales2, colour=I('red'), geom='boxplot')

###############################################################################

# 3. Observations and insights

## Your observations and insights here ......

# One of the products with a code below 1000 has sales that are a huge outlier.
# This product was identfied using:

sales3 <- arrange(sales2, desc(Global_Sales))
head(sales3)

# As being product code 107.

###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.


# Check output: Determine the min, max, and mean values.


# View the descriptive statistics.


###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.


# View the data frame.


# Explore the data frame.



## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.


# Create histograms.


# Create boxplots.


###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.



## 3b) Perform Shapiro-Wilk test
# Install and import Moments.


# Perform Shapiro-Wilk test.



## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.



## 3d) Determine correlation
# Determine correlation.


###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.


###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.


# Determine a summary of the data frame.


###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.



## 2b) Create a plot (simple linear regression)
# Basic visualisation.


###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.


# Multiple linear regression model.


###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.



###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################


