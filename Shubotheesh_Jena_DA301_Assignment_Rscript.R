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
install.packages('tidyverse')
library(tidyverse)

# Import the data set.
data <- read.csv(file.choose(), header=T)

# Print the data frame.
data
View(data)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
data2 = subset(data, select = c('Product', 'Platform', 'NA_Sales', 'EU_Sales', 'Global_Sales'))

# View the data frame.
data2
View(data2)
data2 = data2[-c(90),]
rownames(data2) <- NULL

# View the descriptive statistics.
str(data2)
dim(data2)
typeof(data2)
class(data2)
################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(NA_Sales, Platform, data=data2)
qplot(EU_Sales, Platform, data=data2)
qplot(Global_Sales, Platform, data=data2)

## 2b) Histograms
# Create histograms.
qplot(Platform, data=data2)
qplot(Global_Sales, bins=20, data=data2)

## 2c) Boxplots
# Create boxplots.
qplot(EU_Sales, Platform, data=data2, geom='boxplot')
qplot(NA_Sales, Platform, data=data2, geom='boxplot')
qplot(Global_Sales, Platform, data=data2, geom='boxplot')

###############################################################################

# 3. Observations and insights

## Your observations and insights here ......

# When working with R, removing the redundant columns much like with Python makes dataframes easier to manipulate. 
# There was also a row with an incorrect ‘Platform’ value so was removed. 
# Making different visualisations on sales, I think the scatter and histograms were most insightful. 
# While the scatter graph doesn’t have vertical positioning given the discrete nature of ‘Platform’, we can see that most products had low sales with rare high selling products. 
# From the histogram, we see the different platforms and the amount of sales associated with each one. 
# We see X360 having the highest level of sales and GEN having the lowest. 
# The business could use this visualisation to see which platforms have the most demand and make future decisions off this. 
# It may be more beneficial to prioritise the more popular consoles to optimise profit. 
# Turtle Games should consider the highest frequency platforms from the histogram and the ones with the most games, as well as products with uniquely high sales.



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
View(data2)

# Check output: Determine the min, max, and mean values.
data3=subset(data, select = c('NA_Sales', 'EU_Sales', 'Global_Sales'))
View(data3)

sapply(data3, min)
sapply(data3, max)
sapply(data3, mean)

summary(data3)

# View the descriptive statistics.
str(data3)
dim(data3)
typeof(data3)
class(data3)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
data2_sums <- data2 %>% group_by(Product) %>%
  summarise(sum_EU=sum(EU_Sales), 
            sum_NA=sum(NA_Sales), 
            sum_Global=sum(Global_Sales),
            .groups='drop')

View(data2)

data2_count <- data2 %>% group_by(Product) %>% 
  summarise(total_count=n(),.groups = 'drop') %>% as.data.frame()
data2_count

summary(data2_sums)

# View the data frame.
View(data2_sums)


# Explore the data frame.
str(data2_sums)
dim(data2_sums)
typeof(data2_sums)
class(data2_sums)

sum(is.na(data2$Global_Sales))
sum(is.na(data2$EU_Sales))
sum(is.na(data2$NA_Sales))

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.

qplot(Product, sum_EU, data=data2_sums)
qplot(Product, sum_NA, data=data2_sums)
qplot(Product, sum_Global, data=data2_sums)

data2_sums$Product <- factor(data2_sums$Product)

# Create histograms.
qplot(Product, data=data2_sums)

# Create boxplots.
qplot(Product, sum_EU, data=data2_sums, geom='boxplot')


###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(data2_sums$sum_EU)
# Add a reference line:
qqline(data2_sums$sum_EU, col='red')

qqnorm(data2_sums$sum_NA)
qqline(data2_sums$sum_NA, col='red')

qqnorm(data2_sums$sum_Global)
qqline(data2_sums$sum_Global, col='red')

#skeweddata

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages('moments')
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(data2_sums$sum_EU)
shapiro.test(data2_sums$sum_NA)
shapiro.test(data2_sums$sum_Global)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(data2_sums$sum_EU)
kurtosis(data2_sums$sum_EU)

skewness(data2_sums$sum_NA)
kurtosis(data2_sums$sum_NA)

skewness(data2_sums$sum_Global)
kurtosis(data2_sums$sum_Global)

## 3d) Determine correlation
# Determine correlation.
cor(data2_sums$sum_EU, data2_sums$sum_NA)
cor(data2_sums$sum_EU, data2_sums$sum_Global)
cor(data2_sums$sum_NA, data2_sums$sum_Global)

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

data_bot10 = subset(data2_sums, select = c('Product','sum_Global'))
data_bot10 <- data_bot10[order(data_bot10$sum_Global),]
data_bot10 = data_bot10[1:10,]
data_bot10$Product <- factor(data_bot10$Product)
View(data_bot10)
ggplot(data=data_bot10, aes(x=Product, y=sum_Global)) + geom_bar(stat="identity") + 
  labs(y="Global sales in £millions",title="10 lowest selling games globally")

data_top10 = subset(data2_sums, select = c('Product','sum_Global'))
data_top10 <- data_top10[order(-data_top10$sum_Global),]
data_top10 = data_top10[1:10,]
data_top10$Product <- factor(data_top10$Product)
View(data_top10)
ggplot(data=data_top10, aes(x=Product, y=sum_Global)) + geom_bar(stat="identity") + 
  labs(y="Global sales in £millions",title="10 highest selling games globally")


ggplot(data2_sums) + aes(x=sum_EU, y=sum_NA) + 
  geom_point()+
  stat_smooth(method="lm", se=FALSE) + 
  labs(x="EU sales", y="NA sales", title="EU sales against NA")


over10 <- filter(data2, Global_Sales>20)
View(over10)
ggplot(over10, aes(x=Platform)) +
  geom_histogram(stat='count') +
  labs(y="Frequency", title="Number of games with over £20 million sold globally by platform")

###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# Getting statistical insights into our data gives the business a better idea of what is and is not effective. 
# The mean is a simple but effective way of telling how much on average gets sold. 
# We see that in NA the games sell a little more than in the EU and that together they make up a majority of global sales. 
# However, knowing that some games sold upwards of 20 million, this mean is quite low suggesting certain products definitely dominate for Turtle Games. 
# Grouping the data by product lets us look further into this. 
# However, I found that the discrete variable of product was not effective with any of the plots we looked at before. 
# To make some more insightful graphics from the product id variable, I visualised the games that sold over £20 million worth by platform. 
# The Wii has 5 products that sold over that while there are 3 others with 2 games. 
# With these consoles Turtle Games could prioritise these high selling games over other ones. 
# Finally, I visualised the top and bottom 10 games in terms of sales globally. 
# This involved sorting the database then extracting the top 10 rows before plotting. 
# This would be of great use to Turtle games as they can see which products are most popular and which are least and perhaps make decisions on altering production or marketing. 



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

# 1. Load and explore the data
# View data frame created in Week 5.
data2_sums

# Determine a summary of the data frame.
summary(data2_sums)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model1 <-lm(Global_Sales ~ EU_Sales, data=data2)
model1
summary(model1)

model2 <-lm(Global_Sales ~ NA_Sales, data=data2)
model2
summary(model2)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
par(mar=c(4,4,4,4))
plot(data2$EU_Sales, data2$Global_Sales, main="EU Sales on Global Sales", xlab="EU Sales", ylab="Global Sales")
abline(coefficients(model1))

par(mar=c(4,4,4,4))
plot(data2$NA_Sales, data2$Global_Sales, main="NA Sales on Global Sales", xlab="NA Sales", ylab="Global Sales")
abline(coefficients(model2))

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
data4=subset(data2, select = c('EU_Sales', 'NA_Sales', 'Global_Sales')) 
View(data4)
cor(data4)
install.packages('psych')
library(psych)
corPlot(data4, cex=2)

# Multiple linear regression model.
model4 <- lm(Global_Sales ~ EU_Sales + NA_Sales, data=data4)
model4
summary(model4)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.
data5 <- data4[c(1,98,175,210,10),]
View(data5)

predictTest = predict(model4, newdata=data5,
                      interval='confidence')

predictTest

str(data5)
###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# To look at how different regions sales correlate, I used regression analysis. 
# I used the original dataframe without grouping by products as different platforms may make an impact on our predictions. 
# Doing simple linear regression on global sales with EU and NA sales as the independent variables showed us the following.
# We see the coefficient for EU sales being 1 unit higher than NA sales. 
# This means that EU sales have a bigger impact on global sales compared to NA sales. 
# An increase in EU sales by £1 million should equate to a £2.7 milion increase in global.
# To look at both regions impact together, I tried multiple linear regression. 
# Modelling global sales against EU and NA sales showed us:
# EU has a slightly greater influence on global sales than NA but they are very similar here. 
# With it being close to one, we see the influence isnt overly substantial. 
# Our regression model had an R2 value of 0.969 suggesting a high goodness of fit. 
# We can predict using our model the global sales values based on certain regional sales.
# We see the predicted values are all slightly over the actual value meaning the model is overestimating. 
# Looking at the upper and lower bounds shows that only 1 of the actual values lie in the boundaries. 
# This suggests that the model is quite inaccurate. 
# Perhaps a transformation for some of the variables may yield a more accurate prediction but due to lack of time I could not test this.
# We can tell Turtle Games that there is a positive correlation between the different regions with sales and a slight dominance with EU. 
# However, with our current model the prediction will not be accurate enough to give them insights.


###############################################################################
###############################################################################




