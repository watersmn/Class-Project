# Data Wrangling for Amazon Price versus In-Store Project # Ming Waters

library(dplyr)
library(tidyr)
library(ggplot2)
library(outliers)

#Change working directory
setwd("C:/Users/IAZ867/Desktop/Desktop/Class/Assignments")
 
# Load original csv file and rename to w_price

 w_price <- read.csv("original_amazon_compare.csv")

# Select needed columns 
 
w_price <- select(w_price, id, price, price_amazon, sale_online, 
                  datediff, PRICETYPE, category)

# Add new column to calculate % price difference between store price 

w_price <- mutate(w_price, "delta" = (price_amazon - price))

# versus amazon price (Percentages will allow us to not neglect low cost items)
w_price <- mutate(w_price, "p_difference" = (delta/price))

# Create a vector to determine if id transfered over to price
id_typo <- which(w_price$id == w_price$price)

# two rows contain match 1034, 1614, remove the rows
w_price <- w_price[-id_typo, ]

# Graph price difference to visualize data entry errors
# determining what setting to set threshold for exclusion
ggplot(w_price, aes(x = category, y = p_difference, col = category)) 
+ geom_point()

# Visualization showed that the outliers seem to be points where p_difference 
# > 100 
 
# Create vector with values < 100 p_difference naming vector error
error <- which(w_price$p_difference > 100)

# Remove values from dataset
w_price2 <- w_price[-error,]

# Plot new data set to see range of category and verify erronous have been removed
ggplot(w_price2, aes(x = category, y = p_difference, col = category)) 
+ geom_point()

# Create data frames by category type
electronics_set <- filter(w_price2, category == "Electronics")
home_app <- filter(w_price2, category == "Home and Appliances")
mix <- filter(w_price2, category == "Mix")
office <- filter(w_price2, category =="Office Products")
pharm_health <- filter(w_price2, category == "Pharmacy and Health")

# Plot histogram to view distribution across categories and to determine spread
ggplot(electronics_set, aes(x = p_difference)) + geom_histogram(binwidth = 1.0)

# Electronic sets has a skewed negative bias with a few outliers that need to be 
# investigated on the high positive side
median(electronics_set$p_difference, na.rm = TRUE)

#median = -0.04 data is shows amazon price bias
ggplot(home_app, aes(x = p_difference)) + geom_histogram(binwidth = 1.0)

#home appliances has extreme values on positive side and a median with zero
median(home_app$p_difference, na.rm = TRUE)


ggplot(mix, aes(x = p_difference)) + geom_histogram(binwidth = 0.5)
# data shows positive skewed with extreme values
median(mix$p_difference, na.rm = TRUE)
# mix data shows in store as price value 0.165

ggplot(office, aes(x = p_difference)) + geom_histogram(binwidth = 0.5)
# office shows small counts of positive extreme values
median(office$p_difference, na.rm = TRUE)
# office shows amazon price value -0.111

ggplot(pharm_health, aes(x = p_difference)) + geom_histogram(binwidth = 0.5)
# data shows multiple counts of postitive extremes with large postive skew
median(pharm_health$p_difference, na.rm = TRUE)
# median shows price value for in store 2.030

# Data is not normal and traditional outlier test such as Grubbs, chi^2 will 
# not apply, a random inspection of data set shows multiple obvious typos
# View extreme positives of electronics
View(electronics_set)
# Row 153 shows -0.90 percent difference, inspection of values shows an
# in store price of 69.99 versus an amazon price of 6.98, most 
# items would not have such a significance difference, 
# therefore, it would be resonable to conclude this was a typo 
# since the values are so similar but with a difference in decimal placement

# To view outliers for non-normal data, the median IQR 1.5 removal approach will be taken

# visualize boxplot
ggplot(w_price2, aes(factor(category), p_difference)) + geom_boxplot()
# UglyPlots

# Apply IQR to electronic data set
eI <- 1.5*(IQR(electronics_set$p_difference))

# Get quartiles
summary(electronics_set$p_difference)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -0.98920 -0.23610 -0.04924  0.40260  0.00000 79.08000 

# Calculate upper and lower thresholds
e_upper <- (0+eI)
e_lower <- (-0.23 - eI)
# lower = -0.584, upper = 0.354 
# inspect data to determine if they are valid outliers
# setting lower boundaries at < - 0.590  due to the price at -0.58 seem like a 
# reasonable price difference due to the sale notation 
# on the upper side removing outliers > 0.354 seems reasonable 
# as the price gaps between items seem questionable and no items 
# were on sale and price descriptions in original file indicates this 
# is a good threshold

# Create vector to remove outliers
out_ele <- which(electronics_set$p_difference < -0.590 
                 | electronics_set$p_difference > 0.354)
 
# Remove from set
electronics_set <- electronics_set[-out_ele,]
# Boxplot to look at spread, data is not normal but errors have been removed
boxplot(electronics_set$p_difference)


# Repeat for home_app
# Apply IQR to electronic data set
hI <- 1.5*(IQR(home_app$p_difference))

# IQR*1.5 = 0.3377

# Get quartiles
summary(home_app$p_difference)
#      Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.9999 -0.1000  0.0000  1.4930  0.1252 89.7900

# Calculate upper and lower thresholds
h_upper <- (0.1252 + hI)
h_lower <- (-0.1 - hI)

# lower = -0.4377, upper = 0.46293 
# inspect data and product type to determine if they are valid outliers

# Create vector to remove outliers
out_app <- which(home_app$p_difference < -0.-4378 | home_app$p_difference > 0.462)

# Remove from set
home_app <- home_app[-out_app,]

# Boxplot to look at spread, data is not normal but errors have been removed
boxplot(home_app$p_difference)

# Reoeat for mix data
# Apply IQR to mix data set
mI <- 1.5*(IQR(mix$p_difference))

# Get quartiles
summary(mix$p_difference)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -0.97580 -0.09099  0.16520  1.57000  1.50200 51.51000 

# Calculate upper and lower thresholds
m_upper <- (1.50200 + mI)
m_lower <- (-0.09099 - mI)

# lower = -2.4801, upper = 3.891176
# inspect data to determine if they are valid outliers
# Data riddled with typos....for example cost of chapstic in store 
# 1.98 versus 253.00 on amazon, 
# inspection showed suspect errors for differenes > 2


# Create vector to remove outliers
out_mix <- which(mix$p_difference < -2.48 | mix$p_difference > 2.0)

# Remove from set
mix <- mix[-out_mix,]

# Boxplot to look at spread, data is not normal but errors have been removed
boxplot(mix$p_difference)

# Repeat for office data
# Apply IQR to office data set
oI <- 1.5*(IQR(office$p_difference))

# Get quartiles
summary(office$p_difference)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.9600 -0.3433 -0.1112  0.5264  0.5104 14.2400 

# Calculate upper and lower thresholds
o_upper <- (0.5104 + oI)
o_lower <- (-0.3433 - oI)

# lower = -1.6239, upper = 1.791
# inspect data to determine if they are valid outliers
# Data showed multiple entries of same item and small items like amazon cables


# Create vector to remove outliers
out_office <- which(office$p_difference < -1.6239 | office$p_difference > 1.791)

# Remove from set
office <- office[-out_office,]

# Boxplot to look at spread, data is not normal but errors have been removed
boxplot(office$p_difference)


# Repeat for Pharm_health data
# Apply IQR to office data set
pI <- 1.5*(IQR(pharm_health$p_difference))

# Get quartiles
summary(pharm_health$p_difference)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.8004 -0.1328  2.0250  8.0040  8.8560 80.7400 

# Calculate upper and lower thresholds
p_upper <- (8.8560 + pI)
p_lower <- (-0.1328 - pI)

# lower = -13.61622, upper = 22.3394
# inspect data to determine if they are valid outliers
# Data showed multiple lots of odd high pricing for amazon items that 
# shows very low prices in store although the upper recommends 22, 
# inspetion of the data
# shows threshold of 17 on higher end is more appropriate


# Create vector to remove outliers
out_pharm <- which(pharm_health$p_difference < -13.62 | 
                     pharm_health$p_difference > 17)

# Remove from set
pharm_health <- pharm_health[-out_pharm,]

# Boxplot to look at spread, data is not normal but errors have been removed
boxplot(pharm_health$p_difference)

# Recombind the various categories into one data frame
w_price3 <- rbind(electronics_set, home_app, pharm_health, mix, office)

# Boxplot each of the categories after removal of outliers

ggplot(w_price3, aes(factor(category), p_difference)) + geom_boxplot()

# Analysis to determine which category has the best pricing for on-line versus
# In-Store - using histograms and summary to look at over all median's 
# and distributions

# Electronics
ggplot(electronics_set, aes(x = p_difference)) + geom_histogram(binwidth = 0.1)+
  stat_bin(binwidth= .1) + ylim(c(0, 400)) +  
  stat_bin(binwidth= .1, geom="text", aes(label=..count..), vjust=-1.5) + 
  ggtitle("Percent Difference Distribution of Electronics") + 
  theme(plot.title = element_text(color="blue", size=14, face="bold.italic"))

# Plot of home appliances

ggplot(home_app, aes(x = p_difference)) + geom_histogram(binwidth = 0.1)+
  stat_bin(binwidth= .1) + ylim(c(0, 200)) +  
  stat_bin(binwidth= .1, geom="text", aes(label=..count..), vjust=-1.5) + 
  ggtitle("Percent Difference Distribution of Home Appliances") + 
  theme(plot.title = element_text(color="blue", size=14, face="bold.italic"))

# Plot of Mix Items

ggplot(mix, aes(x = p_difference)) + geom_histogram(binwidth = 0.1)+
  stat_bin(binwidth= .1) + ylim(c(0, 300)) +  
  stat_bin(binwidth= .1, geom="text", aes(label=..count..), vjust=-1.5) + 
  ggtitle("Percent Difference Distribution of Mix Items") + 
  theme(plot.title = element_text(color="blue", size=14, face="bold.italic"))

# Plot of Office Products

ggplot(office, aes(x = p_difference)) + geom_histogram(binwidth = 0.1)+
  stat_bin(binwidth= .1) + ylim(c(0, 100)) +  
  stat_bin(binwidth= .1, geom="text", aes(label=..count..), vjust=-1.5) + 
  ggtitle("Percent Difference Distribution of Office") + 
  theme(plot.title = element_text(color="blue", size=14, face="bold.italic"))

# Plot of Pharmacy and Health Products

ggplot(pharm_health, aes(x = p_difference)) + geom_histogram(binwidth = 1)+
  stat_bin(binwidth= 2) + ylim(c(0, 200)) +  
  stat_bin(binwidth= 2, geom="text", aes(label=..count..), vjust=-1.5) + 
  ggtitle("Percent Difference Distribution of Pharmacy and Health") + 
  theme(plot.title = element_text(color="blue", size=14, face="bold.italic"))

# Create another column in data frame to see if pricing count is better or 
# neutral in store

w_price4 <- mutate(w_price3, bias = ifelse(p_difference  >= 0, 1, 0))

# Create new sets to calculate the distribution
electronics_set <- filter(w_price4, category == "Electronics")
home_app <- filter(w_price4, category == "Home and Appliances")
mix <- filter(w_price4, category == "Mix")
office <- filter(w_price4, category =="Office Products")
pharm_health <- filter(w_price4,category == "Pharmacy and Health")

# Calculate the mean to determine percentage of time, it was neutral 
# or better to buy in store  

mean(electronics_set$bias)
mean(home_app$bias)
mean(mix$bias)
mean(office$bias)
mean(pharm_health$bias)

# Next project is to determine where to shop overall if items are on sale
# in the store or on-line or both

# First I need to develop binary categories for the various combos of
# sale, reg or on sale at both
# Cat 1 = regular price online, regular price in store
# Cat 2 = sale online, regular price in store
# Cat 3 = regular price online, sale in store
# Cat 4 = sale price online, sale price in store

w_price4 %>% 
  mutate(cat_price = ifelse(PRICETYPE == 'Regular Price', 
                            ifelse(is.na(sale_online), 'Cat 1', 'Cat 2'), 
                            ifelse(PRICETYPE == 'Sale/Discounted Price', 
                                   ifelse(is.na(sale_online), 'Cat 3', 'Cat 4'), 
                                   NA))) -> w_price5


# View the various distributions of percent difference by category

ggplot(w_price5, aes(x = cat_price, y = p_difference, col = cat_price)) + 
  geom_point()

# Now I want to determine the percentage of time my price categories 
# gives better pricing on line 

# Split the data frame to each category

cat1 <- filter(w_price5, cat_price =="Cat 1")
cat2 <- filter(w_price5, cat_price =="Cat 2")
cat3 <- filter(w_price5, cat_price =="Cat 3")
cat4 <- filter(w_price5, cat_price =="Cat 4")

# Now to calculate the mean of the bias value to give me the % of cost 
# benefits by category pricing

mean(cat1$bias)
mean(cat2$bias)
mean(cat3$bias)
mean(cat4$bias)

# Results are as follows:
# Category 1 which states that the item is reg price in store and online
# shows a better pricing in store only 43% of the time
# Cat 2 = sale online, regular price in store - shows better pricing in store 
# only 26% of the time
# Cat 3 = regular price online, sale in store shows better in store 68% of 
# the time
# Cat 4 = sale price online, sale price in store, this category showed better 
# pricing in the store only 38% of the time

# Conclusion....if the item is on sale both places, buy online, if the price
# is reg. both places...buy online
# and if the price is on sale at one location versus the other...well, common 
# sense, and the data, says buy it where it is on-sale





