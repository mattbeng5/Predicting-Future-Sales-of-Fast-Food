# Predicting Future Sales of Fast Food

## Table of Contents
- [Project Overview](#project-overview)
- [Data Sources](#data-sources)
- [Tools](#tools)
- [Data Cleaning and Preparation](#data-cleaning-and-preparation)
- [Correlation Between Variables](#correlation-between-variables)
- [Linear Regression Models](#linear-regression-models)
- [Conclusion](#conclusion)
- [Recommendations](#recommendations)




## Project Overview
The objective of this project is the analyze the impact events, holidays, and discounts have on demand at two fast food restaurants. This analysis will assist these restaurants in utilizing the correct inventory policies to keep up with demand and reduce waste and costs.

## Data Sources
Training Sales Data: From file "historicsales_fastfooditems_train.csv", this contains sales data from 2 fast food restaurants
Testing Sales Data: From file "historicsales_fastfooditems_test.csv", this contains a small subset of sales data to test predictive modeling

## Tools
-R (tidyverse)

## Data Cleaning and Preparation
In the initital phase of this analysis, we performed the following tasks:
1. Replace null values in the discount_percent column of both tables with 0
2. Transform date columns from character data type to date data type
```R
# Load tidyverse and import CSV files
library(tidyverse)

test_data <- read_csv("/kaggle/input/historical-sales-data-for-fast-food-items/historicsales_fastfooditems_test.csv")
training_data <- read_csv("/kaggle/input/historical-sales-data-for-fast-food-items/historicsales_fastfooditems_train.csv")

# Replace Null values with 0 in discount_percent column
training_data <- 
	training_data %>% 
	mutate(discount_percent = ifelse(is.na(discount_percent), 0, discount_percent))			

test_data <- 
	test_data %>% 
	mutate(discount_percent = ifelse(is.na(discount_percent), 0, discount_percent))			

# Transform Date Format
training_data <- training_data %>% mutate(date = as.Date(date, format = "%d-%b-%y"))
test_data <- test_data %>% mutate(date = as.Date(date, format = "%d-%b-%y"))
```
## Correlation Between Variables
We will first examine the correlation between discount_percent and sales_quantity. We will then stratify the data by restaurant and item_name to gain deeper insights
```R
# Discount and Sales Quantity Correlation

correlation <- round(cor(training_data$discount_percent, training_data$sales_quantity), 2)

# Scatter Plot for Correlation Between discount_percent and sales_quantity
ggplot(training_data, aes(x = discount_percent, y = sales_quantity)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +   
  theme_minimal() + 
  labs(title = "Correlation Between Discount Percent and Sales Quantity",
       x = "Discount Percent",
       y = "Sales Quantity")
```
A correlation of 0.64 suggests a moderate relationship between discount_percent and sales_quantity. Now, let's analyze whether discounts on specific items at each restaurant show an even stronger correlation.

```R
# Which restaurant and item pair have the highest correlation of discount_percent and sales_quantity
r1_burger <- training_data %>%
  filter(restaurant == 'R1' & item_name == 'Burger')
r1_burger_cor <- round(cor(r1_burger$discount_percent, r1_burger$sales_quantity), 2)
 
r1_salad <- training_data %>%
  filter(restaurant == 'R1' & item_name == 'Salad')
r1_salad_cor <- round(cor(r1_salad$discount_percent, r1_salad$sales_quantity), 2)

r2_burger <- training_data %>%
  filter(restaurant == 'R2' & item_name == 'Burger')
r2_burger_cor <- round(cor(r2_burger$discount_percent, r2_burger$sales_quantity), 2)

r2_salad <- training_data %>%
  filter(restaurant == 'R2' & item_name == 'Salad')
r2_salad_cor <- round(cor(r2_salad$discount_percent, r2_salad$sales_quantity),2)

# Scatter plot of correlation between discount percent and sales quantity
ggplot(training_data, aes(x = discount_percent, y = sales_quantity, color = item_name)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + 
  facet_wrap(~ restaurant) +  
  scale_color_brewer(palette = "Pastel1") +
  theme_minimal() + 
  labs(title = "Correlation Between Discount Percent and Sales Quantity",
       x = "Discount Percent",
       y = "Sales Quantity",
       color = "Item Name")
```
Both menu items at restaurant 1 have strong correlations between percentage discounts being offered and sales quantity, while only the salads at restaurant 2 show a strong correlation. The sales quantity of burgers at restaurant 2 only shows a moderate correlation with the discount percentage, and when looking at the above scatterplot we can see the customers at this location heavily favor salads over burgers when discounts are applied.

Our next step is going to examine the correlations between the burger sales at restaurant 2 and the other variables in this dataset: is_weekend, is_friday, is_holiday, and baseprice_USD.

```R
# Restaurant 2 Burger Sales Correlations
burger_weekend_cor <- round(cor(r2_burger$sales_quantity, r2_burger$is_weekend),2)

burger_friday_cor <- round(cor(r2_burger$sales_quantity, r2_burger$is_friday),2)

burger_holiday_cor <- round(cor(r2_burger$sales_quantity, r2_burger$is_holiday), 2)

# Burger vs Salad Prices across both restaurants
item_price <- 
    training_data %>%
    group_by(item_name,restaurant)%>%
    select(restaurant, item_name, baseprice_USD)%>%
    distinct()
```
The sales quantity of burgers at restaurant 2 has a moderate correlation with the weekend and a weak correlation with both friday and holiday factors.

When comparing the baseprice of these two items at each restaurant, restaurant 2 has higher baseprices for their items than restaurant 1. With the salad being $18, it would make sense that there is a strong correlation between discounts and sale quantity of salads. This also may be a reason why the burger sales at this restaurant 2 shows no strong correlation with the other variables as they are usually priced out of buying a salad and only buy it when discounted.

## Linear Regression Models

For the next portion of our analysis, we will be creating linear regression models to analyze how effectively discount_percent, is_weekend, is_friday, and is_holiday explain variations in sales_quantity at each restaurant. We will check for the multicollinearity of our predictor variables first. 
```R
# Create data sets for each restaurant
R1 <- training_data %>% filter(restaurant == "R1")
R2 <- training_data %>% filter(restaurant == "R2")

# Linear regression for individual predictors at restaurant 1
lm_discount_1 <- lm(sales_quantity ~ discount_percent, data = R1)
lm_weekend_1 <- lm(sales_quantity ~ is_weekend, data = R1)
lm_friday_1 <- lm(sales_quantity ~ is_friday, data = R1)
lm_holiday_1 <- lm(sales_quantity ~ is_holiday, data = R1)
broom::glance(lm_discount_1)
broom::glance(lm_weekend_1)
broom::glance(lm_friday_1)
broom::glance(lm_holiday_1)

# Linear regression for all predictors at restaurant 1
lm_all_1 <- lm(sales_quantity ~ discount_percent + is_weekend + is_friday + is_holiday, data = R1)

# Linear regressions for individual predictors at restuaraunt 2
lm_discount_2 <- lm(sales_quantity ~ discount_percent, data = R2)
lm_weekend_2 <- lm(sales_quantity ~ is_weekend, data = R2)
lm_friday_2 <- lm(sales_quantity ~ is_friday, data = R2)
lm_holiday_2 <- lm(sales_quantity ~ is_holiday, data = R2)

# Linear regression for all predictors at restaurant 2
lm_all_2 <- lm(sales_quantity ~ discount_percent + is_weekend + is_friday + is_holiday, data = R2)
broom::glance(lm_all_2)
```
Based on our analysis, combining all the predictor variables at Restaurant 1 gives us the most accurate model for predicting sales variation. Discounts on their own explain about 72% of the variability, but when we add weekend, Friday, and holiday indicators, the model explains up to 85% of the sales variation. In contrast, for Restaurant 2, neither the individual predictors nor the combined model seem to significantly capture the variability in sales.

In the last step of our analysis, we'll use the test data to generate sales predictions with the combined model from on burgers for Restaurant 1 and then calculate the Root Mean Squared Error (RMSE) to assess its accuracy.
```R
# R1 Burger linear regression

lm_r1_burger <- lm(sales_quantity ~ discount_percent + is_weekend + is_friday + is_holiday, data=r1_burger)
broom::glance(lm_r1_burger)

# Create predicted sales column
test_data$predicted_sales <- predict(lm_r1_burger, newdata = test_data)

# Calculate RMSE
rmse <- sqrt(mean((test_data$sales_quantity - test_data$predicted_sales)^2))
rmse

mean_sale <- mean(test_data$sales_quantity)
mean_sale
```
The mean sale of the test data is 161.10, with an RMSE of 16.3, our model's predictions are about 10.1% ($16.30) off from the actual sales value.

## Conclusion
After analyzing the relationship between discounts and sales at both restaurants, we can conclude that discounts play a significant role in driving sales—but the strength of this relationship varies by location and item type. At Restaurant 1, both hamburger and salad sales are strongly correlated with discounts. In contrast, for Restaurant 2, salad sales show a strong relationship with discounts, while hamburger sales are only moderately related to discounts, have a moderate correlation with the weekend, and a weak correlation with Friday and holiday variables. We believe that pricing differences influence these outcomes: hamburgers are priced hamburgers are priced 2 dollars higher and salads 3 dollars higher at Restaurant 2 (resulting in base prices of 10 dollars for hamburgers and 18 dollars for salads), which may make the sales of salads more sensitive to discount offers as customers seek a better deal.

To further evaluate these relationships, we employed linear regression models. For Restaurant 1, the model shows that discounts alone account for 72% of the variation in sales, and when combined with weekend, Friday, and holiday indicators, the full model explains 85% of the sales variability. On the other hand, the models for Restaurant 2 do not suggest that our predictors significantly account for sales variation, indicating that additional factors need to be explored. Variables such as local income levels, competitor activity, special events, and time of day could be investigated to better understand sales performance at Restaurant 2.

Finally, when applied to the Restaurant 1 test set, our full model forecasted burger sales within an average error of 10.1% (≈$16.30), establishing a clear benchmark for accuracy.

## Recommendations
1. Targeted Discounts
   -Restaurant 1:  Both burgers and salads show strong price sensitivity. Lean into discounts on these items whenever you need a predictable sales uplift.
   -Restaurant 2: Focus promotions on salads, which drive the biggest lift. Experiment with burger bundle promotions (for example, burger + side combos) to measure effectiveness in driving incremental sales.

2. Additional Variable Testing for Restaurant 2
  - Our linear regression models did not suggest the provided predictor variables significantly account for sales variation.
  - Test new predictors: income levels, competitor pricing, special events, time of day.
    
3. Inventory and Staffing Alignment
  - Restaurant 1: Using the 10.1% margin of error, hold extra stock and increase staff on fridays, the weekend, and when there are discounts offered.
  - Restaurant 2: Hold extra inventory of salad items when they are being discounted. Defer burger inventory changes until new variables are tested.
    









