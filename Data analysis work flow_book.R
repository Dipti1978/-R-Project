# Name :- Dipti
# Title : Guided project, Data Analysis work flow part 2

library(tidyverse)
library(lubridate)

# Reading data from CSV file

sales <- read_csv("sales2019.csv")

#Data Exploration
# How big is the data

dim(sales)

# what are the column names

colnames(sales)

# what are types of all columns

for (col in colnames(sales)) {
  paste0(col, " : ", typeof(sales[[col]])) %>% print
}

# Is there missing data anywhere?

for (col in colnames(sales)) {
  paste0(col, 
         ", number of missing data rows: ", 
         is.na(sales[[col]]) %>% sum) %>% print
}

## Handling missing data

# Remove the rows with no user_submitted_review

complete_sales <- sales %>% 
  filter(
    !is.na(user_submitted_review)
  )
complete_sales

# Calculate the mean of the total_purchased column, without the missing values

purchase_mean <- complete_sales %>% 
  filter(!is.na(total_purchased)) %>% 
  pull(total_purchased) %>% 
  mean
purchase_mean

# Assign this mean to all of the rows where total_purchased was NA

complete_sales <- complete_sales %>% 
  mutate(
    imputed_purchases = if_else(is.na(total_purchased), 
                                purchase_mean,
                                total_purchased)
  )
complete_sales

# Processing Review Data

complete_sales %>% pull(user_submitted_review) %>% unique

#The reviews range from outright hate ("Hated it") to positive ("Awesome!"). 
#We'll create a function that uses a `case_when()` function to produce the output. 
#`case_when()` functions can be incredibly bulky in cases where there's many options, 
#but housing it in a function to `map` can make our code cleaner.

is_positive <- function(review) {
  review_positive = case_when(
    str_detect(review, "Awesome") ~ TRUE,
    str_detect(review, "OK") ~ TRUE,
    str_detect(review, "Never") ~ TRUE,
    str_detect(review, "a lot") ~ TRUE,
    TRUE ~ FALSE # The review did not contain any of the above phrases
  )
}

complete_sales <- complete_sales %>% 
  mutate(
    is_positive = unlist(map(user_submitted_review, is_positive))
  )
complete_sales

# Comparing Book Sales Between Pre- and Post-Program Sales

complete_sales <- complete_sales %>% 
  mutate(
    date_status = if_else(mdy(date) < ymd("2019/07/01"), "Pre", "Post")
  )
complete_sales %>% 
  group_by(date_status) %>% 
  summarize(
    books_purchased = sum(imputed_purchases)
  )

complete_sales %>% 
  group_by(date_status, title) %>% 
  summarize(
    books_purchased = sum(imputed_purchases)
  ) %>% 
  arrange(title, date_status)

# Comparing Book Sales Within Customer Type

complete_sales %>% 
  group_by(date_status, customer_type) %>% 
  summarize(
    books_purchased = sum(imputed_purchases)
  ) %>% 
  arrange(customer_type, date_status)

# Comparing Review Sentiment Between Pre- and Post-Program Sales


complete_sales %>% 
  group_by(date_status) %>% 
  summarize(
    num_positive_reviews = sum(is_positive)
  )


















