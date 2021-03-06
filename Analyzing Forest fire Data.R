
# Exploratory visualization of forest fire data

#Name: Dipti

#Date: 22-07-2010

#Loading Packages

library(readr)
library(dplyr)
library(ggplot2)
library(purrr)

#Importing Data

forest_fires <- read_csv("forestfires.csv")

#Create a bar chart showing the number of forest fires occuring during each month

fires_by_month <- forest_fires %>%
  group_by(month) %>%
  summarize(total_fires = n()) 
  ggplot(data = fires_by_month) +
  aes(x = month, y = total_fires) +
  geom_bar(stat = "identity")  +
  theme(panel.background = element_rect(fill = "pink"), 
        axis.line = element_line(size = 0.25, 
                                 colour = "black"))
  #Create a bar chart showing the number of forest fires occurring on each day of the week

  fires_by_DOW <- forest_fires %>%
    group_by(day) %>%
    summarize(total_fires = n())
  ggplot(data = fires_by_DOW) +
    aes(x = day, y = total_fires) +
    geom_bar(stat = "identity") +
    theme(panel.background = element_rect(fill = "white"), 
          axis.line = element_line(size = 0.25, 
                                   colour = "black")) 
  #Change the data type of month to factor and specify the order of months

  forest_fires <- forest_fires %>%
    mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", 
                                            "jun", "jul", "aug", "sep", "oct", "nov", "dec")), 
           day = factor(day, levels = c("sun", "mon", "tue", "wed", "thu", "fri", "sat")))
  
  ## once you have reordered the months and days of the week, you can re-run the bar chart code above
  # to create new bar graphs
  
  #Write a function to create a boxplot for visualizing variable distributions by month and day of the week
  
  
  create_boxplots <- function(x, y) {
    ggplot(data = forest_fires) + 
      aes_string(x = x, y = y) +
      geom_boxplot() +
      theme(panel.background = element_rect(fill = "pink"))
  }
  ## Assign x and y variable names 
  x_var_month <- names(forest_fires)[3] ## month
  x_var_day <- names(forest_fires)[4] ## day
  y_var <- names(forest_fires)[5:12]
  
  ## use the map() function to apply the function to the variables of 
  
  month_box <- map2(x_var_month, y_var, create_boxplots) ## visualize variables by month
  day_box <- map2(x_var_day, y_var, create_boxplots) ## visualize variables by day
  
  #Create scatter plots to see which variables may affect forest fire size
  
  create_scatterplots = function(x, y) {
    ggplot(data = forest_fires) + 
      aes_string(x = x, y = y) +
      geom_point() +
      theme(panel.background = element_rect(fill = "white"))
  }
  ## Assign x and y variable names 
  x_var_scatter <- names(forest_fires)[5:12]
  y_var_scatter <- names(forest_fires)[13]
  ## use the map() function to apply the function to the variables of interest
  scatters <- map2(x_var_scatter, y_var_scatter, create_scatterplots)

  
  
      
  
  
  




