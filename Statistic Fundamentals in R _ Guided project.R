---
# title: 'Statistics Fundamentals in R: Guided Project Solutions'
#author: "Dipti"
#date: "26/07/20"
#output: html_document

  
  # Is Fandango Still Inflating Ratings?
  #In October 2015, Walt Hickey from FiveThirtyEight published a [popular article](https://fivethirtyeight.com/features/fandango-movies-ratings/) where he presented strong evidence which suggest that Fandango's movie rating system was biased and dishonest. In this project, we'll analyze more recent movie ratings data to determine whether there has been any change in Fandango's rating system after Hickey's analysis.

# Understanding the Data
#We'll work with two samples of movie ratings: the data in one sample was collected *previous* to Hickey's analysis, while the other sample was collected *after*. Let's start by reading in the two samples (which are stored as CSV files) and getting familiar with their structure.

#{r message=FALSE}
library(readr)
previous <- read_csv('fandango_score_comparison.csv')
after <- read_csv('movie_ratings_16_17.csv')
head(previous)


head(after)

#Below we isolate only the columns that provide information about Fandango so we make the relevant data more readily available for later use. 

#{r message=FALSE}

library(dplyr)
fandango_previous <- previous %>% 
  select(FILM, Fandango_Stars, Fandango_Ratingvalue, 
         Fandango_votes, Fandango_Difference)
fandango_after <- after %>% 
  select(movie, year, fandango)
head(fandango_previous)

head(fandango_after)


#Our goal is to determine whether there has been any change in Fandango's rating system after Hickey's analysis. The population of interest for our analysis is made of all the movie ratings stored on Fandango's website, regardless of the releasing year.

#Because we want to find out whether the parameters of this population changed after Hickey's analysis, we're interested in sampling the population at two different periods in time - previous and after Hickey's analysis - so we can compare the two states.

#The data we're working with was sampled at the moments we want: one sample was taken previous to the analysis, and the other after the analysis. We want to describe the population, so we need to make sure that the samples are representative, otherwise we should expect a large sampling error and, ultimately, wrong conclusions.

#From Hickey's article and from the README.md of [the data set's repository](https://github.com/fivethirtyeight/data/tree/master/fandango), we can see that he used the following sampling criteria:
  
 # * The movie must have had at least 30 fan ratings on Fandango's website at the time of sampling (Aug. 24, 2015).
#* The movie must have had tickets on sale in 2015.

#The sampling was clearly not random because not every movie had the same chance to be included in the sample - some movies didn't have a chance at all (like those having under 30 fan ratings or those without tickets on sale in 2015). It's questionable whether this sample is representative of the entire population we're interested to describe. It seems more likely that it isn't, mostly because this sample is subject to temporal trends - e.g. movies in 2015 might have been outstandingly good or bad compared to other years.

#The sampling conditions for our other sample were (as it can be read in the README.md of [the data set's repository](https://github.com/mircealex/Movie_ratings_2016_17)):
  
#  * The movie must have been released in 2016 or later.
#* The movie must have had a considerable number of votes and reviews (unclear how many from the README.md or from the data).

#This second sample is also subject to temporal trends and it's unlikely to be representative of our population of interest.

#Both these authors had certain research questions in mind when they sampled the data, and they used a set of criteria to get a sample that would fit their questions. Their sampling method is called [purposive sampling](https://www.youtube.com/watch?v=CdK7N_kTzHI&feature=youtu.be) (or judgmental/selective/subjective sampling). While these samples were good enough for their research, they don't seem too useful for us.

# Changing the Goal of our Analysis
#At this point, we can either collect new data or change our the goal of our analysis. We choose the latter and place some limitations on our initial goal.

#Instead of trying to determine whether there has been any change in Fandango's rating system after Hickey's analysis, our new goal is to determine whether there's any difference between Fandango's ratings for popular movies in 2015 and Fandango's ratings for popular movies in 2016. This new goal should also be a fairly good proxy for our initial goal.

# Isolating the Samples We Need
#With this new research goal, we have two populations of interest:

#1. All Fandango's ratings for popular movies released in 2015.
#1. All Fandango's ratings for popular movies released in 2016.

#We need to be clear about what counts as popular movies. We'll use Hickey's benchmark of 30 fan ratings and count a movie as popular only if it has 30 fan ratings or more on Fandango's website.

#Although one of the sampling criteria in our second sample is movie popularity, the `fandango_after` dataframe doesn't provide information about the number of fan ratings. We should be skeptical once more and ask whether this sample is truly representative and contains popular movies (movies with over 30 fan ratings).

#One quick way to check the representativity of this sample might be to sample randomly 10 movies from it and then check the number of fan ratings ourselves on Fandango's website. 


set.seed(1)
sample_n(fandango_after, size = 10)

#Above we used a value of 1 as the random seed. This is good practice because it suggests that we weren't trying out various random seeds just to get a favorable sample.

#After checking the number of fan ratings for the movies above, we discover that as of August, 2019 Fandango no longer uses the 5-Star Fan Ratings described above. Instead, Fandango now uses the [Rotten Tomatoes verified Audience Score](https://editorial.rottentomatoes.com/article/introducing-verified-audience-score/). These are the number of fan ratings we found on [Rotten Tomatoes](https://www.rottentomatoes.com/):

set.seed(1)
sampled <- sample_n(fandango_after, size = 10)

# Create a single column tibble of Rotten Tomato review counts

reviews <- tibble(reviews = c(13569, 74904, 24293, 4141, 30183, 48952, 14328, 59359, 54765, 82222))
bind_cols(sampled, reviews)


#All ten movies sampled have well above 30 fan ratings, but it is possible that the Rotten Tomatoes Verified Audience user base is larger than the Fandango user base. We cannot really say with confidence whether these review numbers are comparable to the Fandango fan ratings. In addition, time has passed since Hickey's analysis, giving more fans an opportunity to submit reviews. So even if we did still have access to Fandango's 5-star fan ratings, we would have no way to compare the number of fan ratings we see to the number that Hickey observed. 

#Let's move on to the `fandango_previous` dataframe that does include the number of fan ratings for each movie. The documentation states clearly that there're only movies with at least 30 fan ratings, but it should take only a couple of seconds to double-check here.


sum(fandango_previous$Fandango_votes < 30)


#If you explore the two data sets, you'll notice that there are movies with a releasing year different than 2015 or 2016. 


head(fandango_previous$FILM, n = 10)


unique(fandango_after$year)


#For our purposes, we'll need to isolate only the movies released in 2015 and 2016.

library(stringr)
fandango_previous <- fandango_previous %>% 
  mutate(year = str_sub(FILM, -5, -2))

#Let's examine the frequency distribution for the Year column and then isolate the movies released in 2015.

fandango_previous %>% 
  group_by(year) %>% 
  summarize(Freq = n())


#Alternatively, we can use the base R `table()` function because we only need to get a quick view of the distribution.

table(fandango_previous$year)

fandango_2015 <- fandango_previous %>% 
  filter(year == 2015)
table(fandango_2015$year)

#Great, now let's isolate the movies in the other data set.

head(fandango_after)

table(fandango_after$year)

fandango_2016 <- fandango_after %>% 
  filter(year == 2016)
table(fandango_2016$year)


# Comparing Distribution Shapes for 2015 and 2016

#Our aim is to figure out whether there's any difference between Fandango's ratings for popular movies in 2015 and Fandango's ratings for popular movies in 2016. One way to go about is to analyze and compare the distributions of movie ratings for the two samples.

#We'll start with comparing the shape of the two distributions using kernel density plots.

library(ggplot2)

# 2015 dataframe is specified in the ggplot call

ggplot(data = fandango_2015, 
               aes(x = Fandango_Stars)) +
  geom_density() +
  
  # 2016 dataframe is specified in the second geom_density() call
  
  geom_density(data = fandango_2016, 
               aes(x = fandango), color = "blue") +
  labs(title = "Comparing distribution shapes for Fandango's ratings\n(2015 vs 2016)",
       x = "Stars",
       y = "Density") +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5), 
                     limits = c(0, 5))



#Two aspects are striking on the figure above:

#* Both distributions are strongly left skewed.
#* The 2016 distribution is slightly shifted to the left relative to the 2015 distribution.

#The left skew suggests that movies on Fandango are given mostly high and very high fan ratings. Coupled with the fact that Fandango sells tickets, the high ratings are a bit dubious. It'd be really interesting to investigate this further - ideally in a separate project, since this is quite irrelevant for the current goal of our analysis.

#The slight left shift of the 2016 distribution is very interesting for our analysis. It shows that ratings were slightly lower in 2016 compared to 2015. This suggests that there was a difference indeed between Fandango's ratings for popular movies in 2015 and Fandango's ratings for popular movies in 2016. We can also see the direction of the difference: the ratings in 2016 were slightly lower compared to 2015.

fandango_2015 %>% 
  group_by(Fandango_Stars) %>% 
  summarize(Percentage = n() / nrow(fandango_2015) * 100)


fandango_2016 %>% 
  group_by(fandango) %>% 
  summarize(Percentage = n() / nrow(fandango_2016) * 100)
```

#In 2016, very high ratings (4.5 and 5 stars) had lower percentages compared to 2015. In 2016, under 1% of the movies had a perfect rating of 5 stars, compared to 2015 when the percentage was close to 7%. Ratings of 4.5 were also more popular in 2015 - there were approximately 13% more movies rated with a 4.5 in 2015 compared to 2016.

#The minimum rating is also lower in 2016 - 2.5 instead of 3 stars, the minimum of 2015. There clearly is a difference between the two frequency distributions.

#For some other ratings, the percentage went up in 2016. There was a greater percentage of movies in 2016 that received 3.5 and 4 stars, compared to 2015. 3.5 and 4.0 are high ratings and this challenges the direction of the change we saw on the kernel density plots.

#Determining the Direction of the Change

#Let's take a couple of summary metrics to get a more precise picture about the direction of the change. In what follows, we'll compute the mean, the median, and the mode for both distributions and then use a bar graph to plot the values.


library(tidyr)

# Mode function from stackoverflow

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
summary_2015 <- fandango_2015 %>% 
  summarize(year = "2015",
    mean = mean(Fandango_Stars),
    median = median(Fandango_Stars),
    mode = mode(Fandango_Stars))
summary_2016 <- fandango_2016 %>% 
  summarize(year = "2016",
            mean = mean(fandango),
            median = median(fandango),
            mode = mode(fandango))

# Combine 2015 & 2016 summary dataframes

summary_df <- bind_rows(summary_2015, summary_2016)

# Gather combined dataframe into a format ready for ggplot

summary_df <- summary_df %>% 
  gather(key = "statistic", value = "value", - year)
summary_df

ggplot(data = summary_df, aes(x = statistic, y = value, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparing summary statistics: 2015 vs 2016",
       x = "",
       y = "Stars")


#The mean rating was lower in 2016 with approximately 0.2. This means a drop of almost 5% relative to the mean rating in 2015.


means <- summary_df %>% 
  filter(statistic == "mean")
means %>% 
  summarize(change = (value[1] - value[2]) / value[1])



#While the median is the same for both distributions, the mode is lower in 2016 by 0.5. Coupled with what we saw for the mean, the direction of the change we saw on the kernel density plot is confirmed: on average, popular movies released in 2016 were rated slightly lower than popular movies released in 2015.

# Conclusion

#Our analysis showed that there's indeed a slight difference between Fandango's ratings for popular movies in 2015 and Fandango's ratings for popular movies in 2016. We also determined that, on average, popular movies released in 2016 were rated lower on Fandango than popular movies released in 2015.

#We cannot be completely sure what caused the change, but the chances are very high that it was caused by Fandango fixing the biased rating system after Hickey's analysis.
