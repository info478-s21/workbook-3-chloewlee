# Analysis

# Set up - make sure to set your working directory using RStudio
library(tidyr)
library(dplyr)
library(ggplot2)

# Create the `charts/` directory (you can do this from R!)
dir.create("charts", showWarnings = FALSE)

# Load prepped data
health.data <- read.csv("./data/prepped/all_data.csv")

# Are HALE and life expectancy correlated?
# - Plot 2016 life expectancy against 2016 HALE. Save the graph to `charts/`
# - Compute the correlation between 2016 life expectancy against 2016 HALE

data.2016 <- health.data %>% 
  filter(year == 2016)

ggplot(data.2016) + 
  geom_point(mapping = aes(x = le, y = hale)) + 
  labs(title =  "Life Expectancy vs HALE", x = "Life Expectancy",
       y = "HALE") 

ggsave("charts/le_hale_graph.png")

cor(data.2016$hale, data.2016$le)
# correlation is strong, almost 1

# Are HALE and DALYs correlated?
# - Plot 2016 HALE against 2016 DALYs. Save the graph to `charts/`
# - Compute the correlation between 2016 HALE and DALYs

ggplot(data.2016) + 
  geom_point(mapping = aes(x = dalys, y = hale)) +
  labs(title =  "DALYS vs HALE", 
       x = "DALY",
      y = "HALE") 

cor(x = data.2016$daly, y = data.2016$hale)
# Strong negative correlation. 

ggsave("charts/daly_hale_graph.png")

# As people live longer, do they live healthier lives 
# (i.e., is a smaller fraction of life spent in poor health)?
# Follow the steps below to attempt to answer this question.

# First, you will need to reshape the data to create columns *by metric-year*
# This will create `hale_2016`, `hale_1990`, `le_2016`, etc.
# To do this, I suggest that you use the `pivot` function in the new
# tidyverse release:https://tidyr.tidyverse.org/articles/pivot.html#wider

data_wide <- health.data %>% 
  pivot_wider(names_from = year, 
              values_from = c(hale, le, dalys))

# Create columns to store the change in life expectancy, and change in hale
data_wide <- data_wide %>% 
  mutate(hale_diff = hale_2016 - hale_1990,
         le_diff = le_2016 - le_1990)

# Plot the *change in hale* against the *change in life expectancy*
# Add a 45 degree line (i.e., where x = y), and save the graph to `charts/`
# What does this mean?!?! Put your interpretation below

ggplot(data_wide) + 
  geom_point(mapping = aes(x = le_diff, y = hale_diff)) + 
  labs(title =  "Life Expectancy vs HALE difference", 
       x = "Change in Life Expectancy",
       y = "Change in HALE") + 
  geom_abline(intercept = 0, slope = 1) + 
  xlim(-15, 20) + ylim(-15,20)

ggsave("charts/changeplot.png")

# Change in life expectancy and HALE is very similar from 
# looking at the difference in 1990 and 2016. 

