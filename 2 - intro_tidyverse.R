# installing packages
install.packages("tidyverse")

# load package
library(tidyverse)


# load a dataset from package
data() # view all available datasets
data("diamonds") # this function only prepares the dataset (look at environment on the right)
str(diamonds) # when you ACCESS the dataset that is when it is actually loaded into memory


# a dataframe is a list
# - each element (column) is named and can have different type
# - but each value in a column has to be of same type
# - therefore a dataframe is a list containing vectors
diamonds$carat # access elements using $


# a tibble is a superpowered dataframe with more features


# inspecting the dataset
names(diamonds)
str(diamonds)
View(diamonds)


# basic stats
mean(diamonds$depth)
levels(diamonds$cut)
map_dbl(diamonds, mean) # repeat a function for all columns
count(diamonds, cut, sort = T) # count categorical values in a column
count(diamonds, cut, clarity, sort = T) # count categorical values in a column (pivot table)


# piping
diamonds %>% map_dbl(mean)
diamonds %>% count() # versatile count function
diamonds %>% count(cut, sort = T)
diamonds %>% count(cut, clarity, sort = T)


# philosophy of the tidyverse
# - grammar of data manipulation
# - tidy data
# - ecosystem of packages


# basic verbs
diamonds %>% head() # get first 6 rows
diamonds %>% tail(10) # get last 10 rows

diamonds %>% arrange(carat) # sort dataframe
diamonds %>% arrange(carat, desc(cut)) # sorting on multiple columns

diamonds %>% select(cut, clarity, x, y, z) # select a subset of columns
diamonds %>% select(-x, -y, -z) # select all EXCEPT these columns

diamonds %>% filter(cut == "Ideal") # filter a subset of rows
diamonds %>% filter(
  cut == "Ideal",
  depth >= 60
) # multiple conditions
diamonds %>% filter(
  (cut == "Ideal") | (cut == "Fair"),
  depth >= 60
) # multiple conditions
diamonds %>% filter(
  cut %in% c("Ideal", "Fair"),
  depth >= 60
) # multiple conditions


diamonds %>% rename(depth_measurement = depth)
diamonds %>% mutate(volume = x * y * z) # create new column

diamonds %>% summarise(
  count = n(),
  avg_carat = mean(carat),
  avg_depth = mean(depth)
) # aggregation function (some special things like n() are available)
diamonds %>% summarise(across(where(is.numeric), mean)) # conditional aggregation functions available (look at docs for you scenario)


# grouping and method chaining
diamonds %>% group_by(cut) # notice the 'Groups' entry above the printed data (R has set this table as special now)
diamonds %>% 
  group_by(cut) %>% 
  count() # this has identical output to the last count() example in the piping section but he group_by version is more flexible in general
diamonds %>% 
  group_by(cut) %>% 
  summarize(n()) 
diamonds %>% 
  group_by(cut, clarity) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  View() # the 10 most common cut-clarity combinations
diamonds %>% 
  group_by(cut, clarity) %>% 
  summarize(
    n = n(),
    avg_price = mean(price)
  ) %>% 
  filter(n >= 500) %>% 
  arrange(desc(avg_price)) %>% 
  head(10) %>% 
  View() # the 10 most expensive cut-clarity combinations with atleast 500 entries


# warning about grouped aggregations
# Because filtering expressions are computed within groups, they may yield different results on grouped tibbles.
starwars %>% 
  filter(mass > mean(mass, na.rm = TRUE))

starwars %>% 
  group_by(gender) %>% 
  filter(mass > mean(mass, na.rm = TRUE))
