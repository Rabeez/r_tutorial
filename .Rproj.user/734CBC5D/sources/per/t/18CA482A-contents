library(tidyverse) # joins are from dplyr package
library(nycflights13) # contains 5 dataframes which we can use to practice joins


# the data (notice the `flights` table contains some columns from all other tables)
nycflights13::airlines
nycflights13::airports
nycflights13::planes
nycflights13::weather
nycflights13::flights

# I will only select a few columns from the tables to keep printed outputs easy to read
my_flights <- flights %>% 
  select(year, month, day, dep_delay, carrier, tailnum, origin, dest)
my_planes <- planes %>% 
  select(tailnum, year, manufacturer, engine)


# mutating joins (creates new columns or rows)
inner_join(my_flights, my_planes) # natural join (this can cause mistakes so don't use)
my_flights %>% 
  inner_join(my_planes) # more commonly used piping syntax
inner_join(my_flights, my_planes, by = "tailnum")
inner_join(my_flights, my_planes, by = c("tailnum", "year")) # explicit equivalent of the natural join above (shown here as example of multiple column join)
inner_join(my_flights, my_planes, by = c("flight_tail_number" = "plane_tail_number", 
                                         "flight_year" = "plane_year")) # illustrative example of joining when column names don't match (will NOT actually work with this data)
left_join(my_flights, my_planes, by = "tailnum")
right_join(my_flights, my_planes, by = "tailnum")
full_join(my_flights, my_planes, by = "tailnum")


# filtering joins (useful to diagnose problems with joins. Note these DO NOT add the new columns)
# all flights whose tail-number IS NOT listed in the planes database
anti_join(my_flights, my_planes, by = "tailnum")
anti_join(my_planes, my_flights, by = "tailnum")

# all flights whose tail-number IS listed in the planes database
semi_join(my_flights, my_planes, by = "tailnum")


# set operations (these work on the full row unlike the joins)
df1 <- tibble(x = 1:2, y = c(1L, 1L))
df2 <- tibble(x = 1:2, y = 1:2)

intersect(df1, df2)
union(df1, df2)
setdiff(df1, df2) # not commutative
setequal(df1, df2)


#  ------------- Example ------------
# comparing against group max
# This is a VERY common use case of joins especially in dashboard focused SQL queries
# We want to find the flights which had a longer delay than the average delay of their carrier


# as a simpler case, the following code gives the flights which were delayed longer than the average delay time of NYC airport in 2013
my_flights %>% 
  filter(dep_delay > mean(dep_delay, na.rm = T))


# The 'normal' way to do this is to first obtain the avg delays for each carrier in an auxiliary table ...
carrier_delays <- my_flights %>% 
  group_by(carrier) %>% 
  summarise(carrier_avg_delay = mean(dep_delay, na.rm = T))
# ... then join the auxiliary table with the original table ('self-join') and do a filter using the new column (notice this has repeated values)
# In SQL this is by far the most common place where subqueries or CTEs are used
my_flights %>% 
  inner_join(carrier_delays) %>% 
  filter(dep_delay > carrier_avg_delay)

# This idiom is so common that dplyr verbs support this behaviour implicitly
# If you do a filter (or mutate, arrange etc) on a grouped tibble your aggregation functions apply within the group
# however the resulting tibble will still be grouped since no explicit aggregation (summarize, count etc) was used so an ungroup is needed
my_flights %>% 
  group_by(carrier) %>% 
  filter(dep_delay > mean(dep_delay, na.rm = T)) %>% 
  ungroup()

