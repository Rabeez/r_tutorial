library(tidyverse)

# --------- rstudio cheatsheets ------------
# dplyr, reshaper, purrr etc


# --------- loading data -------------------
# csv file
mtcars_df <- read_csv("https://github.com/tidyverse/readr/raw/master/inst/extdata/mtcars.csv")

# alternate way - explicitly downloads the file via HTTP and gives contents to the read_csv function
mtcars_df2 <- read_csv(content(httr::GET("https://github.com/tidyverse/readr/raw/master/inst/extdata/mtcars.csv")))

# excel file (only option is to download the file to hard disk and load it afterwards)
# you can prevent clutter of random excel files in folder by using a tempfile
httr::GET("https://github.com/Hamleyburger/Django_FecobiomeInitiative/blob/48d3eb5b6a90f2d057a08ea49ae9070d9d268456/media/admin/examples/data_template.xlsx?raw=true", 
          write_disk(tf <- tempfile(fileext = ".xlsx")))
excel_df <- readxl::read_excel(tf)


# --------- making data inline -------------
# a tribble is useful for testing or making small reference tables for reference/cleaning
# this can be done in excel as well but there is the option to do it in the script as well
data <- tribble(
  ~colA, ~colB,
  "a",   1,
  "b",   2,
  "c",   3
)


# --------- converting to tidy -------------
# tidyr (pivot_longer, pivot_wider)

relig_income %>%
  pivot_longer(!religion, names_to = "income", values_to = "count")

fish_encounters %>%
  pivot_wider(names_from = station, values_from = seen)

# --------- string manipulation ------------
# stringr
test_strings <- c("the quick brown $42 dog", "jumped over", "the lazy $77 fox")

str_to_upper(test_strings)
str_to_sentence(test_strings)

str_detect(test_strings, "\\$\\d+")
str_extract(test_strings, "\\$\\d+")

str_split(test_strings, pattern = " ")
map_int(str_split(test_strings, pattern = " "), length)


# glue (string interpolation)
name <- "Rabeez"

# old style using paste for string concatenation
paste("My name is ", name, ". Nice to meet you.", sep = "")

# kinda modern way with string interpolation
glue::glue("My name is {name}. Nice to meet you")

# --------- factor manipulation ------------
# forcats
fct_count(diamonds$clarity)

fct_collapse(diamonds$clarity,
             good = c("IF", "VVS1", "VVS2"),
             medium = c("VS1", "VS2"),
             bad = c("SI1", "SI2", "I1")) %>% 
  fct_count()

# reorder factors based on frequency
fct_infreq(diamonds$clarity) %>% 
  fct_count()
# reorder based on a function on a separate column/vector
# sort the factors in calrity based on median price of that category
fct_reorder(diamonds$clarity, diamonds$price, median) %>% 
  fct_count()

# groups bottom few categories into "Other" based on a criteria
fct_lump_n(diamonds$clarity, n = 3) %>% 
  fct_count()

# --------- data/time manipulation ---------
# lubridate

test_dates <- c("09-01-01", "09-01-02", "09-01-03")

# similar functions exist for other orderings as well
converted_dates <- lubridate::ymd(test_dates)

converted_dates %>% 
  lubridate::day()

converted_dates %>% 
  lubridate::ceiling_date(unit = "month")

converted_dates %>% 
  lubridate::leap_year()

converted_dates %>% 
  lubridate::wday(label = T)

lubridate::hms("14:44:32") %>% 
  lubridate::am()

# functions to make and manipulate time durations and intervals also present

# --------- nested dataframes --------------
# tidyr (nest/unnest)

diamonds %>% 
  group_by(color) %>% 
  nest() %>% 
  mutate(num_diamonds = map(data, nrow)) %>% 
  unnest(num_diamonds) 


# real example: Suppose I want to figure out which are the most commonly answered languages but each person can answer with multiple languages
mcq_data <- tribble(
  ~srno, ~ans,
  1, "English,Spanish",
  2, "English",
  3, "Spanish,Mexican",
  4, "Chinese,English",
  5, "Japanese,English,Korean",
  6, "English",
  7, "English,Spanish",
  8, "English",
)

# this is obviously not going to work
mcq_data %>% 
  pull(ans) %>% 
  fct_count(sort = T)

# we have to first split the strings into list objects and then unnest those objects into separate rows before counting
mcq_data %>% 
  mutate(ans_split = str_split(ans, pattern = ",")) %>% 
  unnest(ans_split) %>% 
  pull(ans_split) %>% 
  fct_count(sort = T)

# --------- other tidyverse packages -------
# other tidyverse (https://www.tidyverse.org/packages/)

skimr::skim(diamonds)


# --------- ggplot extensions --------------
# ggplot extensions (https://exts.ggplot2.tidyverse.org/)

# patchwork for subplots
library(patchwork)

p1 <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) + geom_point() + theme(legend.position = "none")
p2 <- ggplot(mtcars, aes(factor(cyl), fill = factor(cyl))) + geom_bar()
p1 + p2


# plotly for simple interactive plots
p <- ggplot(gapminder::gapminder, aes(gdpPercap, lifeExp)) + 
  geom_point(alpha = 0.7)
p

library(plotly)
ggplotly(p)


# gganimate for gifs/videos
library(gganimate)

ggplot(gapminder::gapminder, aes(gdpPercap, lifeExp)) + 
  geom_point(alpha = 0.7) + 
  facet_wrap(~continent) + 
  scale_x_log10(labels = scales::dollar_format()) + 
  transition_time(year) + 
  labs(title = 'Year: {frame_time}')


# ggrepel for labelled scatter plots
as_tibble(mtcars, rownames = "car") %>%
  ggplot(aes(wt, mpg, label = car)) +
  geom_point() + 
  geom_text()

as_tibble(mtcars, rownames = "car") %>%
  ggplot(aes(wt, mpg, label = car)) +
  geom_point() + 
  ggrepel::geom_text_repel()


# gghighlight for storytelling
as_tibble(mtcars, rownames = "car") %>%
  ggplot(aes(wt, mpg, label = car)) +
  geom_point(color = "red") + 
  gghighlight::gghighlight((gear == 3) & (mpg > 20), 
                           label_key = car,
                           use_direct_label = FALSE, 
                           unhighlighted_params = list(color = alpha("black", 0.5))) + 
  ggrepel::geom_text_repel() +
  labs(title = "The only 3-gear cars with more than 20 miles/gallon")


# --------- plotting on maps ---------------
# gps map (stamen, ggmap)
crime_df <- tibble(ggmap::crime) %>% 
  filter(between(lat, 29.73631, 29.78400),
         between(lon, -95.39681, -95.34188))

ggmap::qmplot(lon, lat, data = crime_df, maptype = "toner-lite", color = I("red"))
ggmap::qmplot(lon, lat, data = crime_df, maptype = "toner-lite", geom = "density2d", color = I("red"))

crime_df %>%
  ggmap::qmplot(lon, lat, data = ., 
                maptype = "toner-background", geom = "blank", darken = .7) + 
  stat_density_2d(aes(fill = ..level..), geom="polygon", alpha = 0.3) + 
  scale_fill_gradient2("Crime level", low = "blue", mid = "pink", high = "red", midpoint = 1500, limits = c(0, 3000))


# --------- plotting geographical regions ----
# shapefiles (sf)

# tigris package has lots of geographical info for the US
continential_us <- tigris::states() %>% 
  filter(REGION != 9,
         !(NAME %in% c("Hawaii", "Alaska")))

# here I just plot the polygons using geom_sf
continential_us %>% 
  ggplot() + 
  geom_sf() + 
  ggthemes::theme_map()

# here I not only plot the landmarks as choropleth I add a manually created polygon for the state boundary
landmarks_ca <- tigris::landmarks(state = "CA", type = "area")
landmarks_ca %>% 
  filter(!is.na(FULLNAME)) %>% 
  arrange(desc(ALAND + AWATER)) %>% 
  head(50) %>% 
  ggplot(aes(fill = AWATER)) + 
  geom_polygon(data = map_data("state", "CA"), aes(long, lat), 
               fill = NA, color = "gray60") + 
  geom_sf(size = 0.1, color = "white") + 
  scale_fill_continuous("Area of water", labels = scales::comma_format()) + 
  ggthemes::theme_map() + 
  labs(title = "The 50 largest landmarks in California (CA) by total area (land + water) covered")


# --------- network analysis ---------------
# networks

library(tidygraph) # provides the core data structure and graph theory algorithms
library(ggraph) # ggplot extension to plot network diagrams

# create a random network
network <- tidygraph::play_erdos_renyi(10, 0.5) 

# plot it in default layout
network %>% 
  ggraph::ggraph() + 
  ggraph::geom_edge_link() + 
  ggraph::geom_node_point(color = "blue")

# apply dplyr-style manipulation to the network nodes and/or edges before plotting using familiar ggplot features
network %>% 
  tidygraph::activate(nodes) %>% 
  mutate(degree = tidygraph::centrality_degree()) %>% 
  ggraph::ggraph() + 
  ggraph::geom_edge_link() + 
  ggraph::geom_node_point(aes(color = degree), size = 5)

# --------- funnel analysis ----------------
# This is about analyzing funnel processes. For example imagine users landing on your homepage and then only a few of them will register with you
# They can land and register multiple times in no particular order
# You want to identify people who follow a certain 'journey'. Maybe only the ones who register the first time they visit you
# This package simplifies the joining/filtering operations needed to work with data for these kinds of processes
library(funneljoin)

# this table contains entries for people who landed on your webpage
funneljoin::landed

# this table contains entries for people who registered (this is a subset of landed)
funneljoin::landed

# 1. Choose users: This is an AFTER inner join, so only the users who registered after landing are included in result (same day does not count)
# 2. For selected users choose timestamps: The chosen 'type' means that in the result for each user we want the first landing time and 
# the first registration time after the chosen landing
funneljoin::landed %>%
  funneljoin::after_inner_join(funneljoin::registered, 
                               by_user = "user_id", by_time = "timestamp",
                               type = "first-firstafter",
                               suffix = c("_landed", "_registered"))

# Of course there are other join functions and types available and these can be chained to create a longer funnel with more conversion steps

