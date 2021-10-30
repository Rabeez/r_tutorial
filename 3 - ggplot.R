library(tidyverse) # ggplot2 is loaded as part of tidyverse


# grammar of graphics
# - abstraction pyramid
# - layering (adding on plot)
# - link with tidy data

# empty plot
ggplot(data = diamonds) 

# alternative syntax
diamonds %>% ggplot() 

# set the aesthetics
diamonds %>% 
  ggplot(mapping = aes(x = carat, y = price)) 

# specify a geometry
diamonds %>%
  ggplot(aes(x = carat, y = price)) + 
  geom_point()

# control over aesthetics argument (identical result)
diamonds %>%
  ggplot() +
  geom_point(aes(x = carat, y = price))

# specifying a different geometry results in a different representation of the SAME data
diamonds %>%
  ggplot(aes(x = carat, y = price)) +
  stat_bin2d()

# small multiples / faceting (treat this as an aesthetic as well)
ggplot(data = diamonds, aes(x = carat, y = price)) + 
  geom_point() + 
  facet_wrap(~color)

# customize the geometry, tick labels and axis limits
diamonds %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point(alpha = 0.2, color = "coral") + 
  scale_x_continuous(breaks = seq(0,5,0.5)) + 
  coord_cartesian(xlim = c(0, 3))

# categorical axis and overplotting
diamonds %>%
  ggplot(aes(x = clarity, y = price)) +
  geom_point()

diamonds %>%
  ggplot(aes(x = clarity, y = price)) +
  geom_jitter()

# layering geometries (order matters)
diamonds %>%
  ggplot(aes(x = clarity, y = price)) +
  geom_boxplot() + 
  geom_jitter(alpha = 0.1, size = 0.5, color = "coral")

# also the aesthetics are inherited so they MUST be in the ggplot call and not in the geometries otherwise they will have to be repeated
diamonds %>%
  ggplot() +
  geom_boxplot(aes(x = clarity, y = price)) + 
  geom_jitter(aes(x = clarity, y = price), alpha = 0.1, size = 0.5, color = "coral")


# applying a scale
diamonds %>%
  ggplot(aes(x = price)) +
  geom_histogram() + 
  scale_x_log10()

# applying a more complex scale along with dedicated data transformations
diamonds %>%
  mutate(volume = x * y * z) %>% 
  filter(volume <= 600) %>% 
  arrange(depth) %>%
  # arrange(desc(depth)) %>%
  ggplot(aes(x = volume, y = price, color = depth)) +
  geom_point() + 
  scale_color_gradient2(low = "red", mid = "pink", high = "blue", midpoint = median(diamonds$depth))


# bespoke visualization with professional labeling and format
diamonds %>%
  ggplot(aes(x = price)) +
  geom_histogram(aes(y = ..density..), color = "white", alpha = 0.2) +
  geom_density(size = 1) +
  geom_rug(size = 0.05, alpha = 0.05) +
  scale_x_log10(labels = scales::dollar_format()) + 
  labs(x = "Price", y = "Density", 
       title = "What is the price of diamonds?", 
       subtitle = "Price plotted on a log scale", 
       caption= "Source: tidyverse sample data") + 
  theme_minimal()


# --------------- Homework --------------------

# hypothesis: the dependence of price on carat goes down as we go up the colors
# translation: the slope of best fit line (y=price, x=carat) decreases for higher colors

# visualization that gave this hypothesis idea (repeated from above)
ggplot(data = diamonds, aes(x = carat, y = price)) + 
  geom_point() + 
  facet_wrap(~color)

# modified visualization to reinforce hypothesis
# output: looks like hypothesis might be true visually
ggplot(data = diamonds, aes(x = carat, y = price, color = color)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth(method = "lm")


# --------------- Bonus showcase (you're not supposed to understand the code here) --------------------

# Proper verification of the hypothesis by fitting separate linear models for each Color
# and comparing how the slope of fitted line changes for different Colors
# weighted regression is used because J color has MUCH fewer diamonds
diamonds %>% 
  group_by(color) %>% 
  nest() %>% 
  mutate(amount = map(data, nrow)) %>% 
  unnest(amount) %>% 
  mutate(linear_model = map(data, function(df) lm(price ~ carat, data = df))) %>%
  mutate(model_params = map(linear_model, broom::tidy)) %>%
  unnest(model_params) %>%
  filter(term == "carat") %>%
  ggplot(aes(x = color, y = estimate, size = amount)) +
  geom_point() + 
  geom_smooth(aes(weight = amount, group = 1), method = "lm", show.legend = F) + 
  labs(x = "Color", y = "Slope (Price ~ Carat)", size = "Number of points",
       title = "Price is less proportional to Carat for higher Colors")

