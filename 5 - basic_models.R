library(tidyverse)
library(patchwork) # subplots
library(gapminder) # dataset
library(broom) # tidy and glance functions
library(glmnet) # regularized models
library(glmnetUtils) # formula (recommended) interface to glmnet

# ----- basic linear models -----

# FUNTIONAL INTERPRETATION OF MODELS
# These models assume a linear relationship of the form
# y = m*x + c        where m is the slope and c is the y-intercept and x is independent variable, y is dependent variable
# x is also called the feature/regressor and y is also called the target/response
# The model assumes a directly proportional relationship with m being the proportionality constant and c being the offset

# We can have multiple variables involved in the regression (multiple linear regression)
# In such a model the assumption is that the target is proportional to all variables (with different amount) with a single intercept

# There appears to be a linear trend between log(population) and life expectancy
gapminder %>%
  ggplot(aes(x = pop, y = lifeExp)) +  
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::label_comma())

# This model formula specifies the relationship mentioned above
model <- lm(formula = lifeExp ~ log10(pop), data = gapminder)

# We can explore the model using these functions
# Notice the p-values next to the estimates
# Regressions are actually statistical tests so the p-value of the slope tells the significance of the proportionality 
summary(model)
glance(model)
tidy(model)

# to plot residuals (good best-fit lines have randomly distributed residual plots. With no discernible pattern)
plot(resid(model))

# A common practice is to plot the fitted coefficients (more useful with multiple regression)
tidy(model, conf.int = T) %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_point()

# We can ask the fitted model for predictions like this
prediction_tbl <- tibble(pop = seq(1000000,10000000,100))
prediction_tbl %>% 
  mutate(prediction = predict(model, prediction_tbl)) %>% 
  ggplot(aes(pop, prediction)) + 
  geom_point(data = gapminder, mapping = aes(x = pop, y = lifeExp)) + 
  geom_line(color = "red", size = 2) + 
  scale_x_log10(labels = scales::label_comma())


## ------- Including interaction effects -------
# Plotting life expectancy against both variables shows a trend
p1 <- gapminder %>%
  ggplot(aes(x = pop, y = lifeExp)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::label_comma())
p2 <- gapminder %>%
  ggplot(aes(x = year, y = lifeExp)) + 
  geom_point() +
  geom_smooth(method = "lm")
(p1 + p2) & coord_cartesian(ylim = c(20, 90))

# so let's make a multiple regression
model_multi <- lm(lifeExp ~ log10(pop) + year, data = gapminder)
summary(model_multi)

p3 <- gapminder %>%
  ggplot(aes(x = pop, y = year)) + 
  geom_point(aes(color = lifeExp)) + 
  geom_smooth(method = "lm") + 
  scale_x_log10(labels = scales::label_comma())
(p1 + p2) / p3

# basically, accounting for year the significance of log(pop) is greatly reduced
model_inter <- lm(lifeExp ~ log10(pop) * year, data = gapminder)
summary(model_inter)


## ------- Including polynomial features (polynomial regression) -----
gapminder %>%
  # ggplot(aes(x = pop, y = lifeExp)) + 
  ggplot(aes(x = lifeExp, y = gdpPercap)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = T), color = "red") #+
  # scale_y_log10()

# model_a <- lm(lifeExp ~ pop, data = gapminder)
model_a <- lm(gdpPercap ~ lifeExp, data = gapminder)
summary(model_a)
model_b <- lm(gdpPercap ~ poly(lifeExp, 2, raw = T), data = gapminder)
summary(model_b)


## ------- Including categorical/factor features ------
# There seems to be huge difference in the slopes for different continents
gapminder %>%
  ggplot(aes(x = pop, y = lifeExp, color = continent)) + 
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::label_comma()) + 
  labs(
    title = "Varying slope+intercept model",
    subtitle = "Could be thought of as 5 separate models"
  )

# Note: This model spec does to represent the plot above (will see why later)
# This model spec is going to fit a slope for the numeric variable (same as before) and single offsets for each of the values in the categories
# Meaning we are asking for separate lines for each continent but forcing them to be parallel
# Note: If categories are represented by integers then an explicit factor conversion is needed in the formula (true for all models basically)
model2 <- lm(lifeExp ~ log10(pop) + continent, data = gapminder)

# Similar to before we can inspect the model
summary(model2)
glance(model2)
tidy(model2)

# This code combines the tidy results of both models and makes a comparison plot
tidy(model, conf.int = T) %>% 
  inner_join(tidy(model2, conf.int = T), by = "term", suffix = c("_model", "_model2")) %>% 
  select(term, starts_with("estimate"), starts_with("std"), starts_with("conf")) %>% 
  pivot_longer(cols = -term, names_to = c("quantity", "model"), names_sep = "_") %>% 
  pivot_wider(names_from = quantity, values_from = value) %>% 
  ggplot(aes(x = model, y = estimate, color = model)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_point() + 
  facet_wrap(~term, scales = "free") + 
  theme(legend.position = "none") + 
  labs(title = "Accounting for continent effects greatly reduces the effect \nof population on life expectancy")

# Let's try asking for predictions by making new data
prediction_tbl2 <- tibble(crossing(pop = seq(1000000,10000000,100), continent = as_factor(levels(gapminder$continent)))) 
prediction_tbl2 <- prediction_tbl2 %>% 
  rename_all(function(temp) c("pop", "continent")) %>% 
  mutate(prediction = predict(model2, prediction_tbl2))
# This plot demonstrates the limitation of the assumptions made with this model
# Notice how the slope of all lines is set to the slope for Africa (the first category provided to the model since default was alphabetical)
prediction_tbl2 %>% 
  ggplot(aes(x = pop, y = prediction, color = continent)) + 
  geom_smooth(data = gapminder, mapping = aes(x = pop, y = lifeExp, color = continent), method = "lm", se = F) + 
  geom_line(linetype = "dashed") + 
  scale_x_log10(labels = scales::label_comma()) + 
  labs(
    title = "Comparison of 2 types of models with same variables",
    subtitle = "Solid lines = Varying slope+intercept (same as previous plot) | Made using geom_smooth\nDashed lines = Varying intercept + Fixed slope | Made using lm"
  )


## ------- Varying slope (interaction with a factor) ------
# This model relaxes the assumption of fixed slope for all continents
model3 <- lm(lifeExp ~ log10(pop) * continent, data = gapminder)
# model3 <- lm(lifeExp ~ log10(pop) + continent + log10(pop):continent, data = gapminder) # equivalent but more verbose formula
summary(model3)
glance(model3)
tidy(model3)

# The predictions for this interaction model match the result from the original geom_smooth with color aesthetic (varying slope+intercept)
prediction_tbl3 <- tibble(crossing(pop = seq(1000000,10000000,100), continent = as_factor(levels(gapminder$continent)))) 
prediction_tbl3 <- prediction_tbl3 %>% 
  rename_all(function(temp) c("pop", "continent")) %>% 
  mutate(prediction = predict(model3, prediction_tbl3))
prediction_tbl3 %>% 
  ggplot(aes(x = pop, y = prediction, color = continent)) + 
  geom_smooth(data = gapminder, mapping = aes(x = pop, y = lifeExp, color = continent), method = "lm", se = F) + 
  geom_line(linetype = "dashed") + 
  scale_x_log10(labels = scales::label_comma()) + 
  labs(
    title = "Comparison of 2 types of models with same variables",
    subtitle = "Solid lines = Varying slope+intercept | Made using geom_smooth\nDashed lines = Varying slope+intercept | Made using lm"
  )

# Let's explicitly compare both versions of the model
prediction_merged <- prediction_tbl2 %>% 
  inner_join(prediction_tbl3, by = c("pop", "continent"), suffix = c(".fixed_slope", ".varying_slope")) %>% 
  pivot_longer(cols = -c("pop", "continent"), values_to = "prediction") %>% 
  mutate(name = str_replace(name, "prediction.", "")) 
prediction_merged %>% 
  ggplot(aes(x = pop, y = prediction, color = continent)) + 
  geom_line() + 
  scale_x_log10(labels = scales::label_comma()) + 
  facet_wrap(~name) + 
  labs(
    title = "Comparing both models fitted using lm"
  )


# ----- regularized linear models -----
# These are the preferred method if the goal is to learn a general model for prediction
# Basic idea is to penalize large coefficient values to prevent overfitting
# bias/variance tradeoff (google images)


# Runs a cross-validated lasso (alpha = 1) regression
model_lasso <- cv.glmnet(formula = lifeExp ~ log10(pop) + continent, data = gapminder, alpha = 1)
plot(model_lasso)

# Returns coefficients for the 'best' lambda parameter
coef(model_lasso)


# If predictive modelling is the only objective then rather than the statistical modelling functionality (lm/glm etc)
# it will be better to use packages which are designed to be used for prediction (look up tidymodels set of packages)

# ----- generalized linear models -----
# Mathematical generalization of regression concept
# STATISTICAL INTERPRETATION OF MODELS
# The models assume that the target Y is a random variable obeying some distribution (usually gaussian)
# This RV is determined by Y = h + noise where h is the unobservable 'true' signal and we only see Y 
# which has noise (measurement, inherent, systemic, etc) added to it. 
# For the gaussian model we model Y ~ N(mX+c, sigma) where m is the slope and c is the y-intercept, X is independent random variable and sigma is the noise component
# The expression mu = mX+c is a 'link function' because we are 'linking' a parameter of the target distribution as a function of the regressor RV

# ALL statistical tests are specific forms of this framework
# GLM = formula + family + link-function

# gaussian family (default link = linear) is equivalent to plain-old linear regression
model_glm <- glm(formula = lifeExp ~ log10(pop), data = gapminder, family = "gaussian")
tidy(model_glm)
tidy(model) # compare with original model fitted using `lm`


## ------- poisson glm ------
# appropriate for count/rate data
# default link function = log
insurance <- tibble(MASS::Insurance)

insurance %>% 
  ggplot(aes(x = Holders, y = Claims)) + 
  geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "gaussian"), color = "blue") + 
  geom_smooth(method = "glm", method.args = list(family = "poisson"), color = "red") + 
  scale_x_log10() + 
  labs(
    title = "Clearly the Poisson model fits this data much better"
  )

model_claims_gaussian <- glm(formula = Claims ~ log10(Holders), data = insurance, family = "gaussian")
tidy(model_claims_gaussian)

model_claims_poisson <- glm(formula = Claims ~ log10(Holders), data = insurance, family = "poisson")
# exponentiated coefficient is the %age increase
tidy(model_claims_poisson, exponentiate = T)


## ------- logistic glm -------
# appropriate for probability/binary data
# default link function = logit
breast_data <- tibble(MASS::biopsy)

breast_data %>% 
  pivot_longer(cols = V1:V9) %>% 
  ggplot(aes(value, class)) + 
  geom_jitter(alpha = 0.1) + 
  facet_wrap(~name)

model_breast <- glm(formula = class ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9, 
                    data = breast_data, family = "binomial")
summary(model_breast)
# plain coefficient is additive increase in log odds
# exponentiated coefficient is multiplicative increase in odds
# probability = odds / (1- odds)
model_estimates <- tidy(model_breast, exponentiate = T, conf.int = T)

# Plot compares effects of variables on the target
p1 <- model_estimates %>% 
  filter(term != "(Intercept)") %>% 
  mutate(estimate = estimate - 1,
         conf.low = conf.low - 1,
         conf.high = conf.high - 1) %>% 
  ggplot(aes(x = term, y = estimate, color = conf.low < -0.1)) + 
  geom_abline(intercept = 0, slope = 0, color = "gray69") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_point() + 
  scale_color_manual(values = c("green3", "red2"), guide = F) +
  scale_y_continuous(labels = scales::label_percent()) + 
  labs(title = "Change is odds of being diagnosed with breast cancer \nfor a unit increase in each indicator")
p1

# ----- anova (explained variance) -----
p2 <- car::Anova(model_breast, type = 3) %>% 
  tidy() %>% 
  mutate(explained_deviance = statistic / sum(statistic)) %>% 
  ggplot(aes(x = term, y = explained_deviance)) + 
  geom_segment(aes(xend = term, y = 0, yend = explained_deviance)) + 
  geom_point(size = 3) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  labs(
    title = "%age of variance in target explained by each regressor"
  )
p2

# Notice how the variables with high AND significant effect on output have higher explained variance
p1 + p2
