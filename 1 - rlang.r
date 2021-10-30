# making variables
x <- 42
y <- c(4, 6, 7.8)
z <- list(42, value = 56, other_value = c(4, "c"))
p <- 42 > 3


# inspect variables
class(x)
typeof(x)

typeof(z)
str(z)
attributes(z)
names(z)


# basic maths
4 + 5
11 / 5
11 %/% 5 # integer division
11 %% 5 # modulo


# collection type indexing
y[1]
z[[1]]
z[[2]]
z$value


# basic stats using base stats package
mean(y)
median(y)
sd(y)
IQR(y)
sin(y)


# load builtin dataset and inspect it
iris
typeof(iris)
class(iris)
dim(iris)
str(iris)


# basic plotting with base graphics
hist(iris$Sepal.Length)
boxplot(iris$Sepal.Length)

plot(iris$Sepal.Length, iris$Sepal.Width)
plot(iris$Sepal.Length, iris$Sepal.Width, type = "l")


# custom functions
is_outlier <- function(numbers) {
  # determines which of the values are outliers using a fixed threshold difference from the median
  
  cutoff <- median(numbers) + sd(numbers) * 2
  
  abs(numbers) > cutoff
}

is_outlier(y)
is_outlier(c(3,5,1,3,77))
