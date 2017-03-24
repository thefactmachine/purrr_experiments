library(purrr)
# https://blog.rstudio.org/2016/01/06/purrr-0-2-0/

df <- data.frame(
  a = 1L,
  b = 1.5,
  y = Sys.time(),
  z = ordered(1)
)

df[1:4] %>% sapply(class) %>% str()

# clear the decks
rm(list=ls()) 
options(stringsAsFactors=FALSE)
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
# turn off scientific notation.
options(scipen = 999)


# https://blog.rstudio.org/2016/01/06/purrr-0-2-0/
df <- data.frame(
  a = 1L,
  b = 1.5,
  y = Sys.time(),
  z = ordered(1)
)

# these return different data types.
# sapply() tries to guess the appropriate data type
# vapply() takes an extra parameter which lets the programmer specify the data type
df[1:4] %>% sapply(class)
df[1:2] %>% sapply(class) 
df[3:4] %>% sapply(class)

# purrr has multiple functions, one for each common type of output:
# map_lgl(), map_int(), map_dbl(), map_chr(), and map_df()
# these produce the output .... OR ...throw and error....

x <- list(1, 3, 5)
y <- list(2, 4, 6)
# what is c here...?????
map2(x, y, c)

# using a formula -- result is a list
map2(x, y, ~.x + 2 + .y)

map2_dbl(x, y, `+`)

a <- list(1, 10, 100)
b <- list(1, 2, 3)
# returns a vector -- the formula needs to be .x & .y 
# these are passed by position.....
purrr::map2_dbl(a, b, ~ .x + .y)

# returns a list
map2(x, y, ~ .x + .y)

# flatten lists...
lst_ex <- list(1L, 2:3, 4L)
# this returns a list ... previously we had a list of 3 (with element 3 containing two elements) 
# now we have a list of 4 with each element containing a single element. 
# return type is a list
purrr::flatten(lst_ex)

purrr::flatten_int(lst_ex)

# list basics....
a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))

# extracts a vector - gets the primative data type
a[["a"]]
# extracts a list - gets the containing data type
a["a"] %>% class()
xx <- a["a"] 
# extracts a vector
xx$a
# extracts two lists
a[c("a", "b")]
# same as above....
a[1:2]

# from here:  http://r4ds.had.co.nz/lists.html
# from here:  http://r4ds.had.co.nz/lists.html



df_test <- data.frame(a = rnorm(5), b = rnorm(5), c = c("mark", "fred", "bill", "john", "sam"))
df_test[["a"]] %>% class()
df_test$a %>% class()
df_test[[c("a")]] %>% class()



# ==== function to recieve a function an compute an aggregate on a data.frame
fn_col_summary <- function(df, fun) {
  out <- vector("numeric", length(df))
  # produces a sequence of: 1,2,3 for 3 column data frame
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  } # for
  return(out)
} # function


df_test_numbers <- data.frame(a = rnorm(10), b = rnorm(10), c =  rnorm(10))
fn_col_summary(df_test_numbers, mean)
fn_col_summary(df_test_numbers, sum)

# MAP FUNCTIONS....
# map() returns a list.
# map_lgl() returns a logical vector.
# map_int() returns a integer vector.
# map_dbl() returns a double vector.
# map_chr() returns a character vector.
# map_df() returns a data frame.

x <- list(1, 2, 3)
purrr::map_int(x, length)

# the second argument .f, the function to apply,
# can be a formula, a character vector, or an integer vector
# Any arguments after will be passed on....

map_dbl(x, mean, trim = 0.5)

# splits the data frame up into 3 data.frames
list_car_test <- mtcars %>% split(.$cyl) 


models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))

# the above can be written using a one-side formula.
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))

# if you want to extract the R Squared.
# we run summary on the models and extract
# the component called r.squared.

models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared)
# this just does what the above does...
mtcars %>% split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .)) %>%
  map(summary) %>% 
  map_dbl(~.$r.squared)


# we can also use a numeric vector 
# to extract things by position
x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% map_dbl(2)



# notes about the apply functions....
# lapply is basically identical to map().  
# Iterates over a list and returns a list
# sapply() is a wrapper around lapply() and tries to simplify t
# the results and return a vector.  vapply() is like sapply()
# but allows a parameter that defines the return type()

# map_df(x, f)  is effectively the same as do.call("rbind", lapply(x, f))


lst_x <- list(c(1, 2, 3), c(4, 5, 6),  c(7, 8, 9))

fn_square <- function(x) {x * x}

do.call("rbind", lapply(lst_x, sqrt))


mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .x)) %>%
  map_df(~ as.data.frame(t(as.matrix(coef(.)))))




# (if you also want to preserve the variable names see
# the broom package)
library(jsonlite)

# from here:  http://r4ds.had.co.nz/lists.html

lst_issues <- jsonlite::fromJSON("issues.json", simplifyVector = FALSE)
# http://r4ds.had.co.nz/lists.html

# 30 issues
length(lst_issues)

# find out the structure of each of the 30 elements.
str(lst_issues[[1]])

lst_first_element <- lst_issues[[1]]

# splits out a vector of numbers.
lst_issues %>% map_int("id")

# map returns a list.
# structure of the dataset.  list_issues contains 30 elements.
# each of these elements contains 22 elements.  Of these 22 elements, 1 
# elemeent is a list with the name of "user".  This has 17 elements.
lst_users <- lst_issues %>% map("user")
length(lst_users) == 30

# INDEXING DEEPLY INTO THE HIERARCHY....

# so here is the fun part....we pass in a character vector
# with the names of the lists as they are in the hierarchy..
lst_issues %>% map_chr(c("user", "login"))
lst_issues %>% map_int(c("user", "id"))


# UPTO HERE....
# REMOVING A LEVEL OF A HIERARCHY
# http://r4ds.had.co.nz/lists.html





lst_users %>% map_chr("login")
# gets the login from the sublist user
lst_issues %>% map_chr(c("user", "login"))
length(issues)




# =======================================
#  https://github.com/hadley/purrr
# using base R "split()"
lst_split <- split(mtcars, mtcars$cyl)
names(lst_split)

lst_result <- mtcars %>% split(.$cyl) %>% map(~ lm(mpg ~ wt, data = .))

by_species <- iris %>% group_by(Species)

data(diamonds, package = "ggplot2")

# apply the as.character() function when the column is a factor
diamonds %>% map_if(is.factor, as.character) %>% str()

vct_to_scale <- c("x", "y", "z")

new_diamonds <- diamonds %>% map_at(vct_to_scale, scale)

# this is quite incredabiliy...!
cc <- mtcars %>% purrr::slice_rows("cyl") %>% by_slice(map, ~ .x / sum(.x))

# some good stuff here:
#  http://www.machinegurning.com/rstats/map_df/




