library(purrr)
# https://blog.rstudio.org/2016/01/06/purrr-0-2-0/

df <- data.frame(
  a = 1L,
  b = 1.5,
  y = Sys.time(),
  z = ordered(1)
)

df[1:4] %>% sapply(class) %>% str()
