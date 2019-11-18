library(forecast)
library(tidyverse)
get_rand_seas <- function() {
  (forecast::fourier(ts(c(1:36), freq = 12), 2)) %>% rowSums() %>% `*`(30) %>% `+`(90) %>% `+`(rnorm(36, 0, 15))
  
}
actual_pattern <- (forecast::fourier(ts(c(1:36), freq = 12), 2)) %>%
  rowSums() %>%
  `*`(30) %>%
  `+`(90)
act_df <- tibble(week = 1:36, act = actual_pattern)

seas_matrix <- map(1:20, ~get_rand_seas()) %>% bind_cols()


z <- svd(seas_matrix %>% as.matrix, nu = 6, nv = 6)

s <- diag(z$d[1:6]) #Takes 6 largest components
s
#reconstruct
rec <- (z$u %*% s %*% t(z$v) %>% as.data.frame())

trunc_svd <- function(data_matrix, components) {
  z <- svd(data_matrix, nu = components, nv = components)
  s <- diag(z$d[1:components], components, components) #Takes 6 largest components
  rec <- (z$u %*% s %*% t(z$v) %>% as.data.frame()) %>%
    mutate(week = row_number()) %>%
    gather(key = store, value = sales, -week)
  return(rec)
}

1:20 %>% map_dfr(~trunc_svd(seas_matrix %>% as.matrix, .x) %>% mutate(comp = .x)) %>%
  group_by(comp) %>%
  summarize(var = var(sales)) %>%
  mutate(pct_var = var/max(var))

rec %>%
  mutate(week = row_number()) %>%
  gather(key = store, value = sales, -week) %>%
  ggplot(aes(x = week, y = sales, color = store)) + geom_line()

sales_tidy <- seas_matrix %>%
  mutate(week = row_number()) %>%
  gather(key = store, value = sales, -week) %>%
  mutate(type = "og")

rec %>%
  mutate(week = row_number()) %>%
  gather(key = store, value = sales, -week) %>%
  mutate(type = "rec") %>%
  bind_rows(sales_tidy) %>%
  left_join(act_df) %>%
  ggplot(aes(x = week, y = sales, color = type)) + geom_line() + geom_line(aes(y = act, color = "act")) + facet_wrap(~store)

rec %>%
  mutate(week = row_number()) %>%
  gather(key = store, value = sales, -week) %>%
  mutate(type = "rec") %>%
  bind_rows(sales_tidy) %>%
  left_join(act_df) %>%
  mutate(error = act - sales) %>%
  group_by(type) %>%
  summarize(mean(abs(error)))


rec %>%
  mutate(week = row_number()) %>%
  gather(key = store, value = sales, -week) %>%
  mutate(type = "rec") %>%
  bind_rows(sales_tidy) %>%
  group_by(type) %>%
  summarize(var = var(sales))
