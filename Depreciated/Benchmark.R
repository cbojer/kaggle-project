# Load Packages ----------------------------------------------------------------------------------------------------------------
if(!"tidyverse" %in% installed.packages()) {
  install.packages("tidyverse")
} 

if(!"lubridate" %in% installed.packages()) {
  install.packages("lubridate")
} 

if(!"data.table" %in% installed.packages()) {
  install.packages("data.table")
} 


library(tidyverse)
library(lubridate)
library(data.table)

# Forecasts for Rossmann -------------------------------------------------------------------------------------------------------

# Load data
train <- fread("~/kaggle/rossmann/train.csv") %>% mutate(Date = ymd(Date))
test <- fread("~/kaggle/rossmann/test.csv") %>% mutate(Date = ymd(Date))

# Naive
naive <- train %>% group_by(Store) %>% top_n(n = 1, wt = Date) %>% select(Store, Sales)

test %>%
  left_join(naive, by = c("Store")) %>%
  mutate(
    Open = replace_na(Open, 1L),
    Sales = if_else(Open == 0, 0L, Sales)
  ) %>%
  select(Id, Sales) %>% 
  write.csv("Results/Forecasts/rossmann_naive_by_store.csv", row.names = F)

# Seasonal Naive
seasonal_naive <- train %>% group_by(Store, DayOfWeek) %>% top_n(n = 1, wt = Date) %>% select(Store, DayOfWeek, Sales)

test %>%
  left_join(seasonal_naive, by = c("Store", "DayOfWeek")) %>%
  mutate(
    Open = replace_na(Open, 1L),
    Sales = if_else(Open == 0, 0L, Sales)
  ) %>%
  select(Id, Sales) %>% 
  write.csv("Results/Forecasts/rossmann_seasonal_naive_by_store.csv", row.names = F)

rm(train, test, naive, seasonal_naive)
gc()

# Forecasts for Walmart Store Sales ---------------------------------------------------------------------------------------------

# Load data
train <- fread("~/kaggle/walmart/train.csv") %>% mutate(Date = ymd(Date))
test <- fread("~/kaggle/walmart/test.csv") %>% mutate(Date = ymd(Date))

# Fallbacks
backup_dept_mean <- train %>% group_by(Dept) %>% summarize(Dept_mean = mean(Weekly_Sales, na.rm = TRUE))

# Naive
naive <- train %>% group_by(Store, Dept) %>% top_n(n = 1, wt = Date) %>% select(Store, Dept, Weekly_Sales)

test %>%
  left_join(naive, by = c("Store", "Dept")) %>%
  left_join(backup_dept_mean, by = c("Dept")) %>%
  mutate(
    Weekly_Sales = if_else(is.na(Weekly_Sales), Dept_mean, Weekly_Sales),
    Id = str_c(Store, Dept, Date, sep = "_")
  ) %>%
  select(Id, Weekly_Sales) %>% 
  write.csv("Results/Forecasts/walmart_naive_by_store_dept.csv", row.names = F)

# Seasonal Naive
week_aligner <- train %>%
  bind_rows(test %>% mutate(test_set = TRUE)) %>%
  mutate(Week = week(Date)) %>% 
  distinct(year = year(Date), Week, IsHoliday, test_set) %>% 
  mutate(match_week = lag(Week, 52)) %>% 
  filter(test_set) %>% 
  select(Week, match_week)

seasonal_naive_dataset <- train %>% 
  mutate(Week = week(Date), Year = year(Date)) %>%
  filter(Year >= 2011, ! (Week < 45 & Year == 2011))

seasonal_naive <- seasonal_naive_dataset %>% select(Store, Dept, Week, Year = Year, Weekly_Sales)

preds <- test %>% 
  mutate(Week = week(Date)) %>% 
  left_join(week_aligner, by = "Week") %>% 
  left_join(seasonal_naive, by = c("Store", "Dept", "match_week" = "Week")) %>%
  left_join(backup_dept_mean, by = c("Dept")) %>%
  mutate(
    Weekly_Sales = if_else(is.na(Weekly_Sales), Dept_mean, Weekly_Sales),
    Id = str_c(Store, Dept, Date, sep = "_")
  ) 

preds %>% select(Id, Weekly_Sales) %>% write.csv("Results/Forecasts/walmart_seasonal_naive_by_store_dept.csv", row.names = F)

rm(train, test, backup_dept_mean, week_aligner, seasonal_naive_dataset, seasonal_naive, preds)
gc()

# Forecasts for Corporacion Favorita  --------------------------------------------------------------------------------------------

# Load data
train <- fread("~/kaggle/favorita/train.csv") %>% mutate(date = ymd(date))
test <- fread("~/kaggle/favorita/test.csv") %>% mutate(date = ymd(date))

# Fallbacks
fallback_store_item_mean <- train %>% group_by(store_nbr, item_nbr) %>% summarize(store_item_mean = mean(unit_sales, na.rm = TRUE))
fallback_item_mean <- train %>% group_by(item_nbr) %>% summarize(item_mean = mean(unit_sales, na.rm = TRUE))
fallback_store_mean <- train %>% group_by(store_nbr) %>% summarize(store_mean = mean(unit_sales, na.rm = TRUE))

# Naive
naive <- train %>% 
  group_by(store_nbr, item_nbr) %>% 
  top_n(n = 1, wt = date)%>%
  select(store_nbr, item_nbr, unit_sales)

preds <- test %>% 
  left_join(naive, by = c("store_nbr", "item_nbr")) %>% 
  left_join(fallback_item_mean, by = c("item_nbr")) %>% 
  left_join(fallback_store_mean, by = c("store_nbr")) %>% 
  mutate(
    unit_sales = if_else(is.na(unit_sales), item_mean, unit_sales),
    unit_sales = if_else(is.na(unit_sales), store_mean, unit_sales),
    unit_sales = if_else(unit_sales < 0, 0, unit_sales)
  ) %>% 
  ungroup

preds %>% select(id, unit_sales) %>% write.csv("Results/Forecasts/favorita_naive_by_store_item.csv", row.names = F)

# Seasonal Naive
seasonal_naive <- train %>% 
  mutate(dow = wday(date)) %>% 
  group_by(store_nbr, item_nbr, dow) %>% 
  top_n(n = 1, wt = date) %>%
  select(store_nbr, item_nbr, dow, unit_sales)

seasonal_naive_preds <- test %>% 
  mutate(dow = wday(date)) %>% 
  left_join(seasonal_naive, by = c("store_nbr", "item_nbr", "dow")) %>% 
  left_join(fallback_store_item_mean, by = c("store_nbr", "item_nbr")) %>% 
  left_join(fallback_item_mean, by = c("item_nbr")) %>% 
  left_join(fallback_store_mean, by = c("store_nbr")) %>% 
  mutate(
    unit_sales = if_else(is.na(unit_sales), store_item_mean, unit_sales),
    unit_sales = if_else(is.na(unit_sales), item_mean, unit_sales),
    unit_sales = if_else(is.na(unit_sales), store_mean, unit_sales),
    unit_sales = if_else(unit_sales < 0, 0, unit_sales)
  ) %>%
  ungroup

seasonal_naive_preds %>% select(id, unit_sales) %>% write.csv("Results/Forecasts/favorita_seasonal_naive_by_store_item.csv", row.names = F)

rm(train, test, fallback_store_item_mean, fallback_item_mean, fallback_store_mean, naive, preds, seasonal_naive, seasonal_naive_preds)
gc()

# Forecasts for Wikipedia  -------------------------------------------------------------------------------------------------------

# Load data
train <- fread("~/kaggle/web-traffic-time-series-forecasting/train_2.csv") %>% 
  gather(key = date, value = visits, -Page) %>% 
  mutate(date = ymd(date))


keys <- fread("~/kaggle/web-traffic-time-series-forecasting/key_2.csv") %>% 
  mutate(date = str_sub(Page, -10, -1) %>% ymd,
         Page = str_sub(Page, 1, -12))

test <- fread("~/kaggle/web-traffic-time-series-forecasting/sample_submission_2.csv") %>% 
  left_join(keys, by = "Id") %>% 
  select(-Visits)

# Naive
naive_w_na <- train %>% filter(date == max(date))

naive_na_pages <- naive_w_na %>% filter(is.na(visits)) %>% pull(Page)

na_last_visits <- train %>%
  filter(Page %in% naive_na_pages) %>% 
  filter(!is.na(visits)) %>% 
  group_by(Page) %>%
  top_n(n = 1, wt = date)

naive_preds <- naive_w_na %>% 
  select(-date) %>% 
  left_join(na_last_visits %>% select(Page, last_visits = visits), by = "Page") %>%
  mutate(visits = if_else(is.na(visits), last_visits, visits),
         visits = if_else(is.na(visits), 0, visits) #zero if non existent in train
  ) %>%
  select(-last_visits)

test %>% 
  left_join(naive_preds, by = "Page") %>% 
  select(Id, Visits = visits) %>% 
  write.csv("Results/Forecasts/wiki_naive_by_page.csv", row.names = F)

# Seasonal Naive
snaive_w_na <- train %>% filter(date == max(date) - days(7))
snaive_na_pages <- snaive_w_na %>% filter(is.na(visits)) %>% pull(Page)

seas_na_last_visits <- train %>%
  filter(Page %in% snaive_na_pages) %>% 
  filter(!is.na(visits)) %>% 
  filter(wday(date) == wday(max(train$date))) %>% 
  group_by(Page) %>%
  top_n(n = 1, wt = date)

snaive_preds <- snaive_w_na %>% 
  select(-date) %>% 
  left_join(seas_na_last_visits %>% select(Page, last_visits = visits), by = "Page") %>%
  mutate(
    visits = if_else(is.na(visits), last_visits, visits),
    visits = if_else(is.na(visits), 0, visits) #zero if non existent in train
  ) %>%
  select(-last_visits)


test %>% 
  left_join(snaive_preds, by = "Page") %>% 
  select(Id, Visits = visits) %>% 
  write.csv("Results/Forecasts/wiki_seasonal_naive_by_page.csv", row.names = F)

rm(train, test, keys, naive_w_na, na_last_visits, naive_preds, snaive_w_na, snaive_na_pages, seas_na_last_visits, snaive_preds)
gc()

# Forecasts for Recruit Restaurant -----------------------------------------------------------------------------------------------

# Load data
train <- fread("~/kaggle/recruit-restaurant/air_visit_data.csv") %>% rename(date = visit_date) %>% mutate(date = ymd(date))
test <- fread("~/kaggle/recruit-restaurant/sample_submission.csv") %>%
  mutate(
    date = str_sub(id, -10, -1) %>% ymd(),
    air_store_id = str_sub(id, 1, -12)
  ) %>%
  select(-visitors)

# Fallbacks
fallback_store_mean <- train %>% 
  group_by(air_store_id) %>%
  summarize(store_mean = mean(visitors, na.rm = TRUE))

# Naive
naive <- train %>% 
  group_by(air_store_id) %>% 
  top_n(n = 1, wt = date) %>%
  select(-date)


test %>%
  left_join(naive, by = c("air_store_id")) %>%
  select(id, visitors) %>%
  write.csv("Results/Forecasts/recruit_naive_by_store.csv", row.names = FALSE)

# Seasonal Naive
snaive <- train %>% 
  mutate(dow = wday(date)) %>% 
  group_by(air_store_id, dow) %>% 
  top_n(n = 1, wt = date) %>%
  select(-date)

test %>% 
  mutate(dow = wday(date)) %>% 
  left_join(snaive, by = c("air_store_id", "dow")) %>% 
  left_join(fallback_store_mean, by = "air_store_id") %>% 
  mutate(
    visitors = if_else(is.na(visitors), store_mean, as.double(visitors))
  ) %>%
  select(id, visitors) %>%
  write.csv("Results/Forecasts/recruit_seasonal_naive_by_store.csv", row.names = FALSE)

rm(train, test, fallback_store_mean, naive, snaive)
gc()

# Forecasts for Walmart Stormy Weather --------------------------------------------------------------------------------------------

# Load data
train <- fread("~/kaggle/walmart-stormy/train.csv") %>% mutate(date = ymd(date))
test <- fread("~/kaggle/walmart-stormy/test.csv") %>% mutate(date = ymd(date))

# Naive
naive_preds <- train %>% 
  bind_rows(test) %>% 
  mutate(test = is.na(units)) %>%
  arrange(store_nbr, item_nbr, date) %>%
  group_by(store_nbr, item_nbr) %>%
  fill(units, .direction = "down") %>%
  filter(test) %>%
  select(date, store_nbr, item_nbr, units)

test %>% 
  left_join(naive_preds, by = c("store_nbr", "item_nbr", "date")) %>% 
  mutate(
    id = str_c(store_nbr, item_nbr, date, sep = "_")
  ) %>%
  select(id, units) %>%
  write.csv("Results/Forecasts/walmart_stormy_naive_by_store_item.csv", row.names = FALSE)

# Seasonal Naive
snaive_preds <- train %>% 
  bind_rows(test) %>% 
  mutate(
    test = is.na(units),
    dow = wday(date)
  ) %>%
  arrange(store_nbr, item_nbr, date) %>%
  group_by(store_nbr, item_nbr, dow) %>%
  fill(units, .direction = "down") %>%
  filter(test) %>%
  ungroup %>% 
  select(date, store_nbr, item_nbr, units)


test %>% 
  left_join(snaive_preds, by = c("store_nbr", "item_nbr", "date")) %>% 
  mutate(
    id = str_c(store_nbr, item_nbr, date, sep = "_")
  ) %>%
  select(id, units) %>%
  write.csv("Results/Forecasts/walmart_stormy_seasonal_naive_by_store_item.csv", row.names = FALSE)
