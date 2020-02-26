# Load Packages ----------------------------------------------------------------------------------------------------------------
if(!"dplyr" %in% installed.packages()) {
  install.packages("dplyr")
} 

if(!"readxl" %in% installed.packages()) {
  install.packages("readxl")
} 

if(!"tidyr" %in% installed.packages()) {
  install.packages("tidyr")
} 

if(!"purrr" %in% installed.packages()) {
  install.packages("purrr")
} 

library(dplyr)
library(readxl)
library(purrr)
library(tidyr)

# Load Data --------------------------------------------------------------------------------------------------------------------

# Load Leaderboard Data
lb_data <- map2_dfr(
  c("RossmannLB", "WikiLB", "RecruitLB", "FavoritaLB", "Walmart1LB", "Walmart2LB"),
  c("Rossmann", "Wikipedia", "Recruit", "Favorita", "Walmart1", "Walmart2"),
  ~read_excel(
    "Writings/Leaderboard.xlsx", 
    sheet = .x,
    col_names = c("place", "pub", "team", "notebook", "members", "score", "entries", "last"), 
    col_types = c("numeric", "text", "text", "text", "text", "numeric", "numeric", "text"),
    skip = 1, 
    ) %>% 
    mutate(comp = .y)
  ) %>% 
  mutate(
    score = if_else(comp == "Walmart1", score / 100000, score),
    score = if_else(comp == "Wikipedia", score / 100000, score)
  )

# Load Benchmark Performance (from Late Submissions to Kaggle)
benchmarks <- read_excel("Benchmarks/benchmarks-perf.xlsx") %>% rename(comp = CompId, score = Score) %>% mutate(place = NA)


# Relative Error -----------------------------------------------------------------------------------------------------------------

# Relative Error for 1st place and 25th place
top25 <- lb_data %>% 
  filter(place <= 25) %>% 
  group_by(comp) %>%
  mutate(pct_diff = (score - min(score))/min(score)*100)

top25 %>% 
  group_by(comp) %>% 
  mutate(rel_err = (score / max(score)) %>% round(digits = 2)) %>% 
  filter(place == 1) %>%
  select(Competition = comp, RelErr = rel_err) %>% 
  write.csv("Results/benchmarking_1st_25th_table.csv", row.names = FALSE)


# Relative Error for Benchmarks
bm_clean <- benchmarks %>% 
  spread(key = Benchmark, value = score) %>%
  select(-place)

lb_data %>% 
  filter(place %in% c(1, 50)) %>%
  left_join(bm_clean, by = "comp") %>%
  mutate(
    Naive = score / Naive,
    SNaive = score / `Seasonal Naive`
  ) %>%
  select(Competition, place, Naive, SNaive) %>%
  gather(key = method, value = RelError, Naive, SNaive) %>%
  spread(key = place, value = RelError) %>%
  mutate_at(vars(`1`, `50`), ~round(., digits = 2)) %>%
  rename(Method = method, `Rel. Error 1st` = `1`, `Rel. Error 50th` = `50`) %>%
  write.csv("Results/benchmarking_1st_benchmark_table.csv", row.names = FALSE)
