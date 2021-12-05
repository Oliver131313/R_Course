# Homework: dplyr
# =====================================================

# Goal: Make it tidy!

# clear workspace
rm(list = ls())

# load data
load("hw08.RData")
library(tidyverse)

# 1. Create one table from list of tables in CZSO
czso_table <- CZSO %>% reduce(rbind)

czso_table

# 2. Filter observations
czso_table_step1 <- czso_table %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= 1991 &
               year <= 2010 & str_detect(city_id, pattern = "^5"))

czso_table_step1

# 3. Get average mortality rate and population size at settlement level
czso_table_step2 <- czso_table_step1 %>%
    group_by(city_id) %>%
    summarise(mort_rate = mean(mort / pop_b, na.rm=TRUE),
              pop_b = mean(pop_b, na.rm=TRUE)) %>%
    ungroup()

czso_table_step2

# 4. Get aggregates for settlement categories
czso_table_step3 <- czso_table_step2 %>%
    mutate(city_pop_cat = cut(
        pop_b,
        breaks = c(0, 1000, 10000, 2e6),
        include.lowest = TRUE,
        right = TRUE,
        labels = c("male", "stredne", "velke")
    )) %>%
    group_by(city_pop_cat) %>%
    summarise(mort_rate_mean = mean(mort_rate, na.rm=TRUE),
              mort_rate_sd = sd(mort_rate, na.rm = TRUE)) %>% 
    ungroup()

czso_table_step3


# Save the solution
save(czso_table,
     czso_table_step1,
     czso_table_step2,
     czso_table_step3,
     file = "results.RData")
