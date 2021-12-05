# Homework: tidyr
# =====================================================

# Goal: Make it tidy!

# clear workspace
rm(list = ls())

# 1. load necessary libraries
library(tidyverse)
library(stringr)

# Import data
raw.data <- read_delim(
    "hw07_data.tsv",
    delim = "\t",
    escape_double = FALSE,
    trim_ws = TRUE,
    quote = "``"
)

# 2. make it tidy

##### 1. Alternativa

# Extrahuj data oddelene ciarkami
# data.split <- as_tibble(
#     str_split(data$`unit,sex,age,c_birth,geo\\time`, ",", simplify = TRUE),
#     name_repair = TRUE
# )
# # Premenuj stlpce
# names(data.split) <- c("unit", "sex", "age", "c_birth", "geo")
#
# data <- cbind(data.split, data[, -1])
# data
# eudata <- data[50, ] %>%
#     pivot_longer(
#         -c(unit, sex, age, c_birth, geo),
#         names_to = "year",
#         names_transform = list(year = as.integer)
#     ) %>%
#     separate(value, c("urate", "note"), sep = " ")
######

##### 2. Alternativa (jednoduchsie, krajsie)
eudata <- raw.data %>%
    separate(`unit,sex,age,c_birth,geo\\time`,
             c("unit", "sex", "age", "c_birth", "geo"),
             ",") %>%
    pivot_longer(
        -c(unit, sex, age, c_birth, geo),
        names_to = "year",
        names_transform = list(year = as.integer)
    ) %>%
    separate(value, c("urate", "note"), sep = " ")

eudata

# Odstran dvojbodky a preved stlpec urate na double 
eudata$urate <- as.double(str_replace(eudata$urate, ":", ""))
# Odstran whitespace zo stlpca note
eudata$note <- str_trim(eudata$note, side = "both")
summary(eudata)


# Save the solution
save(eudata, file = "results.RData")
