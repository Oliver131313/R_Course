# Homework: strings manipulation
# =====================================================

# Goal: work with strings

# clear workspace
rm(list = ls())

# 1. load necessary libraries
library(stringr)
library(readr)
library(tidyverse)

# 2. load the data ("hw06.txt"; encoding UTF8) into people
## Nie som si isty, ci toto je sposob akym to chcete, ale pride mi urcite jednoduchsi
## ako pouzivat napriklad read_lines a podobne. Vdaka tomu sa automaticky zmazu nadby-
## tocne medzery a dalej som zistil, ze v stlpci names neostali ani zbytocne uvodzovky.
## Napriek tomu sa uvodzoviek zbavujem, rovnako aj pomocou str_trim odstranujem zbytocny whitespace
## aby bolo vidno, ze to viem, a taktiez pre istotu aby som sa uistil, ze tam naozaj nebudu pritomne.
people <-
    read_delim(
        "hw06.txt",
        col_names = FALSE,
        delim = "|",
        escape_double = FALSE,
        locale = locale(encoding = 'UTF-8', ),
        trim_ws = TRUE
    )
save(people, file = "results.RData")

# 3. create df1
df1 <- tibble(name = people$X1,
              height = people$X2,
              weight = people$X3)
save(people, df1, file = "results.RData")
print(df1)

# 4. clean names in df2
df2 <- tibble(
    name = str_replace_all(str_trim(df1$name), '"+', ""),
    height = df1$height,
    weight = df1$weight
)
print.data.frame(df2)
save(people, df1, df2, file = "results.RData")

# 5. change height and weight to numeric in df3
df3 <- tibble(
    name = df2$name,
    height = as.double(str_extract(df2$height, "\\d+")),
    weight = as.double(str_extract(df2$weight, "\\d+"))
)
print(df3)
save(people, df1, df2, df3, file = "results.RData")

# 6. add bmi to df4
df4 <- tibble(
    name = df3$name,
    height = df3$height,
    weight = df3$weight,
    bmi = as.double(weight/(height/100)^2)
)
print(df4)
save(people, df1, df2, df3, df4, file = "results.RData")

# 7. add message to df4
df4$message <- as.character(str_glue("{name} has BMI {bmi}.", 
                        name=df4$name,
                        bmi=round(df4$bmi, 1)))
print(df4)
save(people, df1, df2, df3, df4, file = "results.RData")