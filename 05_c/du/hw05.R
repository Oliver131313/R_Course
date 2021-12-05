# Homework: function, control structures, and iteration
# =====================================================

# Goal: read and write data from / to files

# clear workspace
rm(list = ls())

# 1. load necessary libraries
library(readr)
# 2. load the data
mpg <-
    read_delim(
        "hw05.csv",
        delim = ":",
        escape_double = FALSE,
        col_types = cols(
            manufacturer = col_character(),
            model = col_character(),
            displ = col_double(),
            year = col_integer(),
            cyl = col_integer(),
            trans = col_character(),
            drv = col_character(),
            cty = col_double(),
            hwy = col_double(),
            fl = col_character(),
            class = col_factor(
                levels = c(
                    "compact",
                    "midsize",
                    "suv",
                    "2seater",
                    "minivan",
                    "pickup",
                    "subcompact"
                )
            )
        ),
        locale = locale(decimal_mark = ","),
        trim_ws = TRUE
    )

save(mpg, file = "results.RData")

# 3. calculate the correlation
mpg_cty_cor <- cor(x = mpg$displ,
                   y = mpg$cty,
                   use = "complete.obs")

# 4. save the data into results.RData
save(mpg, mpg_cty_cor, file = "results.RData")
