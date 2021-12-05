library(ggplot2)
library(tidyverse)
library(dplyr)
library(purrr)
# library(eurostat)
library(readr) # Package na nacitanie dat

###########################         Na zahrati      ###################################
# 1
data(txhousing, package = "ggplot2")
head(txhousing)
class(txhousing)

#########################        GDP at market prices       ###########################
# 1
load("tec00001.RData")
head(tec00001)

# 2
tec00001csv <- read_delim("tec00001.csv")
head(tec00001csv)

# 3
tec00001tsv <-
    read_tsv("tec00001.tsv.gz", col_types = "cnnnnnnnnnnn")
head(tec00001tsv)

# 4
library(readxl)
filenam <-
    "Eurostat_Table_tec00001FlagDesc_11c465e3-eae1-4c8e-b956-f473d4a78a0a.xls"
tec00001xls <- read_excel(
    filenam,
    skip = 3,
    range = "A4:Y47",
    na = ":",
    col_types = c("text", rep(c("numeric", "skip"), 12))
)

head(tec00001xls)

##########################       Murphy: Co se může pokazit      #######################
# 1
read_lines("experiment.csv")

# 2
experiment <-
    read_delim(
        "experiment.csv",
        delim = ":",
        escape_double = FALSE,
        col_types = cols(treatment = col_factor(levels = c(
            "control", "group A", "group B"
        ))),
        locale = locale(decimal_mark = ","),
        trim_ws = TRUE,
        skip = 8
    )
head(experiment)

# 3
experiment2 <- read_delim(
    "experiment2.csv",
    delim = ":",
    escape_double = FALSE,
    col_names = c("id", "name", "height", "weight", "treatment", "value"),
    col_types = cols(
        id = "d",
        name = "c",
        height = "d",
        weight = "d",
        treatment = col_factor(levels = c("control", "group A", "group B")),
        value = "d"
    ),
    locale = locale(decimal_mark = ","),
    trim_ws = TRUE
)
head(experiment2)

# 4
guess_encoding("experiment3.csv")
experiment3 <- read_delim(
    "experiment3.csv",
    delim = ":",
    skip = 8,
    locale = locale(encoding = "ISO-8859-2", decimal_mark = ","),
    col_types = cols(treatment = col_factor(levels = c("control", "group A", "group B")))
)

head(experiment3)

# 5
summary(experiment)

#############################       Ukladani dat        ################################
# 1
save(txhousing, tec00001, experiment, file = "results.RData")
rm(list = ls())
load("results.Rdata")

# 2
library(rio)
write_csv(txhousing, "results.csv") # Do jedneho suboru mozno ulozit len 1 tabulku
#export(txhousing, "txhousing.csv", format = "csv")
#export(tec00001, "tec00001.csv", format = "csv")
#export(experiment, "experiment.csv", format = "csv")

#