# Homework: datasets, factors, and the like
# =========================================

# Goal: use tibbles and factors

# clear workspace
rm(list = ls())

# load necessary libraries
library(tibble)

# load the data needed
load("hw03.RData")

# 1. in dataset modal_split: keep only rows where vehicle is TRN and the data are not
#    older than 2004-01-01; remove the rest
modal_split <-
    modal_split[(modal_split$vehicle == "TRN") &
                    (modal_split$time >= '2004-01-01'), ]
save(modal_split, file = "results.RData")
# make no changes in modal_split beyond this line!!!

# 2. in dataset train_total: keep only rows where units are MIO_PKM; remove the rest
train_total <- train_total[train_total$unit == "MIO_PKM",]
save(modal_split, train_total, file = "results.RData")
# make no changes in train_total beyond this line!!!

# 3. create new (tibble) dataset transit with the following variables: time, train_share,
#    train_volume, and transit_volume
transit1 <-
    tibble(
        time = train_total$time,
        train_share = modal_split$values,
        train_volume = train_total$values,
        transit_volume = train_volume / (train_share/100)
    )
save(modal_split, train_total, transit1, file = "results.RData")
# make no changes in transit1

# 4. sort transit dataset so that the first row is for 2004, the second for 2005, etc.
transit2 <- transit1[order(transit1$time), ]
save(modal_split, train_total, transit1, transit2, file = "results.RData")
# make no changes in transit2

# 5.  add transit_growth variable to the dataset transit
# transit_volume in t
tv_t <- transit2$transit_volume[-1]
# transit_volume in t-1
tv_t0 <- transit2$transit_volume[-nrow(transit2)]
tv_growth <- (tv_t/tv_t0 - 1) * 100

# Copy of transit2, so the memory adresses aren't the same because if they were,
# changes made to transit3 would also change transit2
# It can verified using tracemem(transit2) == tracemem(transit3)
transit3 <- as_tibble(transit2)
# Create new column and fill it with growth of transit volume - 1st row must contain NA
# because the first year couldn't see any growth
transit3$transit_growth <- c(NA_real_, tv_growth)
save(modal_split, train_total, transit1, transit2, transit3, file = "results.RData")
