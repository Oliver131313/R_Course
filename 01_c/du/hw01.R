
# Homework: introduction into R
# =============================
# Goal: use RStudio to write the first R script
# reading documentation for mean function needed

# clear workspace
rm(list = ls())

# read numeric vector income from current directory
load(file = "hw01.RData")

# edit only the following line:
mean_income <- mean(x = income, trim=0.025, na.rm = TRUE)

# save mean_income to current directory
save(mean_income, file = "results.RData")
