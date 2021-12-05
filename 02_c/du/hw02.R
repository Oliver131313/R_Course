# Homework: variables, data types, and atomic vectors
# ===================================================

# Goal: use atomic vectors

# clear workspace
rm(list = ls())

# read numeric vectors person, height, and weight
load("hw02.RData")


# 1. count how many subjects you have and save it
number_of_subjects <- length(height)
save(number_of_subjects, file = "results.RData")

# 2. calculate median weight and save it
med_weight <- median(x = weight, na.rm = FALSE)
save(number_of_subjects, med_weight, file = "results.RData")

# 3. for each person, calculate BMI and round it to tenths
bmi <- round((weight / (height/100) ^ (2)), 1)
save(number_of_subjects, med_weight, bmi, file = "results.RData")

# 4. count obese subjects
number_of_obese <- sum(bmi >= 30) # nebo: length(bmi[bmi >= 30])
save(number_of_subjects, med_weight, bmi, number_of_obese, file = "results.RData")

# 5. create a vector with the names of the obese subjects
obese_subjects <- person[bmi >= 30]
save(number_of_subjects,
     med_weight,
     bmi,
     number_of_obese,
     obese_subjects,
     file = "results.RData")
