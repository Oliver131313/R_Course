# Homework: Econometrics
# =====================================================

# Goal: Do it!

# clear workspace
rm(list = ls())

# Your solution follows...
library(tidyverse)
library(lmtest)
library(sandwich)
sandwich::
# Data:
load("hw12.RData")
wage.edit <- wage %>% 
    mutate(gender = factor(gender) %>% relevel("male"),
           ethnicity = factor(ethnicity) %>% relevel("cauc"),
           union = factor(union) %>% relevel("no")
           )

f1 <- log(wage) ~ gender + ethnicity + education
m1r.model <- lm(f1, data = wage.edit)
m1r <- coeftest(m1r.model, vcov=vcovHC.default(m1r.model))
m1r


f2 <- log(wage) ~ gender + ethnicity + education + experience + I(experience^2)
m2r.model <- lm(f2, data = wage.edit)
m2r <- coeftest(m2r.model, vcov=vcovHC.default(m2r.model))
m2r  

f3 <- log(wage) ~ gender + ethnicity + education + experience + I(experience^2) + union
m3r.model <- lm(f3, data = wage.edit)
m3r <- coeftest(m3r.model, vcov=vcovHC.default(m3r.model))
m3r
    
# Save results
save(m1r,m2r,m3r, file = "results.RData")