rm(list=ls())

###################### ULOHA c. 2 #################################
# Load Data and libraries
library(tidyverse)
library(broom)
library(stargazer)

load("ZK_wdi_raw.Rdata")

# Show data
print(wdi_raw)

colnames(wdi_raw)

wdi <- wdi_raw[, c("iso3c", "year", "IC.BUS.EASE.XQ", "NE.CON.PETC.ZS", "SL.UEM.TOTL.ZS")]
           
wdi <- wdi %>% 
    group_by(iso3c) %>%
    arrange(year, .by_group = TRUE) %>%
    fill(IC.BUS.EASE.XQ, .direction="up") %>%
    filter(year==2010) %>%
    select(-year) %>%
    filter(all(is.na(c(IC.BUS.EASE.XQ, NE.CON.PETC.ZS, SL.UEM.TOTL.ZS)))==FALSE)



################### Uloha c. 4 #############################
# Load data
load("housing.RData")

# Check data
colnames(housing)

housing

# Modely
m1 <- price ~ size + age + utown

models <- list(
    m1,
    m1 %>% update(.~. + pool + fireplace)
)

# Fitni kazdy model
fitted <- map(models, ~lm(., data=housing))

# Zorgnaizuj oba modely do jednej tabulky
stargazer(fitted,
          type = "text")


# Nutne cisla pre t-test - 3, bod 
stats <- summary(fitted[[2]])[["coefficients"]][c("poolTRUE","fireplaceTRUE"), c("Estimate", "Std. Error")]

