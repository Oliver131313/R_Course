# Clear workspace
rm(list=ls())

# Load libs and data
library(tidyverse)
library(broom)
library(stargazer)

load("ZK_wdi.RData")       # Ukol 1
load("ZK_lits2.RData")           # Ukol 2
load("ZK_wdi_raw.Rdata")   # Ukol 3
load("housing.RData")      # Ukol 4
############################### ULOHA c. 1 ##############################
# Check data
lits
names(lits)

# Solution
lits %>% 
    group_by(country_name) %>%
    summarise(owned_prop = sum(dwelling_ownership=="OWNED")/length(dwelling_ownership))



######################## ULOHA c. 2 #################################
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

############################# ULOHA c. 3 #####################################
# Check data
colnames(housing)
housing

# 1. Solution
housing %>% 
    ggplot(aes(x=price, group=utown, fill=utown)) +
    geom_density(alpha=0.33) +
    theme_bw()

# 2. Solution
housing %>% 
    ggplot(aes(x=size, y=price, size=age, color=utown)) + 
    geom_point() +
    theme_bw()



################### Uloha c. 4 #############################


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

### 3. - metoda 1
## Nutne cisla pre t-test - 3. bod 
# Odhadnute hodnoty parametrov a smer. odch.
stats <- summary(fitted[[2]])[["coefficients"]][c("poolTRUE","fireplaceTRUE"), c("Estimate", "Std. Error")]
# Kovariancia
cov <- vcov(summary(fitted[[2]]))["poolTRUE", "fireplaceTRUE"]

# H0: Beta.4 == Beta.5 ---> beta.4 - beta.5 = 0
# Vzorec pre vypocet SE koeficientov
# sqrt[ Var(b.4, b.5) ] = sqrt[ Var(b.4) + Var(b.5) - 2*Cov(b.4, b.5) ]

SE <- sqrt(stats["poolTRUE", "Std. Error"]^2 + stats["fireplaceTRUE", "Std. Error"]^2 - 2*cov)
T.stat <- (stats["poolTRUE", "Estimate"] - stats["fireplaceTRUE", "Estimate"]) / SE

# P-hodnota
pt(q=T.stat, df=nrow(housing) - 1 - ncol(housing), lower.tail = FALSE)*2


### 3 - metoda 2

library(car)

linearHypothesis(fitted[[2]], "poolTRUE = fireplaceTRUE")

### 4. - predikovane hodnoty
pred <-
    predict(fitted[[2]],
            newdata = data.frame(
                size = 100,
                age = 30,
                utown = TRUE,
                pool = FALSE,
                fireplace = FALSE
            ))

print(pred)
