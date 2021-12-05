# clear workspace
rm(list = ls())

library(tidyverse)

# Na zahrati 
# 1
tidyr::table4a

table4a %>% pivot_longer(-country, names_to = "year")

table4a %>% 
    # Vsetky stlpce okrem country daj do noveho stlpca "year"
    pivot_longer(-country, names_to = "year") %>% 
    # Z country urob mena stlpcov
    pivot_wider(names_from = country) 

# A - Psychologie zeleniny
veg <- psychTools::veg
veg$Veg_B <- row.names(veg)
veg <- as_tibble(veg, rownames = "Veg_B")

veg %>% 
    pivot_longer(-Veg_B, names_to = "Veg_A")

veg %>% 
    pivot_longer(-Veg_B, names_to = "Veg_A") %>% 
    arrange(desc(value))

# B - World Bank
# Ak pracujem z API, tak je dobre stiahnut raz, data ulozit a nerobit s API kontinualne,
# pretoze data mozu byt revidovane alebo pomenene !!!
load("tidyr_wbdata.RData")
wbdata

# 1. moznost
# Vyhod Indicator aby neskor mal df spravny pocet riadkov (pretoze ten jeden je duplikovany)
wbdata <- wbdata %>% select(-indicator) 

wbdata <- wbdata %>%
            pivot_wider(names_from = indicatorID) 

# 2. Moznost
wbdata <- wbdata %>%
    # id_cols akoby udava tie na ktore by sa mali vytvorit kombinacie stlpca indicatorID
    pivot_wider(id_cols = c(country,iso2c,date), names_from = indicatorID)

## C - Yahoo Finance
goog <- read_csv("tidyr_GOOG.csv")

# 1. Moznost
library(lubridate)
goog$year <- year(goog$Date)
goog$month <- month(goog$Date)
goog$day <- day(goog$Date)

# 2. 
# Najprv je potrebne nacitat stlpec Date ako character a nie ako Date
goog <- read_csv("tidyr_GOOG.csv", col_types = "cnnnnnn")
goog %>% 
    separate(Date, c("year","month","day"), convert = TRUE, sep = "-")

# Napriek tomu je lepsie to urobit skrz lubridate ale iba pri filtrovani. To znamena, ze
# nechceme zmenit stlpec Date ale iba z neho vytahovat to co potrebujeme.


# D - World Population Prospects
load("tidyr_wpp_pop.RData")
pop

# 1
pop <- pop %>%
    # Vyhodou tohto negativneho vyctu je, ze ak pribudne novy rok a kodovali by sme
    #  to na tvrdo, tak uz by nam kod nefungoval tak ako chceme.
    pivot_longer(
        -c(country_code, name),
        names_to = "year",
        values_to = "population",
        names_transform = list(year = as.integer)
    ) %>%
    # 2 - Zmeni 5 rocny interval na 1 rocny
    # Funkcia najde vsetky unique kombinacie tychto 3 stlpcov a priradi im hodnoty
    # z population
    # Da sa to urobit aj bez .$ ale s tym placeholderom . to bude urcite fungovat
    # nesting() spravi to aby country_code a name boli povazovane za jeden stlpec, pretoze
    # v podstate indikuju presne tu istu informaciu a keby to spravime bez toho, tak
    # nam vytvori vsetky mozne kombinacie pre oba stlpce, co je blbost lebo by kombinoval
    # rozne kody s roznymi nazvami statov. V skutocnosti ma kazdy stat len jeden kod
    # a teda oba stlpce popisuju len jednu vec (stat) a nie dve rozne !!!
    complete(nesting(country_code, name), year = min(.$year):max(.$year)) %>%
    # Nutnost spravneho zoradenia pretoze by doplnovalo hodnoty jednej krajiny pre inu
    group_by(country_code) %>% 
    arrange(year, .by_group = TRUE) %>% 
    fill(population, .direction = "down") %>%  # Doplnenie NA hodnot - odhora dolu
    ungroup()


# E - Google Forms ##### DOKONCIT!!!!
gforms <- read_csv("tidyr_GoogleForms.csv") 
print(gforms)

