# clear workspace
rm(list = ls())


library(tidyverse)

oly <- as_tibble(VGAMdata::oly12)

# Na zahrati
oly <- oly %>%
    filter(!str_detect(Sport, ",")) %>%
    drop_na(Height, Weight, Sex) %>%
    mutate(bmi = Weight / (Height ^ 2))

oly %>%
    filter(Sex == "M") %>%
    arrange(bmi)

oly <- oly %>%
    group_by(Sport, Sex) %>%
    summarize(mean_bmi = mean(bmi, na.rm = TRUE))

oly %>%
    pivot_wider(names_from = Sex, values_from = mean_bmi) %>%
    select(-F) %>%
    mutate(projectedWeight = M * (1.78 ^ 2)) %>%
    arrange(M)


# CVICENI 1: Privatizace a Resistuce v CR
load("dplyr_exercise_01.RData")

step_01 <- domy %>%
    mutate(LIDAMSIDOB = as.character(LIDAMSIDOB)) %>%
    left_join(privatizace,
              by = c("LIDAMSIDOB" = "IDOB"),
              suffix = c("_p", "_d")) %>%
    # case_when --> Syntax je velmi podobny syntaxu SQL; TRUE sluzi ako ELSE
    mutate(
        status = case_when(
            kod91_vlast == 1 &
                OBDVD %in% c("10", "3") & jvlast_r01 == 1 ~ "restituted",
            !is.na(year) ~ "privatized",
            TRUE ~ "other"
        )
    )

# CVICENI 2: Census v otevrenych datech CSU
census <- read_delim("SLDB_OBYVATELSTVO_2011.CSV")

load("tabulka_obci.RData")
census <- census %>% filter(typuz_naz == "obec") %>%
    # V %
    mutate(u_rate = vse6181 / vse6111 * 100)

census <- census %>%
    mutate(
        KOD_OBEC = as.character(uzkod)
    ) %>% 
    left_join(.,tabulka_obci, by = "KOD_OBEC") %>% 
    mutate(
        pop_category = cut(vse1111, 
                           breaks = c(0,1000,2e6),
                           include.lowest = TRUE,
                           right = TRUE,
                           labels = c("malé","ostatní"))
        # Alternativně můžete použít například case_when() nebo ifelse()
    ) %>% 
    group_by(KOD_KRAJ,pop_category) %>% 
    summarise(
        min = min(u_rate, na.rm = TRUE),
        mean = mean(u_rate, na.rm = TRUE),
        max = max(u_rate, na.rm = TRUE)
    )


# CVICENI 3: WIID data
wiid <- read_tsv("WIID_ver3_sept15_0.tsv")
wiid


# Problem tohto riesenia zavisi na distinct
wiid <- wiid %>% select(Country, Year, Gini, Quality) %>%
    mutate(
        Quality = Quality %>%  factor(levels = c("High","Average","Low","Not known"))
    ) %>% 
    arrange(Quality) %>% 
    drop_na(Gini) %>% 
    # Vyberie iba unique rows pre kombo Country a Year pri zanechani vsetkych prem.
    # .keep_all=FALSE by nam vratil iba Country a Year
    distinct(Country, Year, .keep_all = TRUE)

wiid


# 2. moznost - nezavisi na distinct
wiid <- read_tsv("WIID_ver3_sept15_0.tsv")
wiid <- wiid %>% 
    select(Country, Year, Gini, Quality) %>% 
    drop_na(Gini) %>% 
    group_by(Country, Year) %>% 
    mutate(
        Quality = Quality %>%  factor(levels = c("High","Average","Low","Not known"))
    ) %>% 
    arrange(Quality, .by_group = TRUE) %>% 
    # Slice sluzi... no na slicing :D
    slice(1L) %>%
    ungroup()
    
wiid

#### PREJST SI OSTATNE CVICENIA !