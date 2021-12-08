# clear workspace
rm(list = ls())


library(tidyverse)


# Cviceni A
dplyr::starwars %>%
    ggplot(aes(x = eye_color)) +
    geom_bar()


# 1. Moznost zoradenia
starwars %>%
    mutate(eye_color = factor(
        eye_color,
        levels = c(
            "brown",
            "blue",
            "yellow",
            "black",
            "orange",
            "red",
            "hazel",
            "unknown",
            "blue-gray",
            "dark",
            "gold",
            "green, yellow",
            "pink",
            "red, blue",
            "white"
        )
    )) %>%
    ggplot(aes(x = eye_color)) +
    geom_bar()

# 2. moznost zoradenia
starwars %>%
    group_by(eye_color) %>%
    summarise(obs = n()) %>%
    arrange(desc(obs)) %>%
    mutate(eye_color = factor(eye_color, levels = eye_color)) %>%
    ggplot(aes(x = eye_color, y = obs)) +
    geom_col()


# Prehodenie os
starwars %>%
    group_by(eye_color) %>%
    summarise(obs = n()) %>%
    arrange(obs) %>%
    mutate(eye_color = factor(eye_color, levels = eye_color)) %>%
    ggplot(aes(x = eye_color, y = obs)) +
    geom_col() +
    coord_flip()


# Cvicenie B - Histogramy
load("village.RData")

village %>%
    ggplot(aes(x = age)) +
    geom_histogram()

village %>%
    ggplot(aes(x = age)) +
    geom_histogram(bins = 100)


village %>%
    ggplot(aes(x = age)) +
    geom_histogram(binwidth = 1)


village %>%
    ggplot(aes(x = age)) +
    geom_histogram(breaks = seq(from = 0, to = 80, by = 2))

# Cviceni C
village %>%
    ggplot(aes(x = age)) +
    geom_density(alpha = 0.3, bw = 1)


village %>%
    ggplot(aes(x = age, fill = gender, linetype = gender)) +
    geom_density(alpha = 0.5)


village %>%
    ggplot(aes(x = age)) +
    geom_density() +
    stat_density(alpha = 0.3,
                 bw = 1)


# Cviceni E
village %>%
    ggplot() +
    geom_histogram(
        data = select(village, -gender),
        aes(y = ..count.., x = age),
        fill = "grey",
        color = NA
    ) +
    geom_histogram(aes(y = ..count.., x = age, fill = gender),
                   color = NA) +
    facet_wrap("gender") +
    theme_bw()


# Cviceni F
village %>%
    ggplot(aes(x = gender, y = age)) +
    geom_boxplot()


village %>%
    ggplot(aes(x = gender, y = age)) +
    geom_violin()

# Cviceni G
readxl::read_excel("klima.xlsx", sheet = 2, skip = 3) %>%
    rename(mesic = `měsíc`) %>%
    pivot_longer(-c(rok, mesic), names_to = "den") %>%
    mutate(den = as.integer(den)) %>%
    group_by(mesic, den) %>%
    summarise(average = mean(value, na.rm = TRUE)) %>%
    ggplot(aes(x = den, y = mesic, fill = average)) +
    geom_tile() +
    scale_fill_distiller(palette = "RdYlBu")

# Cvciceni H
oly12 <- VGAMdata::oly12 %>% as_tibble()

oly12 %>%
    filter(Country == "France") %>%
    drop_na(Height, Weight, Age, Sex) %>%
    ggplot(aes(
        x = Height,
        y = Weight,
        size = Age,
        color = Sex
    )) +
    geom_point(shape = 1) +
    theme_classic()

# Cviceni I
oly12 %>%
    filter(Country == "France") %>%
    drop_na(Height, Weight, Age, Sex) %>%
    ggplot(aes(x = Height, y = Weight, fill = Sport)) +
    geom_point(shape = 21,
               color = "white") +
    theme_classic()


oly12 %>%
    filter(Country == "France") %>%
    drop_na(Height, Weight, Age, Sex) %>%
    mutate(Sport = as.character(Sport)) %>%
    mutate(
        Sport = case_when(
            str_detect(Sport, "Cycling") ~ "Cycling",
            str_detect(Sport, "Canoe") |
                Sport == "Rowing" | Sport == "Sailing" ~ "Stuff with a boat",
            Sport %in% c("Basketball", "Badminton", "Football", "Handball") ~ "Collective",
            Sport %in% c(
                "Archery",
                "Fencing",
                "Judo",
                "Shooting",
                "Weightlifting",
                "Wrestling"
            ) ~ "Fighting",
            Sport %in% c("Diving", "Swimming") ~ "Watersports",
            str_detect(Sport, "Tennis") ~ "Tennis",
            Sport %in% c("Equestrian", "Triathlon") ~ "Endurance",
            TRUE ~ Sport
        )
    ) %>%
    ggplot(aes(x = Height, y = Weight, fill = Sport)) +
    geom_point(
        shape = 21,
        color = "white",
        size = 4,
        alpha = 0.75
    ) +
    scale_fill_brewer(palette = "Paired") +
    theme_classic()


# Cviceni J
oly12 %>%
    filter(Country == "France") %>%
    drop_na(Height, Weight, Age, Sex) %>%
    ggplot(aes(x = Height, y = Weight, size = Age)) +
    geom_point(shape = 1) +
    theme_classic()


oly12 %>% 
    filter(Country == "France") %>% 
    drop_na(Height,Weight,Age,Sex) %>% 
    ggplot(
        aes(x = Age)
    ) +
    geom_histogram() +
    theme_classic()


oly12 %>%
    filter(Country == "France") %>%
    drop_na(Height, Weight, Age, Sex) %>%
    mutate(age_cat = cut(Age, breaks = c(0, 20, 25, 30, 35, Inf))) %>%
    ggplot(aes(x = Height, y = Weight, fill = age_cat)) +
    geom_point(
        shape = 21,
        color = "white",
        size = 4,
        alpha = 0.75
    ) +
    scale_fill_brewer(name = "Age",
                      palette = "OrRd") +
    theme_classic()