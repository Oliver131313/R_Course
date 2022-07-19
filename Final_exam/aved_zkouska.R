# clear workspace
rm(list = ls())

# Libs and data
library(tidyverse)
library(stargazer)
library(lfe)

load("ZK_lits2.RData")
load("star.RData")



################################## Ukol c. 1 ####################################
# Check data
lits

# Solution
u_1 <- lits %>%
    mutate(age_cohort = cut(
        respondentage,
        breaks = c(0, 20, 30, 40, 50, 1e20),
        labels = c(1, 2, 3, 4, 5),
        include.lowest = TRUE,
        right = TRUE
    )) %>%
    group_by(age_cohort) %>%
    summarise(
        moving_proportion = sum(migration_domestic == "Yes" |
                                    migration_international == "Yes") /
            length(migration_domestic)
    ) %>% na.omit()
    

str_glue("\n\n--------- Exercise 1 - Solution ---------\n\n")
u_1


################################## Ukol c. 2 ####################################
# 2.1 - Load
chmi_1 <- read_delim(
    "ZK_chmi.tsv",
    delim = "\t",
    escape_double = FALSE,
    locale = locale(decimal_mark = ","),
    trim_ws = TRUE
)
# Check data
str_glue("\n\n--------- Exercise 2.1 (Data Load) - Solution ---------\n\n")
chmi_1

# 2.2
chmi_2 <- chmi_1 %>% select(-c("X", "Max.", "N"))

str_glue("\n\n--------- Exercise 2.2 (Remove columns) - Solution ---------\n\n")
chmi_2

# 2.3
chmi_3 <- chmi_2 %>%
    mutate(Month = `Den/Měsíc`) %>%
    select(-`Den/Měsíc`) %>%
    pivot_longer(
        -Month,
        names_to = "Day",
        values_to = "PM10",
        names_transform = list(Day = as.integer),
        values_transform = list(PM10 = as.double)
    ) %>%
    arrange(Day, Month)

str_glue("\n\n--------- Exercise 2.3 (Change the structure) - Solution ---------\n\n")
chmi_3
################################## Ukol c. 3 ####################################
# check data
head(star)

model_1 <- totalscore ~ small + boy + white_asian + tchexper
model_2 <- totalscore ~ small + boy + white_asian + tchexper | schid

models <- list("OLS" = lm(model_1, data = star),
               "Fixed effects - schid"=felm(model_2, data = star))

# Solution
str_glue("\n\n--------- Exercise 3 - Solution ---------\n\n")
stargazer(models,
          type = "text",
          column.labels = c("Model 1", "Model 2"),
          column.separate = c(1, 2),
          model.names = FALSE,
          model.numbers = FALSE)

################################## Ukol c. 4 ####################################
# using multiple version - non shows much of a relationship

# V. 1 - The best one probably
v1 <- star %>%
    ggplot(aes(x=tchexper, y=totalscore, color=as.character(boy))) +
    geom_jitter(alpha=0.4) +
    scale_color_manual(name="Gender", 
                       breaks=c(0, 1), 
                       labels=c("Girl", "Boy"),
                       values = c("red", "blue")) +
    facet_grid(white_asian ~ small, labeller = labeller(
        white_asian = c(`0` = "White/Asian = No", `1`="White/Asian = Yes"),
        small = c(`0` = "Small class = No", `1`="Small class = Yes")
    )) + 
    theme_bw() +
    theme(strip.text.y = element_text(angle=0))

v1
   
# V. 2
v2 <- star %>%
    ggplot(aes(x=tchexper, y=totalscore, color=as.character(small))) +
    geom_jitter(alpha=0.4) +
    scale_color_manual(name="Class size", 
                       breaks=c(0, 1), 
                       labels=c("Not small", "Small"),
                       values = c("red", "blue")) +
    facet_grid(white_asian ~ boy, labeller = labeller(
        white_asian = c(`0` = "White/Asian = No", `1`="White/Asian = Yes"),
        boy = c(`0` = "Girl", `1`="Boy")
    )) + 
    theme_bw() +
    theme(strip.text.y = element_text(angle=0))

v2

# V. 3
v3 <- star %>%
    ggplot(aes(x=tchexper, y=totalscore)) +
    geom_point(alpha=0.4) +
    facet_grid(white_asian ~ boy + small, labeller = labeller(
        white_asian = c(`0` = "White/Asian = No", `1`="White/Asian = Yes"),
        boy = c(`0` = "Girl", `1`="Boy"),
        small = c(`0` = "Small class = No", `1` = "Small class = Yes")
    )) + 
    theme_bw() +
    theme(strip.text.y = element_text(angle=0))

v3


v1 <- star %>%
    ggplot(aes(x=tchexper, y=totalscore, color=as.character(boy))) +
    geom_point(alpha=0.1) +
    geom_smooth(method = "lm") +
    scale_color_manual(name="Gender", 
                       breaks=c(0, 1), 
                       labels=c("Girl", "Boy"),
                       values = c("red", "blue")) +
    facet_grid(white_asian ~ small, labeller = labeller(
        white_asian = c(`0` = "White/Asian = No", `1`="White/Asian = Yes"),
        small = c(`0` = "Small class = No", `1`="Small class = Yes")
    )) + 
    theme_bw() +
    theme(strip.text.y = element_text(angle=0))
v1




star %>% ggplot(aes(x = tchexper, y = totalscore, color = as.character(boy))) +
    geom_point() +
    geom_smooth(method="lm") +
    facet_grid(white_asian ~ small,
               labeller = labeller(white_asian = as_labeller(
                   c("1" = "White/Asian", "0" = "Nie je White/Asian")
               ),
               small = as_labeller(
                   c("1" = "Trieda je malá", "0" = "Trieda je veľká")
               ))) +
    scale_x_continuous("Celkové skóre") +
    scale_y_continuous("Skúsenosti učiteľa") +
    scale_color_discrete(name="Pohlavie", breaks=c(0, 1), labels=c("Dievča", "Chlapec"))
    theme_bw()

