# Homework: ggplot2
# =====================================================

# Goal: Plot it!

# clear workspace
rm(list = ls())

# Your solution follows...
library(tidyverse)
# Data:

# VGAMdata::oly12 %>%
#     na.omit() %>%
#     mutate(
#         BMI = Weight / Height ^ 2,
#         BMI_limits = cut(
#             BMI,
#             breaks = c(17, 18.5, 25, 30),
#             include.lowest = TRUE,
#             right = TRUE,
#             labels = c("Mild thinness", "Normal range", "Pre-obese")
#         )
#     ) %>%
#     group_by(BMI_limits, Sex) %>%
#     summarise(
#         max_W = max(Weight),
#         min_W = min(Weight),
#         max_H = max(Height),
#         min_H = min(Height),
#         max_BMI = max(BMI),
#         min_BMI = min(BMI)
#     )

# Nacitanie dat
oly <- VGAMdata::oly12

# GGplot
oly %>%
    ggplot(aes(x = Height, y = Weight)) +
    geom_hex() +
    geom_function(fun = function(x) x ^ 2 * 30,
                  aes(linetype = "ob_upper")) +
    geom_function(fun = function(x) x ^ 2 * 25,
                  aes(linetype = "norm_upper")) +
    geom_function(fun = function(x) x ^ 2 * 18,
                  aes(linetype = "norm_lower")) +
    geom_function(fun = function(x) x ^ 2 * 17.5,
                  aes(linetype = "thin_lower")) +
    facet_wrap(Sex ~ .,
               scales = "free",
               labeller = as_labeller(c(`F` = "Female", `M` = "Male"))) +
    scale_fill_distiller(name="No. of observations", 
                         palette = "RdYlBu",
                         breaks=c(160, 120, 80, 40)) +
    scale_linetype_manual(name = "BMI limits",
                          values = c("ob_upper" = "dashed",
                                     "norm_upper" = "solid",
                                     "norm_lower" = "solid",
                                     "thin_lower" = "dashed"),
                          labels = c("Pre-obese (upper bound)",
                                     "Normal range (upper bound)",
                                     "Normal range (lower bound)",
                                     "Mild thinness (lower bound)"),
                          ) + 
    theme_bw() + 
    theme(legend.position = "bottom",
          legend.direction = "vertical") + 
    labs(title = "London 2012 participants")


# Save results
ggsave("results.pdf",
       width = 190,
       height = 160,
       units = "mm")




