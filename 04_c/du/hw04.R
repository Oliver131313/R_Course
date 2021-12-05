# Homework: function, control structures, and iteration
# =====================================================

# Goal: create functions and use iteration

# clear workspace
rm(list = ls())

# load necessary libraries
library(tibble)
library(purrr)

# load the data needed
load("hw04.RData")

# 1. create function alpha(pstar, p)
alpha <- function(pstar, p) {
    if (!(is.numeric(pstar) && pstar > 0
        && length(pstar) == 1 && is.numeric(p)
        && any(p > 0)))
        
        stop("Bad inputs")
        
    upper <- round(sum((p - pstar) ^ 2) / length(p), 5)
    alpha <- round(sqrt(upper) / pstar * 100, 5)
    return(alpha)
}

save(alpha, file = "results.RData")

# 2. create function alpha_df(df, id)
library(dplyr)
alpha_df <- function(df, ids) {
    #df <- df[!is.na(ids), ]
    df <- df[df$id %in% ids & !is.na(df$id), ]
    pstars <- unique(df$pstar)
    map_dbl(pstars, ~alpha(., df[df$pstar==., ]$p))
}

save(alpha, alpha_df, file = "results.RData")

# 3. create vector ids
ids <- unique(experiment$id)
save(alpha, alpha_df, ids, file = "results.RData")

# 4. create vector alphas
alphas <- alpha_df(experiment, ids)
save(alpha, alpha_df, ids, alphas, file = "results.RData")

# 5. create tibble outcomes
outcomes <- tibble(id = ids, alpha = alphas)
print.data.frame(outcomes)
save(alpha, alpha_df, ids, alphas, outcomes, file = "results.RData")