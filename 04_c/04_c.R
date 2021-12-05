# Ukol 1
totez <- function(x)
    x

print(totez(5))
print(totez(c(1, 2, 3)))


# Ukol 2
root <- function(x, n = 2) {
    x ^ (1 / n)
}

print(root(16))


# Ukol 3
s <- list(1, 1:2, 1:3, 1:4, 1:5)
#  alebo - zadaj x, aplikuj funkciu tak aby tvoril 1:1, 1:2, 1:3,...
s <- map(1:5, function(x)
    , 1:x) # purr
#  alebo - robi to iste co to predtym (. je povolene meno premennej)
s <- map(1:5, ~ 1:.)
v <- map_dbl(s, mean) # map_dbl s balika purr

print(v)

# Ukol 4
Sign <- function(x) {
    if (x > 0)
        1
    else if (x == 0)
        0
    else
        - 1
}

print(Sign(-10))
print(Sign(1))
print(Sign(0))

# Ukol 5
rand_vekt <- map(1:5, ~ runif(10)))

print(rand_vekt)
####################################################################
####################################################################
#                       Hratky s funkcemi
# Ukol 1
vekt <- function(n) {
    if (is.numeric(n) && length(n) == 1 && round(n) == n && n > 0)
        1:n
    else
        NA
}

print(vekt(5))
print(vekt(0))

# Ukol 2
nmax <- function(x, n = 1) {
    stopifnot(is.numeric(x),
              is.numeric(n),
              length(n) == 1,
              round(n) == n,
              n > 0)
    sort(unique(x), decreasing = TRUE)[n]
    
}

print(nmax(1:5, 2))
print(nmax(1:10))
print(nmax(1:10, c(1, 2)))

##################################################################
##################################################################
#                           Iterace
# Ukol 1
library(purrr)
library(ggplot2)
data("mpg", package = "ggplot2")

print(map_chr(mpg, class))
print(map_int(mpg, ~ sum(is.na(.))))

##################################################################
#                       Nemecka picovina
# Ukol 1
testy <- function(o = 50,
                  so = 30,
                  pok = 3) {
    spravil <- FALSE
    n <- 0
    while (spravil != 1 && n != pok) {
        odpovede <- sample(
            x = c(0, 1),
            size = o,
            prob = c(3 / 4, 1 / 4),
            replace = TRUE
        )
        spravne <- sum(odpovede)
        if (spravne < so) {
            spravil <- FALSE
            n <- n + 1
        }
        else if (spravne >= so)
            spravil <- TRUE
    }
    return(spravil)
}

# call
S <- 10000
vysledok <- 0
for (s in 1:10000) {
    vysledok <- vysledok + testy()
}
freq <- vysledok / S
print(freq)

## Mam to spravne, ale da sa to spravit omnhoo efektivnejsie - UCITELOVO RIESENIE
jeden_test <- function(otazek)
    sum(sample(
        0:1,
        size = otazek,
        replace = TRUE,
        prob = c(3 / 4, 1 / 4)
    ))
# pomocí starého replicate()
testy <- function(otazek, opakovani)
    max(replicate(opakovani, jeden_test(otazek)))
# totéž pomocí purrr
testy <- function(otazek, opakovani)
    max(map_int(seq_len(opakovani), ~ jeden_test(otazek)))
# mean(replicate(1e4, testy(50, 3)) >= 30)  # pomocí starého replicate
freq <- mean(map(1:1e4, ~ testy(50, 3)) >= 30)  # totéž pomocí purrr
print(freq)


# Ukol 2
jeden_test <- function(otazek, pozna)
    sum(sample(
        0:1,
        size = otazek,
        replace = TRUE,
        prob = c(3 / 4, 1 / 4)
    )) +
    as.integer(pozna)
# ! Pozor musel som zmenit map_int na map_dbl lebo to robilo problemy !
testy <- function(otazek, opakovani, pozna)
    max(map_dbl(seq_len(opakovani), ~ jeden_test(otazek, pozna)))
# Simuluj
pozna_o <- function(otazek, opakovani, pozna, n = 1e4)
    mean(map(n, ~ testy(otazek, opakovani, pozna)) >= 30L)
#mean(replicate(n, testy(otazek, opakovani, pozna)) >= 30)
pozna_seq <- 0:50
pozna_p <- map_dbl(pozna_seq, ~ pozna_o(50, 3, .))
plot(pozna_seq, pozna_p, type = "l")

# Ukol 3
print(min(pozna_seq[pozna_p >= 0.05]))
print(min(pozna_seq[pozna_p >= 0.5]))

########################################################################################
#                                   Total Brutal

info <- function(df, ...) {
    info_vector <- function(vec, what) {
        if (!is.numeric(vec))
            return(rep(NA_real_, length(what)))
        else
            map_dbl(what, ~.(vec))
    }

    what <- list(...)
    res <- dplyr::bind_rows(map(df, info_vector, what))
    tibble::as_tibble(cbind(stat = names(what), res))
}

info(mpg, mean = mean, median = median)
