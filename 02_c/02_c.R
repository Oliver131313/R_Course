# 1
v1 <- c(1, 2, 3) # alebo c(1:3)
v2 <- c(TRUE, FALSE, TRUE)
v3 <- c(4, 5, 6) # alebo c(4:6)

v <- c(v1, v2, v3)
print(length(v))

# 2
prvni_posledni = v[-c(1, length(v))] # alebo v[2:length(v)]
treti <- v[seq(3, length(v), 3)]
vyhod_nuly = v[v!=0]

# 3
x <- c(seq(1, 5, 0.01))
y <- log(x)
plot(x, y, type='l')

# 4
random <- sample(100, 30)
random_sorted <- sort(random)
nejmensi <- min(random)
druhe_nejmensi <- random_sorted[2]
nejvecsi <- max(random)

# ___Slozene uroceni____
# 1 
pv <- 1000
r <- 0.02
n <- 50
roky <- seq(1, n)
uroky <- (1 + r) ^ roky
slozene <- c(rep(pv, n)) * uroky

# 2
po_roce <- slozene[1]
po_30 <- slozene[30]
na_konci <- slozene[n]
v_sudem  <- slozene[seq(2, n, 2)]

# 3 
uniform_r <- runif(n, 0, 2*r)
uroky <- cumprod(1 + uniform_r)
unif_slozene <- c(rep(pv, n)) * uroky
x <- c(seq(1, n))
plot(x, slozene, type='l')
lines(x=x , y=unif_slozene, col='red')

# 4 
rozdil <- diff(slozene)
rast <- rozdil/slozene
#lines()


# 5
roky_aspon_2 <- roky[slozene>=2*pv]
prvni <- roky_aspon_2[1]
pravidlo_72 <- 72/(r*100)

# 6
load("exdata02.RData")
deflator <- hdp_bc / hdp_sc * 100
plot(cas, deflator, type='l')
abline(v = 2010, h=100, lty=c('dotted', 'dotted'))

# 7
inflace_deflator <- (deflator[-1]/ deflator[-length(deflator)]) - 1
rust_realny <- (hdp_sc[-1] / hdp_sc[-length(hdp_sc)]) - 1
rust_nominalni <- (hdp_bc[-1] / hdp_bc[-length(hdp_bc)]) - 1
# alebo---> inflace_deflator_2 <- diff(deflator) / deflator[-length(deflator)]
plot(x=cas[-1], y=inflace_deflator, type='l')
abline(h=0, lty='dotted')

# Elasticita 
# 1
p <- c(100,90,80,70,60,50)
q <- c(30,60,90,120,200,210)
plot(p, p*q)
# 2
q_posl = q[length(q)]
q_prvy = q[1]
p_posl = p[length(p)]
p_prvy = p[1]
sum_q_prvy_posl = q_posl + q_prvy
sum_p_prvy_posl = p_posl + p_prvy
prvy_posledny = ((q_posl-q_prvy)/sum_q_prvy_posl) / ((p_posl-p_prvy)/sum_p_prvy_posl)
q1 <- q[-1]
q0 <- q[-length(q)]
sum_q <- (q1 + q0)/2
p1 <- p[-1]
p0 <- p[-length(p)]
sum_p <- (p1 + p0)/2
elasticita <- ((q1-q0)/sum_q) / ((p1-p0)/sum_p)

plot(sum_p, elasticita)
abline(h=-1, lty='dotted')

# DORATAT VSETKY PRIKLADY A UROBIT CHECKS !


