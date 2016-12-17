
# First let's analyze lottery panels
panels <- read.table("panels.csv", header=TRUE)
summary (panels)

attach (panels)

# mean selections in each panel across treatments
aggregate (p1, list(Treatment=treat), mean)
kruskal.test(p1, treat)

aggregate (p2, list(Treatment=treat), mean)
kruskal.test(p2, treat)

aggregate (p3, list(Treatment=treat), mean)
kruskal.test(p3, treat)

aggregate (p4, list(Treatment=treat), mean)
kruskal.test(p4, treat)


# Histograms
hist(p1, -0.05 + (1:11)/10.0, freq=FALSE)

hist(p2, -0.05 + (1:11)/10.0, freq=FALSE)

hist(p3, -0.05 + (1:11)/10.0, freq=FALSE)

hist(p4, -0.05 + (1:11)/10.0, freq=FALSE)


# Average selection
r <- (p1 + p2 + p3 + p4) / 4
aggregate (r, list(Treatment=treat), mean)
aggregate (r, list(Treatment=treat), sd)
kruskal.test(r, treat)
fligner.test(r, treat)

hist(r, -0.05 + (1:11)/10.0, freq=FALSE)

# Mon
mon12 <- p2 <= p1
mon23 <- p3 <= p2
mon34 <- p4 <= p3

mon <- as.factor (mon12)

table (treat, mon)

aggregate (r, list(Mon=mon, Treatment=treat), mean)
aggregate (r, list(Mon=mon, Treatment=treat), sd)

wilcox.test (r ~ mon)
wilcox.test (r ~ mon, subset=treat=="t1")
wilcox.test (r ~ mon, subset=treat=="t2")
wilcox.test (r ~ mon, subset=treat=="t3")
wilcox.test (r ~ mon, subset=treat=="t4")
wilcox.test (r ~ mon, subset=treat=="t5")
wilcox.test (r ~ mon, subset=treat=="t6")
wilcox.test (r ~ mon, subset=treat=="t7")
wilcox.test (r ~ mon, subset=treat=="t8")


fligner.test (r ~ mon)
fligner.test (r ~ mon, subset=treat=="t1")
fligner.test (r ~ mon, subset=treat=="t2")
fligner.test (r ~ mon, subset=treat=="t3")
fligner.test (r ~ mon, subset=treat=="t4")
fligner.test (r ~ mon, subset=treat=="t5")
fligner.test (r ~ mon, subset=treat=="t6")
fligner.test (r ~ mon, subset=treat=="t7")
fligner.test (r ~ mon, subset=treat=="t8")

detach(panels)

rm (panels, mon12, mon23, mon34, mon, r)

# Tests on means
search.full <- read.table("search.csv", header=TRUE)

# Skip first 20 periods
search <- subset (search.full, search.full$period > 20 & 
                  (search.full$indexed == 1 | search.full$bias == "u"))

pmean = aggregate (search$price,
  list (market = search$market, period = search$period,
        treat = search$treat), mean)

search <- subset (search.full, search.full$period > 20 & 
                  search.full$indexed == 1)

pmin = aggregate (search$price,
  list (market = search$market, period = search$period,
        treat=search$treat), min)


markets = data.frame (treat = pmean$treat, period = pmean$period,
  market = pmean$market, eps = pmean$x, mu = pmin$x)

attach (markets)
aggregate (eps, list(Treatment=treat), mean)
aggregate (eps, list(Treatment=treat), sd)
aggregate (mu, list(Treatment=treat), mean)
aggregate (mu, list(Treatment=treat), sd)

t1 <- treat == "t1"
t2 <- treat == "t2"
t3 <- treat == "t3"
t4 <- treat == "t4"
t5 <- treat == "t5"
t6 <- treat == "t6"
t7 <- treat == "t7"
t8 <- treat == "t8"

wilcox.test (eps[t1], eps[t4], alternative = "l")

wilcox.test (eps[t5], eps[t4], alternative = "l")
wilcox.test (eps[t7], eps[t4], alternative = "l")
wilcox.test (eps[t7], eps[t5], alternative = "l")
wilcox.test (eps[t2], eps[t1], alternative = "l")

t.test (eps[t1], eps[t4], alternative = "l", var.equal = TRUE)

t.test (eps[t5], eps[t4], alternative = "l", var.equal = TRUE)
t.test (eps[t7], eps[t4], alternative = "l", var.equal = TRUE)
t.test (eps[t7], eps[t5], alternative = "l", var.equal = TRUE)
t.test (eps[t2], eps[t1], alternative = "l", var.equal = TRUE)

t.test (eps[t2], eps[t7], alternative = "t", var.equal = TRUE)

t.test (eps[t6], eps[t4], alternative = "l", var.equal = TRUE)
t.test (eps[t8], eps[t4], alternative = "l", var.equal = TRUE)
t.test (eps[t8], eps[t6], alternative = "l", var.equal = TRUE)
t.test (eps[t3], eps[t1], alternative = "l", var.equal = TRUE)

t.test (eps[t8], eps[t3], alternative = "l", var.equal = TRUE)

t.test (eps[t6], eps[t5], alternative = "l", var.equal = TRUE)
t.test (eps[t8], eps[t7], alternative = "l", var.equal = TRUE)
t.test (eps[t3], eps[t2], alternative = "l", var.equal = TRUE)


t.test (mu[t4], mu[t1], alternative = "l", var.equal = TRUE)

t.test (mu[t4], mu[t5], alternative = "l", var.equal = TRUE)
t.test (mu[t4], mu[t7], alternative = "l", var.equal = TRUE)
t.test (mu[t5], mu[t7], alternative = "l", var.equal = TRUE)
t.test (mu[t1], mu[t2], alternative = "l", var.equal = TRUE)

t.test (mu[t2], mu[t7], alternative = "t", var.equal = TRUE)

t.test (mu[t6], mu[t4], alternative = "l", var.equal = TRUE)
t.test (mu[t8], mu[t4], alternative = "l", var.equal = TRUE)
t.test (mu[t8], mu[t6], alternative = "l", var.equal = TRUE)
t.test (mu[t3], mu[t1], alternative = "l", var.equal = TRUE)

t.test (mu[t8], mu[t3], alternative = "l", var.equal = TRUE)

t.test (mu[t6], mu[t5], alternative = "l", var.equal = TRUE)
t.test (mu[t8], mu[t7], alternative = "l", var.equal = TRUE)
t.test (mu[t3], mu[t2], alternative = "l", var.equal = TRUE)

detach (markets)


search <- subset (search.full, search.full$period > 20 & 
                  (search.full$indexed == 1 | search.full$bias == "u"))
attach (search)

t1 <- treat == "t1"
t2 <- treat == "t2"
t3 <- treat == "t3"
t4 <- treat == "t4"
t5 <- treat == "t5"
t6 <- treat == "t6"
t7 <- treat == "t7"
t8 <- treat == "t8"

plot.density (price[t1 & subject==1])
detach (search)

search <- subset (search.full, search.full$period > 15 & 
                  (search.full$indexed == 1 | search.full$bias == "u"))
aggregate (search$price, list(Treatment=search$treat), min)
