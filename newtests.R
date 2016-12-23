library(tidyverse)
library(stringr)
library(magrittr)
library(lazyeval)

treat_to_int <- 1:8
names(treat_to_int) <- paste0("t", treat_to_int)


col_types <- cols(treat = col_character(),
                  bias = col_character(),
                  n = col_integer(),
                  k = col_integer(),
                  subject = col_integer(),
                  market = col_integer(),
                  indexed = col_logical(),
                  period = col_integer(),
                  price = col_double(),
                  p1 = col_double(),
                  p2 = col_double(),
                  p3 = col_double(),
                  p4 = col_double())

db <- read_tsv("./search.csv", col_types = col_types) %>%
  filter(period > 20) %>%
  mutate(treat_id = treat_to_int[treat],
         market_id = treat_id * 1000 + market * 100 + period,
         subject_id = (treat_id - 1) * 18 + subject,
         group = as.integer(floor((period - 1) / 10)),
         epsprice = if_else(bias == "u" | indexed, price, NA_real_),
         muprice = if_else(indexed, price, NA_real_))

pricedb <- db %>% group_by(market_id) %>%
  summarise(treat = first(treat),
            period = first(period),
            pmean = mean(epsprice, na.rm = TRUE),
            pmin = min(muprice, na.rm = TRUE))

pricedb %>% group_by(treat) %>%
  summarise('Avg p: mean' = mean(pmean),
            'Avg p: sd' = sd(pmean),
            'Min p: mean' = mean(pmin),
            'MIn p: sd' = sd(pmin))


avg1 <- filter(pricedb, treat == "t1")$pmean
avg2 <- filter(pricedb, treat == "t2")$pmean
avg3 <- filter(pricedb, treat == "t3")$pmean
avg4 <- filter(pricedb, treat == "t4")$pmean
avg5 <- filter(pricedb, treat == "t5")$pmean
avg6 <- filter(pricedb, treat == "t6")$pmean
avg7 <- filter(pricedb, treat == "t7")$pmean
avg8 <- filter(pricedb, treat == "t8")$pmean

min1 <- filter(pricedb, treat == "t1")$pmin
min2 <- filter(pricedb, treat == "t2")$pmin
min3 <- filter(pricedb, treat == "t3")$pmin
min4 <- filter(pricedb, treat == "t4")$pmin
min5 <- filter(pricedb, treat == "t5")$pmin
min6 <- filter(pricedb, treat == "t6")$pmin
min7 <- filter(pricedb, treat == "t7")$pmin
min8 <- filter(pricedb, treat == "t8")$pmin

kruskal.test(epsprice ~ group, data = filter(db, treat == "t1", group != 2))
kruskal.test(epsprice ~ group, data = filter(db, treat == "t2", group != 2))
kruskal.test(epsprice ~ group, data = filter(db, treat == "t3", group != 2))
kruskal.test(epsprice ~ group, data = filter(db, treat == "t4", group != 2))
kruskal.test(epsprice ~ group, data = filter(db, treat == "t5", group != 2))
kruskal.test(epsprice ~ group, data = filter(db, treat == "t6", group != 2))
kruskal.test(epsprice ~ group, data = filter(db, treat == "t7", group != 2))
kruskal.test(epsprice ~ group, data = filter(db, treat == "t8", group != 2))


filter(pricedb, treat == "t1") %$% cor.test(pmean, period, method = "kendall")
filter(pricedb, treat == "t2") %$% cor.test(pmean, period, method = "kendall")
filter(pricedb, treat == "t3") %$% cor.test(pmean, period, method = "kendall")
filter(pricedb, treat == "t4") %$% cor.test(pmean, period, method = "kendall")
filter(pricedb, treat == "t5") %$% cor.test(pmean, period, method = "kendall")
filter(pricedb, treat == "t6") %$% cor.test(pmean, period, method = "kendall")
filter(pricedb, treat == "t7") %$% cor.test(pmean, period, method = "kendall")
filter(pricedb, treat == "t8") %$% cor.test(pmean, period, method = "kendall")


do_plot <- function(treat) {
  ggplot(data = filter_(pricedb, interp(~ treat == x, x = treat)),
         mapping = aes(x = period, y = pmean)) +
    geom_point() +
    geom_smooth(method = "lm") +
    coord_fixed(ratio = 30 / 1) +
    ggtitle(paste0("Treatment: ", treat))
}

treatments <- paste0("t", 1:8)
for(t in treatments) {
  plt <- do_plot(t)
  print(plt)
}

ggplot(data = filter(pricedb, treat == "t8"),
       mapping = aes(x = period, y = pmean)) +
  geom_point() +
  geom_smooth()


filter(pricedb, treat == "t1") %$% plot(period, pmean)



t.test(avg1, avg4, alternative = "less")

t.test(avg5, avg4, alternative = "less")
t.test(avg7, avg4, alternative = "less")
t.test(avg7, avg5, alternative = "less")
t.test(avg2, avg1, alternative = "less")

t.test(avg2, avg7, alternative = "two.sided")

t.test(avg6, avg4, alternative = "less")
t.test(avg8, avg4, alternative = "less")
t.test(avg8, avg6, alternative = "less")
t.test(avg3, avg1, alternative = "less")

t.test(avg8, avg3, alternative = "less")

t.test(avg6, avg5, alternative = "less")
t.test(avg8, avg7, alternative = "less")
t.test(avg3, avg2, alternative = "less")


t.test(min4, min1, alternative = "less")

t.test(min4, min5, alternative = "less")
t.test(min4, min7, alternative = "less")
t.test(min5, min7, alternative = "less")
t.test(min1, min2, alternative = "less")

t.test(min2, min7, alternative = "two.sided")

t.test(min6, min4, alternative = "less")
t.test(min8, min4, alternative = "less")
t.test(min8, min6, alternative = "less")
t.test(min3, min1, alternative = "less")

t.test(min8, min3, alternative = "less")

t.test(min6, min5, alternative = "less")
t.test(min8, min7, alternative = "less")
t.test(min3, min2, alternative = "less")



wilcox.test(avg1, avg4, alternative = "less")

wilcox.test(avg5, avg4, alternative = "less")
wilcox.test(avg7, avg4, alternative = "less")
wilcox.test(avg7, avg5, alternative = "less")
wilcox.test(avg2, avg1, alternative = "less")

wilcox.test(avg2, avg7, alternative = "two.sided")

wilcox.test(avg6, avg4, alternative = "less")
wilcox.test(avg8, avg4, alternative = "less")
wilcox.test(avg8, avg6, alternative = "less")
wilcox.test(avg3, avg1, alternative = "less")

wilcox.test(avg8, avg3, alternative = "less")

wilcox.test(avg6, avg5, alternative = "less")
wilcox.test(avg8, avg7, alternative = "less")
wilcox.test(avg3, avg2, alternative = "less")


wilcox.test(min4, min1, alternative = "less")

wilcox.test(min4, min5, alternative = "less")
wilcox.test(min4, min7, alternative = "less")
wilcox.test(min5, min7, alternative = "less")
wilcox.test(min1, min2, alternative = "less")

wilcox.test(min2, min7, alternative = "two.sided")

wilcox.test(min6, min4, alternative = "less")
wilcox.test(min8, min4, alternative = "less")
wilcox.test(min8, min6, alternative = "less")
wilcox.test(min3, min1, alternative = "less")

wilcox.test(min8, min3, alternative = "less")

wilcox.test(min6, min5, alternative = "less")
wilcox.test(min8, min7, alternative = "less")
wilcox.test(min3, min2, alternative = "less")

