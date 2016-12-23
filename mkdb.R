library(tidyverse)
library(stringr)
library(magrittr)
library(lazyeval)

treat_to_int <- 1:8
names(treat_to_int) <- paste0("t", treat_to_int)

bias_to_logical <- c("b" = TRUE, "u" = FALSE)

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

db <- read_tsv("./orig/search.csv", col_types = col_types) %>%
  mutate(biased = bias_to_logical[bias],
         treat_id = treat_to_int[treat],
         market_id = as.integer(treat_id * 1000 + market * 100 + period),
         subject_id = as.integer((treat_id - 1) * 18 + subject))

subjects <- db %>%
  select(subject_id, p1, p2, p3, p4) %>%
  group_by(subject_id) %>%
  summarise_each(funs(first))

treatments <- db %>%
  select(treat_id, biased, n, k) %>%
  group_by(treat_id) %>%
  summarise_each(funs(first))

prices <- db %>%
  select(treat_id, market_id, period, subject_id, indexed, price)

write_csv(subjects, "./data/subjects.csv")
write_csv(treatments, "./data/treatments.csv")
write_csv(prices, "./data/prices.csv")
