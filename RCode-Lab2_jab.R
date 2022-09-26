# R Code
# Lab 2
# Jonah Bacon
# 21 Sep 2022

library(tidyverse)
library(dplyr) 


# Load Post 2002 Figure 2 data:

day.number <- c(1, 15, 30, 45, 60, 75)

littoral.producers <- c('log.periphyton', 'rock.periphyton', 'macrophytes')
pelagic.producers <- c('small.seston', 'large.seston')
total.producers <- c(littoral.producers, pelagic.producers)

spencer <- data.frame(
  "lake" = rep('Spencer', 30),
  "day" = rep(day.number, each = 5),
  "producer" = rep(total.producers, 6),
  "habitat" = rep(c("littoral", "littoral", "littoral", "pelagic", "pelagic"), 6),
  "d15N" = c(
    5.8, 4.2, 8.6, 5.6, 3.2,
    5.8, 5.0, 6.0, 5.0, 2.8,
    5.0, 2.2, 5.0, 5.0, 5.0,
    4.2, 3.8, 6.0, 5.0, 5.6,
    3.0, 4.0, 5.4, 5.8, 5.4,
    NA, NA, NA, NA, NA
  )
)

cayuga <- data.frame(
  "lake" = rep('Cayuga', 30),
  "day" = rep(day.number, each = 5),
  "producer" = rep(total.producers, 6),
  "habitat" = rep(c("littoral", "littoral", "littoral", "pelagic", "pelagic"), 6),
  "d15N" = c(
    NA, 8.2, 5.4, 7.8, 6.4,
    NA, 8.0, 9.2, 6.6, 5.8,
    NA, 7.0, 9.4, 6.2, 6.0,
    NA, 6.6, 9.2, 7.0, 7.4,
    NA, 6.8, 5.8, 7.8, 6.8,
    NA, NA, NA, NA, NA
  )
)

oneida <- data.frame(
  "lake" = rep('Oneida', 30),
  "day" = rep(day.number, each = 5),
  "producer" = rep(total.producers, 6),
  "habitat" = rep(c("littoral", "littoral", "littoral", "pelagic", "pelagic"), 6),
  "d15N" = c(
    NA, 8.0, NA, 9.4, 4.8,
    NA, 7.0, 10.2, 2.8, 5.0,
    NA, 7.6, 6.4, 6.0, 3.8,
    NA, 7.4, 7.4, 5.2, 6.0,
    NA, 6.0, 5.8, 6.0, 7.0,
    NA, 5.4, 3.8, 3.0, 3.0
  )
)

producers.df <- data.table(rbind(spencer, cayuga, oneida))

consumers.df <- data.frame(
  "lake" = c('Spencer', 'Spencer', 'Cayuga', 'Cayuga', 'Oneida', 'Oneida'),
  "day" = rep(NA, 6),
  "consumer" = rep(c("snail", "mussel"), 3),
  "habitat" = rep(c("littoral", "pelagic"), 3),
  "d15N" = c(
    4.6, 5.2,
    9.6, 7.6,
    8.2, 5.4
  )
)

range.tef <- seq(0.5, 5.5, 0.5)

library(data.table)

test.df <- producers.df %>% 
  filter(habitat == "pelagic") 
  
test.df
group_by(test.df, producer)
  summarize("median_d15N" = median(d15N))

## I could not get the group_by() and summarize() functions to work, so I had to resort to this...

pelagic.producers.df <- data.frame(
  "lake" = c("Spencer", "Spencer", "Cayuga", "Cayuga", "Oneida", "Oneida"),
  "producer" = c("small.seston", "large.seston", "small.seston", "large.seston", "small.seston", "large.seston"),
  "median.d15N" = rep(NA, 6),
  "mussel.d15N" = c(5.2, 5.2, 7.6, 7.6, 5.4, 5.4)
)

pelagic.producers.df[1,3] <- producers.df %>% 
  filter(habitat == "pelagic" & 
           lake == "Spencer" & 
           producer == "small.seston") %>%
  summarize("median.d15N" = median(d15N, na.rm = T))

pelagic.producers.df[2,3] <- producers.df %>% 
  filter(habitat == "pelagic" & 
           lake == "Spencer" & 
           producer == "large.seston") %>%
  summarize("median.d15N" = median(d15N, na.rm = T))

pelagic.producers.df[3,3] <- producers.df %>% 
  filter(habitat == "pelagic" & 
           lake == "Cayuga" & 
           producer == "small.seston") %>%
  summarize("median.d15N" = median(d15N, na.rm = T))

pelagic.producers.df[4,3] <- producers.df %>% 
  filter(habitat == "pelagic" & 
           lake == "Cayuga" & 
           producer == "large.seston") %>%
  summarize("median.d15N" = median(d15N, na.rm = T))

pelagic.producers.df[5,3] <- producers.df %>% 
  filter(habitat == "pelagic" & 
           lake == "Oneida" & 
           producer == "small.seston") %>%
  summarize("median.d15N" = median(d15N, na.rm = T))

pelagic.producers.df[6,3] <- producers.df %>% 
  filter(habitat == "pelagic" & 
           lake == "Oneida" & 
           producer == "large.seston") %>%
  summarize("median.d15N" = median(d15N, na.rm = T))

tp.est.df <- data.frame(
  pelagic.producers.df,
  "tef_0.5" = rep(NA, 6),
  "tef_1.0" = rep(NA, 6),
  "tef_1.5" = rep(NA, 6),
  "tef_2.0" = rep(NA, 6),
  "tef_2.5" = rep(NA, 6),
  "tef_3.0" = rep(NA, 6),
  "tef_3.5" = rep(NA, 6),
  "tef_4.0" = rep(NA, 6),
  "tef_4.5" = rep(NA, 6),
  "tef_5.0" = rep(NA, 6),
  "tef_5.5" = rep(NA, 6)
)

i=1
j=1
for (i in 1:length(range.tef)) {
  for (j in 1:6) {
    tp.est.df[j,i+4] = ((tp.est.df$mussel.d15N[j] - tp.est.df$median.d15N[j])/range.tef[i]) + 1
  }
}

