# =====================================================================
# Lab 4
# Jonah Bacon
# 21 Sep 2022
# FISH 676
# =====================================================================

## Load libraries

library(tidyverse)
library(tidyr)
library(dplyr)

## Part 1

preston.df <- read.csv("Preston2019Data-Grad.csv")
head(preston.df)
str(preston.df)

preston.df$Stream <- as.factor(preston.df$Stream)
preston.df$Reach <- as.factor(preston.df$Reach)
preston.df$TaxaIdentifier <- as.factor(preston.df$TaxaIdentifier)
preston.df$Order <- as.factor(preston.df$Order)

str(preston.df)

aggregated.df <- preston.df %>% 
  group_by(TaxaIdentifier) %>% 
  summarize(
    "TotalPreyCount" = sum(PreyCount),
    "TotalPreyDryMass" = sum(PreyDryMass),
    "TotalPredDryMass" = sum(PredDryMass),
    "TotalPredCount" = sum(PredCount),
    "GutFullness" = sum(((PreyDryMass/PredDryMass)*PredCount))/TotalPredCount,
    "AverageFeedingRate" = sum(FeedingRate*PredCount)/TotalPredCount
    )

library(bipartite)

total.prey.count.mat <- matrix(aggregated.df$TotalPreyCount)
row.names(total.prey.count.mat) <- aggregated.df$TaxaIdentifier
colnames(total.prey.count.mat) <- "Count"

total.prey.mass.mat <- matrix(aggregated.df$TotalPreyDryMass)
row.names(total.prey.mass.mat) <- aggregated.df$TaxaIdentifier
colnames(total.prey.mass.mat) <- "Mass"

gut.fullness.mat <- matrix(aggregated.df$GutFullness)
row.names(gut.fullness.mat) <- aggregated.df$TaxaIdentifier
colnames(gut.fullness.mat) <- "Fullness"

average.feeding.rate.mat <- matrix(aggregated.df$AverageFeedingRate)
row.names(average.feeding.rate.mat) <- aggregated.df$TaxaIdentifier
colnames(average.feeding.rate.mat) <- "Feeding_rate"

par(mfrow=c(4,1))

plotweb(total.prey.count.mat)
plotweb(total.prey.mass.mat)
plotweb(gut.fullness.mat)
plotweb(average.feeding.rate.mat)

dev.off()

## Part 2

library(igraph)
library(readxl)

dalsgaard.df <- read.csv("Dalsgaard1997Data-Grad.csv", row.names = 1)
head(dalsgaard.df)

dals.mat <- data.matrix(dalsgaard.df)

ex1 <- graph_from_adjacency_matrix(dals.mat, weight = TRUE)

plot(ex1, layout = layout.circle, edge.arrow.size = 0.2)

dal.position <- read_excel("Dalsgaard1997Data-Undergrad.xlsx", sheet = 1)
dal.matrix <- read_excel("Dalsgaard1997Data-Undergrad.xlsx", sheet = 2)
dal.links <- read_excel("Dalsgaard1997Data-Undergrad.xlsx", sheet = 3)

E(ex1)$width <- ifelse(E(ex1)$weight>0.8, 10, 
                       ifelse(E(ex1)$weight>0.6, 7, 
                              ifelse(E(ex1)$weight>0.4, 4, 
                                      ifelse(E(ex1)$weight>0.2, 2, 1))))
plot(ex1, layout = layout.circle, edge.arrow.size = 0.2)


cols <- c(
  rep('red',3),
  rep('green',5),
  rep('blue',6),
  rep('orange',5)
)
V(ex1)$label.color <- cols

V(ex1)$vertex.label.cex <- 5

E(ex1)$color <- "black"

plot(ex1, layout=layout.circle, edge.arrow.size = 0.2, vertex.shape="none",
     vertex.size = 5, vertex.color = "black", vertex.label.cex=2)

ddd <- c(0,-pi/10,2*-pi/10,3*-pi/10,4*-pi/10,5*-pi/10,6*-pi/10,7*-pi/10,8*-pi/10,9*-pi/10,
         9*pi/10,8*pi/10,7*pi/10,6*pi/10,5*pi/10,4*pi/10,3*pi/10,2*pi/10,pi/10)
plot(ex1, layout=layout.circle, edge.arrow.size = 0.2, vertex.shape="none",
     vertex.size = 5, vertex.color = "black", vertex.label.cex=2, vertex.label.dist = 2,
     vertex.label.degree = ddd)
