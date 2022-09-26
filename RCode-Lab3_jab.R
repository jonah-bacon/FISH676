# =====================================================================
# Lab 3
# Jonah Bacon
# 21 Sep 2022
# FISH 676
# =====================================================================

library(readxl)
library(tidyverse)
library(ggplot2)

hall.df <- read_excel("HallEtAl2018-Data.xlsx")
str(hall.df)

hall.df$Method <- as.factor(hall.df$Method)
hall.df$Organism <- as.factor(hall.df$Organism)
names(hall.df) <- c("Method", "Organism", "Biomass", "Respiration")
hall.df1 <- hall.df %>% 
  unite(Method_Organism, Method, Organism, remove = FALSE)
hall.df1$Method_Organism <- as.factor(hall.df1$Method_Organism)

str(hall.df1)

ggplot(hall.df1, aes(x = log(Biomass), y = log(Respiration), color = Method_Organism)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  theme(
    panel.background = element_blank(),
    axis.title.x = element_text(size=16, vjust = 0),
    axis.title.y = element_text(size =16),
    axis.text = element_text(size=13, color="black"), 
    axis.line=element_line()
  )

hall.df2 <- hall.df1 %>% 
  mutate(Respiration_mg.g.day = Respiration/1000)

hall.df2$Respiration_mg.m2.day <- ifelse(hall.df2$Organism == "Invertebrate", 0.09*hall.df2$Respiration_mg.g.day, 
                                         0.29*hall.df2$Respiration_mg.g.day)

hall.df2$Respiration_cal.m2.day <- ifelse(hall.df2$Organism == "Fish", 0.35*hall.df2$Respiration_mg.m2.day, 
                                      hall.df2$Respiration_mg.m2.day)

hall.df2$Respiration_joules.m2.day <- hall.df2$Respiration_cal.m2.day*4.184

resp.table <- hall.df2[,-1]

names(resp.table) <- c(
  "Method",
  "Organism",
  "Biomass (g)",
  "Respiration (g O2/g/day)",
  "Respiration (mg O2/g/day)",
  "Respiration (mg O2/m2/day)",
  "Respiration (cal/m2/day)",
  "Respiration (J/m2/day)"
)

write.csv(resp.table,"LAB3.respiration.table.csv", row.names = FALSE)

production.table <- data.frame(hall.df2[,c(1:4,8)])
production.table$log10.Production_cal.m2.day <- ifelse(production.table$Organism == "Fish", 
                                                 log10(production.table$Respiration..cal.m2.day)*0.834 - 0.429, 
                                                 log10(production.table$Respiration..cal.m2.day)*0.978 - 0.06)
production.table$Production_cal.m2.day <- 10^production.table$log10.Production_cal.m2.day
production.table$Production_J.m2.day <- production.table$Production_cal.m2.day*4.184

prod.table <- production.table[,-1]
names(prod.table) <- c(
  "Method",
  "Organism",
  "Biomass (g)",
  "Respiration (cal/m2/day)",
  "log10[Production (cal/m2/day)]",
  "Production (cal/m2/day)",
  "Production (J/m2/day)"
)

write.csv(prod.table,"LAB3.production.table.csv", row.names = FALSE)

summary.production.table1 <- production.table %>% 
  group_by(Method_Organism, Organism) %>% 
  summarize("Total.Production_J.m2.day" = sum(Production_J.m2.day))

summary.respiration.table1 <- hall.df2 %>% 
  group_by(Method_Organism, Organism) %>% 
  summarize("Total.Respiration_J.m2.day" = sum(Respiration_joules.m2.day))

summary.production.table2 <- production.table %>% 
  group_by(Organism) %>% 
  summarize("Total.Production_J.m2.day" = sum(Production_J.m2.day))

summary.respiration.table2 <- hall.df2 %>% 
  group_by(Organism) %>% 
  summarize("Total.Respiration_J.m2.day" = sum(Respiration_joules.m2.day))


ggplot(data = summary.production.table1, aes(x = Organism, y = Total.Production_J.m2.day, fill = Method_Organism)) +
  geom_col(position = "stack")
