##### Aquatic Food Web Ecology Lab 9 #####
## Created 2022-11-01 by J.D. Muehlbauer, jdmuehlbauer@alaska.edu


##### Prepare workspace #####

## Set working directory
# setwd('C:/Users/jmuehlbauer/Desktop/Lab9')

## Load required packages
#install.packages('devtools')
require(devtools)
#install.packages('vegan')
require(vegan)
# install_github(repo = "jmuehlbauer-usgs/R-packages", subdir = "bugR")
require(bugR)
require(tidyverse)
require(ggsci)

## Load data
ord1 <- read.csv('OrdinationMatrix.csv', row.names = 1)
env1 <- read.csv('EnvMatrix.csv', row.names = 1)
bug1 <- read.csv('Specimens.csv')


##### Assess temporal changes in functional feeding groups #####

## Plot by FFG (all groups)
str(bug1)
head(bug1)

ggplot(bug1, aes(x = Day, y = Biomass)) +
  geom_point(aes(color = Trophic, shape = Trophic), cex = 3) +
  geom_smooth(aes(color = Trophic), se = F, method = "lm") +
  scale_shape_manual(values=c(3, 16, 17, 9)) +
  scale_color_manual(values = c(1,2,3,4)) +
  ylab("Biomass (mg)") +
  xlab("Days post-flood") +
  scale_x_continuous(limits = c(1,29), breaks = seq(0, 30, 5), expand = c(0,0)) +
  theme(
    panel.background = element_blank(),
    axis.line=element_line()
    )

## Exclude Detritivores to see other patterns more clearly

bug1 %>% 
  filter(Trophic != "Detritivore") %>% 
ggplot(aes(x = Day, y = Biomass)) +
  geom_point(aes(color = Trophic, shape = Trophic), cex = 3) +
  geom_smooth(aes(color = Trophic), se = F, method = "lm") +
  scale_shape_manual(values=c(16, 17, 9)) +
  scale_color_manual(values = c(2,3,4)) +
  ylab("Biomass (mg)") +
  xlab("Days post-flood") +
  scale_x_continuous(limits = c(1,29), breaks = seq(0, 30, 5), expand = c(0,0)) +
  theme(
    panel.background = element_blank(),
    axis.line=element_line()
  )

##### Prepare dataset for ordination #####

## Get coefficient of variation in data by rows and columns
cv(ord1)
	## NOTE: Ideally (according to McCune & Grace 2002 textbook), we want <100 for both
	## Column cv violates this heavily. Let's deal with this.
	
## Delete rare species
ord2 <- delRare(ord1)
cv(ord2)
	## NOTE: Default here is getting rid of species not present in >5% of samples
	## CV reduced substantially, but still unacceptably high.
	
## Relativize abundance by column (species)
ord3 <- bugR::rel(ord2)
cv(ord3)
	## NOTE: This will turn every column into a vector of proportion data, from 0 to 1.
	## That gets our CV down some more. Good!


##### Run ordinations #####

## Run a quick and dirty ordination to create a step down plot
NMS(ord3,maxruns=1000,stepdown = TRUE)
	## NOTE: This helps identify the ideal dimensionality of the ordination (usually 2D or 3D).
	## In these plots, lower stress is better, but we are looking for an inflection point.
	## The NMS function creates a folder called "NMS Output" in your working directory where
	## it saves the NMS output (matrices of output species and site data).

## Run a full ordination using only 2 (and 3) dimensions
NMS(ord3, maxruns = 10000, stepdown = FALSE, only23 = TRUE)

## Read in site and species point data from ordination output and plot
site1 <- read.csv('NMS Output/NMSPoints2D.csv', row.names = 1)
spp1 <- read.csv('NMS Output/NMSSpecies2D.csv', row.names = 1)
plot(site1)
	## NOTE: Great, we have a plot. But what does it mean? Let's dig deeper.
	

##### Fit environmental data to ordination #####

## Compute fit, get vectors
fit1 <- envfit(site1, env1)
	## NOTE: Some of our environmental parameters seem not very important to the ordination.

## Get R2 of ordination
axisR2(site1, ord3)

## Rotate ordination axes to align Axis 1 along strongest environmental gradient.
rot1 <- ordRotate(site1, fit1, 'DistancePrimary', x.axis = FALSE, flip = TRUE)
fit2 <- envfit(rot1, env1)
plot(rot1)

## Overlay environmental vectors to create a biplot
for(i in 1:2){
	arrows(0, 0, 0.9 * fit2$vectors$arrows[i,1], 0.9 * fit2$vectors$arrows[i,2], lty = i + 1)
}
legend('topright', legend = c('Day post-flood', 'Distance from primary refugia'), lty = 2:3,
	bty = 'n')

## Add ellipses for site groupings
col1 <- c('blue', 'green', 'orange', 'red')
lty1 <- c(1, 5, 2, 3)
grp1 <- LETTERS[1:4]
ordiellipse(rot1, env1$SiteGroup, col = col1, lty = lty1)
legend('topleft', legend = c('Near', 'Mid-Near', 'Mid-Far', 'Far'), lty = lty1, col = col1, 
	bty = 'n')
	
## Re-plot for time groupings, with no individual points (for cleanliness)
plot(rot1, type = 'n')
col2 <- c('darkred', 'red', 'orange', 'green', 'darkgreen', 'blue', 'purple')
lty2 <- c(7, 6, 5, 4, 3, 2, 1)
ordiellipse(rot1, env1$Day, col = col2, lty = lty2)
legend('topright', legend = paste0('Day', unique(env1$Day)), lty = lty2, col = col2, 
	bty = 'n')


##### Compute statistics on the ordination groupings #####

## Overall PERMANOVA tests by distnace from refugia and by time post-flood
adonis2(ord3 ~ env1$SiteGroup)
adonis2(ord3 ~ as.factor(env1$Day))

## Comparison of differences between individual sites
name1 <- c('Near', 'MidNear', 'MidFar', 'Far')
comp1 <- LETTERS[c(1,1,1,2,2,3)]
comp2 <- LETTERS[c(2,3,4,3,4,4)]
comp3 <- name1[c(1,1,1,2,2,3)]
comp4 <- name1[c(2,3,4,3,4,4)]
perm1 <- data.frame(Comparison = paste(comp3, comp4, sep = '-'), F = NA, p = NA)
for(i in 1:length(comp1)){
	t1 <- env1$SiteGroup %in% c(comp1[i], comp2[i])
	t2 <- adonis2(ord3[t1,] ~ env1$SiteGroup[t1])
	perm1$F[i] <- round(t2$F[1], 4)
	perm1$p[i] <- t2$P[1]
}

## Comparison of differences between individual days
comp5 <- as.factor(c(rep(2, 6), rep(4, 5), rep(8, 4), rep(10, 3), rep(13, 2), rep(18, 1)))
comp6 <- as.factor(c(4, 8, 10, 13, 18, 27, 8, 10, 13, 18, 27, 10, 13, 18, 27, 
	13, 18, 27, 18, 27, 27))
perm2 <- data.frame(Comparison = paste(comp5, comp6, sep = '-'), F = NA, p = NA)
for(i in 1:length(comp5)){
	t1 <- env1$Day %in% c(comp5[i], comp6[i])
	t2 <- adonis2(ord3[t1,] ~ env1$Day[t1])
	perm2$F[i] <- round(t2$F[1], 4)
	perm2$p[i] <- t2$P[1]
}

## Comparison of dispersion differences between individual sites
dist1 <- vegdist(ord3)
disp1 <- betadisper(dist1, group = env1$SiteGroup)
anova1 <- anova(disp1)
permutest1 <- permutest(disp1, pairwise = TRUE)
tukey1 <- TukeyHSD(disp1)

## Comparison of dispersion differences between individual days
disp2 <- betadisper(dist1, group = as.factor(env1$Day))
anova2 <- anova(disp2)
permutest2 <- permutest(disp2, pairwise = TRUE)
tukey2 <- TukeyHSD(disp2)
