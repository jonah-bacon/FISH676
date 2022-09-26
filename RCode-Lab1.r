##### Aquatic Food Web Ecology Lab 1 #####
## Created 2022-08-29 by J.D. Muehlbauer, jdmuehlbauer@alaska.edu


##### Get started in R #####

## Install R
#https://cran.r-project.org/
	## NOTE: I am assuming you are working from R v4.x

## Install RStudio (optional)
#https://www.rstudio.com/
	## NOTE: I don't use RStudio. This is because I am a luddite and I don't like things crashing.
	## I recognize I am in the minority here. You can use RStudio, it's fine.
	## If you want an alternative, I love Notepad++, coupled with a little applet called NppToR.
	## Notepad++: https://notepad-plus-plus.org/
	## NppToR: https://sourceforge.net/projects/npptor/
	
## Some formatting guidance from a big name in the R world (H. Wickham)
#http://adv-r.had.co.nz/Style.html

## Set a working directory (optional)
setwd('C:/Users/jdmuehlbauer/Desktop/R')
	## NOTE: This is a simplistic example. Fit to your own file structure.
	## The working directory is wherever you choose to store your data for a given project, save results, etc.
	## You don't have to do this, but it helps keep your workflow clean.
	
## Install tidyverse (optional)
install.packages('tidyverse')
	## NOTE: tidyverse bundles common en vogue packages together. It's massive. 
	## I prefer to use these packages a la cart and install them individually.
	## Again, I'm a luddite. I don't like bloat. You can use tidyverse, it's fine.
	

##### Install/load packages #####

## Install devtools
	## NOTE: R package cheddar has not been pushed to CRAN for the release of R v4.0.
	## So, we need to rely on the package developer's site on GitHub (L. Hudson, AKA quicklizard99)
	## To install from GitHub, we need a package called devtools
install.packages('devtools')
require(devtools)

## Now we can download, install, and load cheddar
packageurl <- 'http://cran.r-project.org/src/contrib/Archive/cheddar/cheddar_0.1-636.tar.gz'
install.packages(packageurl, repos = NULL, type = 'source')
install_github(repo = 'quicklizard99/cheddar')


##### Play around in cheddar #####

## Here is the rdrr.io developer page (since its CRAN page is currently down):
#https://rdrr.io/cran/cheddar/

## The quickstart documentation is here:
#https://rdrr.io/cran/cheddar/f/inst/doc/CheddarQuickstart.pdf
	## NOTE: Other vignettes and package function info are available on the page as well.
	
## An entire paper about the package is here:
#https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12005


##### Analyze Tuesday Lake data #####
	
## Get Tuesday Lake 1984 test data
data(TL84)

## Take a cursory look at the structure of these data
class(TL84)
names(TL84)
	## NOTE: THese base R functions tell us that the class of the dataset is a "Community" class.
	## THis is a class attribute applied specially by cheddar that has no meaning outside of that package.
	## But the secondary class, "list" is extremely common. A list is a list of elements (often data frames).
	## Similarly, names tells us that the list has 3 elements, called nodes, properties, and trophic.links.
	## You can work with a list using [[x]] notation. For example, TL84[[nodes]] brings up the nodes element.
	## TL84$nodes also works as shorthand.
sapply(TL84, head)
	## NOTE: This is shorthand as well. I could instead have done the following:
	## head(TL84$nodes)
	## head(TL84$properties)
	## head(TL84$trophic.links)
	## From this, we can see that nodes and trophic.links are where the action is.
	
## Use a function to get the number of nodes (AKA species, in this case)
NumberOfNodes(TL84)

## Use a function to get the number of interactions (links) between these nodes
NumberOfTrophicLinks(TL84)


##### Plot the Tuesday Lake food web #####

## Plot basic horrendogram
PlotWebByLevel(TL84)
	## NOTE: this is OK, but lacks some basic features of plots like axes labels.
	## Let's look up help documentation for the function
?PlotWebByLevel

## Pretty up the horrendogram (oxymoron?)
PlotWebByLevel(TL84, ylab = 'Trophic level', xlab = 'species', main = '', ylim = c(0.9, 4.1))
box(bty = 'l')
axis(2, at = 1:4, las = 2, labels = rep('', 4))

## Make an Eltonian pyramid
	## NOTE: This one is up to you! I recommend you check out the NPS and plotNPS functions in cheddar.
	## Be aware that the columns M and N are biomass (M for mass) and abundance (N for number), respectively.
	## Your goal is to make a plot of total biomass by trophic level. It can look like a pyramid, or not.

	