##  ----------------------------------------------------------------------------------------------------------  ##
                  # Principle Coordinates Analysis (PCoA) Ordination Function
##  ----------------------------------------------------------------------------------------------------------  ##
# Code written by Nick Lyon
  ## Updated 2020, January 14

# This function makes an aesthetically pleasing PCoA ordination for you

# Colors are colorblind safe and found here:
  # http://colorbrewer2.org/#type=sequential&scheme=GnBu&n=4

# Clear the environment so the function doesn't catch on something strange and user-specific
rm(list = ls())

# Also, set your working directory to the project folder
myWD <- getwd()
myWD

# Required libraries
library(RRPP) # Need this library for null hypothesis testing (pre-ordination)
library(vegan) # Need this library for distance matrix calculation
library(ape) # Need this library for pcoa performance

## --------------------------------------  ##
             # Data Prep ####
## --------------------------------------  ##
# Let's use some of the data from the vegan package to demonstrate the function
data("varespec")
resp <- varespec

# Data on lichen "pastures", can check it out with: 
?varespec

# We do need a group column though for this ordination function
factor <- as.vector(c(rep.int("Trt1", (nrow(resp)/4)),
                      rep.int("Trt2", (nrow(resp)/4)),
                      rep.int("Trt3", (nrow(resp)/4)),
                      rep.int("Trt4", (nrow(resp)/4))) )
ref <- cbind(factor, as.data.frame(resp))

## --------------------------------------  ##
              # Analysis #### 
## --------------------------------------  ##

# ANALYSIS NOTE:
  ## While PCoA can be useful in visualizing multivariate differences, it is NOT a null hypothesis test!
  ## Use of an actual multivariate analytical test is necessary if differences among groups are of interest
  ## Feel free to skip through this section if you don't need/want help with multivariate analysis

# Initial perMANOVA
anova(lm.rrpp(resp ~ factor, data = ref), effect.type = "F")
  ## Interpretation: at least one group is significantly different from the others
  ## Yes these groups don't mean anything, but it's still illustrative.

# In reality would need to track down which groups are different from which (pairwise comparisons)
  ## but we'll leave that alone here

## --------------------------------------  ##
                # PCoA ####
## --------------------------------------  ##

# Get a distance matrix from the data
dist <- vegdist(resp, method = 'kulczynski')
  ## Bray-Curtis (among others) is non-metric which loses you the benefit of PCoA over NMS
  ## TLDR: pick your distance method with care

# Perform a PCoA on the distance matrix to get points for an ordination
pnts <- pcoa(dist)

# Check out that object just so you know what you could access if need be
str(pnts)

# Make the basic biplot that is built into "ape"
biplot(pnts)
  ## Serviceable, but not exactly gorgeous...

## --------------------------------------  ##
        # 4-Group Ordination ####
## --------------------------------------  ##
# PCoA ordination function
pcoa.4.ord <- function(mod, groupcol, g1, g2, g3, g4, 
                    lntp1 = 1, lntp2 = 1, lntp3 = 1, lntp4 = 1,
                    legcont, legpos = "topleft") {
  ## mod = object returned by ape::pcoa
  ## groupcol = group column in the dataframe that contains those (not the matrix used in vegdist)
  ## g1 - g4 = how each group appears in your dataframe (in quotes)
  ## lntp1 - 4 = what sort of line each ellipse will be made of (accepts integers between 1 and 6 for diff lines)
  ## legcont = single object for what you want the content of the legend to be
  ## legpos = legend position, either numeric vector of x/y coords or shorthand accepted by "legend" function
  
  # Create plot
  plot(mod$vectors, display = 'sites', choice = c(1, 2), type = 'none',
       xlab = paste0("PC1 (", round(mod$values$Relative_eig[1] * 100, digits = 2), "%)"),
       ylab = paste0("PC2 (", round(mod$values$Relative_eig[2] * 100, digits = 2), "%)"))
    ## Probably want the relative eigenvalues (% variation explained per axis) on the plot in an obvious way
  
  # Set colors (easier for you to modify if we set this now and call these objects later)
  col1 <- "#fee090" # yellow
  col2 <- "#d73027" # red
  col3 <- "#abd9e9" # light blue
  col4 <- "#4575b4" # blue
  
  # Add points for each group with a different color per group
  points(mod$vectors[groupcol == g1, 1], mod$vectors[groupcol == g1, 2], pch = 21, bg = col1)
  points(mod$vectors[groupcol == g2, 1], mod$vectors[groupcol == g2, 2], pch = 22, bg = col2)
  points(mod$vectors[groupcol == g3, 1], mod$vectors[groupcol == g3, 2], pch = 23, bg = col3)
  points(mod$vectors[groupcol == g4, 1], mod$vectors[groupcol == g4, 2], pch = 24, bg = col4)
  ## As of right now the colors are colorblind safe and each group is also given its own shape
  
  # Get a single vector of your manually set line types for the ellipses
  lntps <- c(lntp1, lntp2, lntp3, lntp4)
  
  # Ordinate SD ellipses around the centroid
  vegan::ordiellipse(mod$vectors, groupcol, 
                     col = c(g1 = col1, g2 = col2, g3 = col3, g4 = col4),
                     display = "sites", kind = "sd", lwd = 2, lty = lntps, label = F)
  
  # Add legend
  legend(legpos, legend = legcont, bty = "n", 
         title = NULL,  cex = 1.15, 
         pch = c(21, 22, 23, 24),
         pt.bg = c(col1, col2, col3, col4))
  
}

# Example syntax
pcoa.4.ord(pnts, # object returned by ape::pcoa
        ref$factor, # grouping column of the dataframe
        g1 = "Trt1", g2 = "Trt2", g3 = "Trt3", g4 = "Trt4", # entries for groups 1 through 4
        lntp1 = 1, lntp2 = 1, lntp3 = 1, lntp4 = 5, # manual settings for ellipse line types
        legcont = c("1", "2", "3", "4"), # entry for legcont (must be single object, hence the "c(...)")
## This is separate (rather than concatenating g1-4 in the function) to allow you to change spelling/casing
        "topleft") # legend position shorthand

# Saving procedure
jpeg(file = "./Test Plots/PCoA_DummyOrd.jpg") # for saving

pcoa.4.ord(pnts, ref$factor, "Trt1", "Trt2", "Trt3", "Trt4", 1, 1, 1, 5, c("1", "2", "3", "4"), "topleft")

dev.off() # for saving

## --------------------------------------  ##
        # 3-Group Ordination ####
## --------------------------------------  ##
# Remove one of the groups and re-level the factor to remove it from the factor list
ref2 <- subset(ref, ref$factor != "Trt3")
ref2$factor <- as.factor(as.character(ref2$factor))

# Get the community matrix again
resp2 <- ref2[,-1]

# Get a new distance matrix
dist2 <- vegdist(resp2, method = 'kulczynski')

# Do PCoA!
pnts2 <- pcoa(dist2)

# 3-group PCoA ordination function
pcoa.3.ord <- function(mod, groupcol, g1, g2, g3,
                       lntp1 = 1, lntp2 = 1, lntp3 = 1,
                       legcont, legpos = "topleft") {
  ## mod = object returned by ape::pcoa
  ## groupcol = group column in the dataframe that contains those (not the matrix used in vegdist)
  ## g1 - g3 = how each group appears in your dataframe (in quotes)
  ## lntp1 - 3 = what sort of line each ellipse will be made of (accepts integers between 1 and 6 for diff lines)
  ## legcont = single object for what you want the content of the legend to be
  ## legpos = legend position, either numeric vector of x/y coords or shorthand accepted by "legend" function
  
  # Create plot
  plot(mod$vectors, display = 'sites', choice = c(1, 2), type = 'none',
       xlab = paste0("PC1 (", round(mod$values$Relative_eig[1] * 100, digits = 2), "%)"),
       ylab = paste0("PC2 (", round(mod$values$Relative_eig[2] * 100, digits = 2), "%)"))
  ## Probably want the relative eigenvalues (% variation explained per axis) on the plot in an obvious way
  
  # Set colors (easier for you to modify if we set this now and call these objects later)
  col1 <- "#fee090" # yellow
  col2 <- "#d73027" # red
  col3 <- "#4575b4" # blue
  
  # Add points for each group with a different color per group
  points(mod$vectors[groupcol == g1, 1], mod$vectors[groupcol == g1, 2], pch = 21, bg = col1)
  points(mod$vectors[groupcol == g2, 1], mod$vectors[groupcol == g2, 2], pch = 22, bg = col2)
  points(mod$vectors[groupcol == g3, 1], mod$vectors[groupcol == g3, 2], pch = 23, bg = col3)
  ## As of right now the colors are colorblind safe and each group is also given its own shape
  
  # Get a single vector of your manually set line types for the ellipses
  lntps <- c(lntp1, lntp2, lntp3)
  
  # Ordinate SD ellipses around the centroid
  vegan::ordiellipse(mod$vectors, groupcol, 
                     col = c(g1 = col1, g2 = col2, g3 = col3),
                     display = "sites", kind = "sd", lwd = 2, lty = lntps, label = F)
  
  # Add legend
  legend(legpos, legend = legcont, bty = "n", 
         title = NULL,  cex = 1.15, 
         pch = c(21, 22, 23),
         pt.bg = c(col1, col2, col3))
  
}

# Example syntax
pcoa.3.ord(pnts2, # object returned by ape::pcoa
           ref2$factor, # grouping column of the dataframe
           g1 = "Trt1", g2 = "Trt2", g3 = "Trt4", # entries for groups 1 through 4
           lntp1 = 1, lntp2 = 1, lntp3 = 4, # manual settings for ellipse line types
           legcont = c("1", "2", "4"), # entry for legcont (must be single object, hence the "c(...)")
           ## This is separate (rather than concatenating g1-4 in the function) to allow you to change spelling/casing
           "topleft") # legend position shorthand

# Saving procedure
jpeg(file = "./Test Plots/PCoA_DummyOrd2.jpg") # for saving

pcoa.3.ord(pnts2, ref2$factor, "Trt1", "Trt2", "Trt4", 1, 1, 4, c("1", "2", "4"), "topleft")

dev.off() # for saving

# END ####
