##  ----------------------------------------------------------------------------------------------------------  ##
            # Non-metric Multidimensional Scaling (NMS) Ordination Function
##  ----------------------------------------------------------------------------------------------------------  ##
# Code written by Nick Lyon
  ## Updated 2018, April 2

# This function performs a non-metric multidimensional scaling (NMS) ordination for you

# Colors are colorblind safe and found here:
  # http://colorbrewer2.org/#type=sequential&scheme=GnBu&n=4

# Required libraries
#install.packages(c("vegan", "geomorph")) # delete the first hashtag and this line will install the packages
library(vegan) # Need this library for NMS
  ## It's also included within the function, but better safe than sorry
library(geomorph) # Need this library for analysis

# START ####

# Clear the environment so the function doesn't catch on something strange and user-specific
rm(list = ls())

# Also, set your working directory to the location where your project is (and delete my WD code here)
setwd("~/Documents/School/Misc R/Custom Functions")
  ## "Session" menu at top of screen -> "Set Working Directory" -> "To Project Directory"

# Let's use some of the data from the vegan package to demonstrate the function
data("varespec")
resp <- varespec
  ## Data on lichen "pastures", can check it out with: 
?varespec

# We do need a group column though for this ordination function
factor <- as.vector(c(rep.int("Trt1", (nrow(resp)/4)),
                      rep.int("Trt2", (nrow(resp)/4)),
                      rep.int("Trt3", (nrow(resp)/4)),
                      rep.int("Trt4", (nrow(resp)/4))) )
ref <- cbind(factor, as.data.frame(resp))

# Analysis #### 

# ANALYSIS NOTE:
  ## While NMS can be useful in visualizing multivariate differences, it is NOT an analytical tool!
  ## Use of an actual multivariate analytical test is necessary if differences among groups are of interest
  ## Feel free to skip through this section if you don't need/want help with multivariate analysis

# Initial perMANOVA
procD.lm(resp ~ factor, data = ref)
  ## Interpretation: at least one group is significantly different from the others
  ## Yes these groups don't mean anything, but it's still illustrative.

# Want to know *which* groups were different from the others
advanced.procD.lm(resp ~ factor, ~ 1, ~ factor, data = ref)
  ## syntax is (Y ~ x, ~ null hypothesis, ~ grouping variable for pairwise comparisons, data = data)

# Significance groups (different letter means significantly different)
# Trt1 = A | Trt2 = A | Trt3 = A | Trt4 = B
  # Only treatment 4 was different from all others

# NMS ####

# Run the model
mds <- metaMDS(resp, autotransform = F, expand = F, k = 2, try = 100)
  ## Where "resp" is the matrix of your community data (without grouping variables)

mds$stress
  ##  "Stress" is typically reported parenthetically for NMS ordinations,
  ## Similar to F statistics or p values
  ## Clarke et al 1993 suggests stress ≤ 0.15 to be a good threshold/rule of thumb

# Actual function (only works for four groups, but that is easily modified by you)
nms.ord <- function(mod, groupcol, g1, g2, g3, g4, 
                    lntp1 = 1, lntp2 = 1, lntp3 = 1, lntp4 = 1,
                    legcont, legpos = "topright") {
  ## mod = object returned by metaMDS
  ## groupcol = group column in the dataframe that contains those (not the community matrix)
  ## g1 - g4 = how each group appears in your dataframe (in quotes)
  ## lntp1 - 4 = what sort of line each ellipse will be made of (accepts integers between 1 and 6 for diff lines)
  ## legcont = single object for what you want the content of the legend to be
  ## legpos = legend position, either numeric vector of x/y coords or shorthand accepted by "legend" function
  
  # Create plot
  plot(mod, display = 'sites', choice = c(1, 2), type = 'none', xlab = "", ylab = "")
  
  # Set colors (easier for you to modify if we set this now and call these objects later)
  col1 <- "#fee090" # yellow
  col2 <- "#d73027" # red
  col3 <- "#abd9e9" # light blue
  col4 <- "#4575b4" # blue
  
  
  # Add points for each group with a different color per group
  points(mod$points[groupcol == g1, 1], mod$points[groupcol == g1, 2], pch = 21, bg = col1)
  points(mod$points[groupcol == g2, 1], mod$points[groupcol == g2, 2], pch = 22, bg = col2)
  points(mod$points[groupcol == g3, 1], mod$points[groupcol == g3, 2], pch = 23, bg = col3)
  points(mod$points[groupcol == g4, 1], mod$points[groupcol == g4, 2], pch = 24, bg = col4)
  ## As of right now the colors are colorblind safe and each group is also given its own shape
  
  # Get a single vector of your manually set line types for the ellipses
  lntps <- c(lntp1, lntp2, lntp3, lntp4)
  
  # Ordinate SD ellipses around the centroid
  library(vegan) # need this package for the following function
  ordiellipse(mod, groupcol, 
              col = c(g1 = col1, g2 = col2, g3 = col3, g4 = col4),
              display = "sites", kind = "sd", lwd = 2, lty = lntps, label = F)
  
  # Add legend
  legend(legpos, legend = legcont, bty = "n", 
         pch = c(21, 22, 23, 24), cex = 1.15, 
         pt.bg = c(col1, col2, col3, col4))
  
}

# Example syntax
nms.ord(mds, # object returned by metaMDS
        ref$factor, # grouping column of the dataframe
        g1 = "Trt1", g2 = "Trt2", g3 = "Trt3", g4 = "Trt4", # entries for groups 1 through 4
        lntp1 = 1, lntp2 = 1, lntp3 = 1, lntp4 = 5, # manual settings for ellipse line types
        legcont = c("1", "2", "3", "4"), # entry for legcont (must be single object, hence the "c(...)")
## This is separate (rather than concatenating g1-4 in the function) to allow you to change spelling/casing
        "bottomright") # legend position shorthand

# Saving procedure
jpeg(file = "./Custom Fxn Test Plots/NMS_DummyOrd.jpg") # for saving

nms.ord(mds, ref$factor, "Trt1", "Trt2", "Trt3", "Trt4", 1, 1, 1, 5, c("1", "2", "3", "4"), "bottomright")

dev.off() # for saving


# END ####
