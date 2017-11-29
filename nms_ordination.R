##  ----------------------------------------------------------------------------------------------------------  ##
            # Non-metric Multidimensional Scaling (NMS) Ordination Function
##  ----------------------------------------------------------------------------------------------------------  ##
# Code written by Nick Lyon
    ## refer questions to njlyon@iastate.edu if insoluble problems arise

# This function performs a non-metric multidimensional scaling (NMS) ordination for you

# Colors are colorblind safe and found here:
  # http://colorbrewer2.org/#type=sequential&scheme=GnBu&n=4

# Code written by Nick Lyon
  ## Updated 11/13/17

# Only need this library for the code to work
library(vegan)
  ## It's also included within the function, but better safe than sorry

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
factor <- as.vector(c(rep.int("a", (nrow(resp)/4)), rep.int("b", (nrow(resp)/4)), rep.int("c", (nrow(resp)/4)), rep.int("d", (nrow(resp)/4))))
ref <- cbind(factor, as.data.frame(resp))

# Run the model
mds <- metaMDS(resp, autotransform = F, expand = F, k = 2, try = 100)
  ## Where "resp" is the matrix of your community data (without grouping variables)

mds$stress
  ##  "Stress" is typically reported parenthetically for NMS ordinations,
  ## Similar to F statistics or p values

# Actual function (only works for four groups, but that is easily modified by you)
nms.ord <- function(mod, groupcol, g1, g2, g3, g4, legcont, legpos) {
  ## mod = object returned by metaMDS
  ## groupcol = group column in the dataframe that contains those (not the community matrix)
  ## g1 - g4 = how each group appears in your dataframe (in quotes)
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
  
  # Ordinate SD ellipses around the centroid
  library(vegan) # need this package for the following function
  ordiellipse(mod, groupcol, 
              col = c(g1 = col1, g2 = col2, g3 = col3, g4 = col4),
              display = "sites", kind = "sd", lwd = 2, label = F)
  
  # Add legend
  legend(legpos, legend = legcont, bty = "n", 
         pch = c(21, 22, 23, 24), cex = 1.15, 
         pt.bg = c(col1, col2, col3, col4))
  
}

# Example syntax
nms.ord(mds, # object returned by metaMDS
        ref$factor, # grouping column of the dataframe
        "a", "b", "c", "d", # entries for groups 1 through 4
        c("A", "B", "C", "D"), # entry for legcont (must be single object, hence the "c(...)")
        # This is separate (rather than concatenating g1-4 in the function) to allow you to change spelling/casing
        "bottomright") # legend position shorthand

# Saving procedure
jpeg(file = "./Custom Fxn Test Plots/NMS_DummyOrd.jpg") # for saving

nms.ord(mds, ref$factor, "a", "b", "c", "d", c("A", "B", "C", "D"), "bottomright")

dev.off() # for saving

# NOTE:
  ## While NMS can be useful in visualizing multivariate differences, it is NOT an analytical tool!
  ## Use of an actual multivariate analytical test is necessary if differences among groups are of interest

# Example with perMANOVA (permutational Multivariate ANOVA)
#install.packages("geomorph")
library(geomorph)

procD.lm(resp ~ factor, data = ref) # at least one group is significantly different from the others

advanced.procD.lm(resp ~ factor, ~ 1, ~ factor, data = ref)
# syntax is (Y ~ x, ~ null hypothesis, ~ grouping variable for pairwise comparisons, data = data)



