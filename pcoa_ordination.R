##  --------------------------------------------------------------------  ##
        # Principle Coordinates Analysis (PCoA) Ordination Function
##  --------------------------------------------------------------------  ##
# Code written by Nick J Lyon
## Updated 2022, 8 March

# Purpose:
## Create publication-quality principal coordinates analysis ordinations

# Required libraries
library(vegan); library(RRPP); library(tidyverse); library(ape)

# Clear the environment
rm(list = ls())

# Also, set your working directory to the project folder
myWD <- getwd()
myWD

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

# Make this into a 'rrpp.data.frame' as is now required by RRPP
ref.rdf <- rrpp.data.frame("factor" = factor, "resp" = as.matrix(resp))
str(ref.rdf)

# Make a dataframe that includes different numbers of groups
data <- resp %>%
  # Make factors of varying numbers of groups
  mutate(
    # 6 factor levels
    factor_6lvl = c(rep.int("Trt1", (nrow(resp)/6)),
                    rep.int("Trt2", (nrow(resp)/6)),
                    rep.int("Trt3", (nrow(resp)/6)),
                    rep.int("Trt4", (nrow(resp)/6)),
                    rep.int("Trt5", (nrow(resp)/6)),
                    rep.int("Trt6", (nrow(resp)/6))),
    # 4 levels
    factor_4lvl = factor,
    # 2 levels
    factor_2lvl = c(rep.int("Trt1", (nrow(resp)/2)),
                    rep.int("Trt2", (nrow(resp)/2))),
    factor_over = 1:nrow(resp),
    .before = everything())

# Check contents
names(data)
unique(data$factor_6lvl)
unique(data$factor_4lvl)
unique(data$factor_2lvl)

## --------------------------------------  ##
              # Analysis #### 
## --------------------------------------  ##

# ANALYSIS NOTE:
  ## While PCoA can be useful in visualizing multivariate differences, it is NOT a null hypothesis test!
  ## Use of an actual multivariate analytical test is necessary if differences among groups are of interest
  ## Feel free to skip through this section if you don't need/want help with multivariate analysis

# Initial perMANOVA
anova(RRPP::lm.rrpp(resp ~ factor, data = ref.rdf), effect.type = "F")
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
      # PCoA Ordination Function ####
## --------------------------------------  ##

# Get our PCoA function (works up to 10 groups)
pcoa_ord <- function(mod, groupcol, title = NA,
                    colors = c('#c51b7d', '#7fbc41', '#d73027', '#4575b4',
                               '#e08214', '#8073ac', '#f1b6da', '#b8e186',
                               '#8c96c6', '#41b6c4'),
                    lines = rep(1, 10),
                    leg_pos = 'bottomleft', leg_cont = unique(groupcol)) {
  # Argument descriptions
  ## mod = object returned by vegan::metaMDS
  ## groupcol = column in the data that includes the groups
  ## title = character vector to use as title for plot
  ## colors = vector of colors (as hexadecimal codes) of length >= group levels (currently *not* colorblind safe because of need for 10 unique colors)
  ## lines = vector of line types (as integers) of length >= group levels
  ## leg_pos = legend position, either numeric vector of x/y coordinates or shorthand accepted by "legend" function
  ## leg_cont = concatenated vector of what you want legend content to be; defaults to entries in group column of data (option provided in case syntax of legend contents should differ from data contents)
  
  # Limiting (for now) to only 10 groups
  if (length(unique(groupcol)) > 10) {
    
    ## Prints an informative message if too many groups
    print('Plotting >10 groups is not supported. Run `unique` on your factor column if you believe there are fewer than 10 groups')
    
  } else {
    
    # Before actually creating the plot we need to make sure our colors/shapes/lines are correctly formatted
    
    # Create vector of shapes
    shapes <- c(21, 22, 23, 24, 25, 21, 22, 23, 24, 25)
    
    # Identify the names of the groups in the data
    groups <- as.vector(unique(groupcol))
    
    # Assign names to the vectors of colors/shapes/lines
    names(colors) <- groups
    names(shapes) <- groups
    names(lines) <- groups
    
    # Crop all three vectors to the length of groups in the data
    colors_actual <- colors[!is.na(names(colors))]
    shapes_actual <- shapes[!is.na(names(shapes))]
    lines_actual <- lines[!is.na(names(lines))]
    
    # Continue on to the actual plot creation
    
    # Create blank plot
    plot(mod$vectors,
         # display = 'sites', choice = c(1, 2), type = 'none',
         main = title,
         xlab = paste0("PC1 (", round(mod$values$Relative_eig[1] * 100, digits = 2), "%)"),
         ylab = paste0("PC2 (", round(mod$values$Relative_eig[2] * 100, digits = 2), "%)"))

    # Create a counter set to 1 (we'll need it in a moment)
    k <- 1
    
    # For each group, add points of a unique color and (up to 5 groups) unique shape (only 5 hollow shapes are available so they're recycled 2x each)
    for(level in unique(groupcol)){
      points(mod$vectors[groupcol == level, 1], mod$vectors[groupcol == level, 2],
             pch = shapes_actual[k], bg = colors_actual[k])
      
      # After each group's points are created, advance the counter by 1 to move the earlier part of the loop to a new color/shape
      k <- k + 1 }
    
    # With all of the points plotted, add ellipses of matched colors
    # This also allows for variation in line type if desired
    vegan::ordiellipse(mod$vectors, groupcol, col = colors_actual,
                       display = 'sites', kind = 'sd', lwd = 2,
                       lty = lines_actual, label = F)
    
    # Finally, add a legend
    legend(leg_pos, legend = leg_cont, bty = "n", title = NULL,
           pch = shapes_actual, cex = 1.15, pt.bg = colors_actual)
    
  }
}

# Test it for two groups
pcoa_ord(mod = pnts, groupcol = data$factor_2lvl, leg_pos = 'bottomleft')

# And again for 4
pcoa_ord(pnts, data$factor_4lvl)

# One more time for 6 levels
pcoa_ord(pnts, data$factor_6lvl, leg_pos = 'topleft',
         leg_cont = c('1', '2', 'Group 3', '4', '5', '6'))

# Save some of these
## 4
jpeg(file = "./Test Plots/PCoA_DummyOrd.jpg")
pcoa_ord(pnts, data$factor_4lvl)
dev.off()
## 6
jpeg(file = "./Test Plots/PCoA_DummyOrd2.jpg")
pcoa_ord(pnts, data$factor_6lvl, leg_pos = 'topleft',
         leg_cont = c('1', '2', 'Group 3', '4', '5', '6'))
dev.off()



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
