##  --------------------------------------------------------------------  ##
      # Non-metric Multidimensional Scaling (NMS) Ordination Function
##  --------------------------------------------------------------------  ##
# Code written by Nick J Lyon
  ## Updated 2022, 8 March

# Purpose:
## Create publication-quality non-metric multidimensional scaling ordinations

# Required libraries
library(vegan); library(RRPP); library(tidyverse)

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
  ## Data on lichen "pastures", can check it out with: 
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
       # Null Hypothesis Testing ####
## --------------------------------------  ##
# ANALYSIS NOTE:
  ## While NMS can be useful in visualizing multivariate differences, it is NOT a hypothesis test!
  ## Use of an actual multivariate analytical test is necessary if differences among groups are of interest
  ## Feel free to skip through this section if you don't need/want help with multivariate analysis

# Initial perMANOVA
anova(RRPP::lm.rrpp(resp ~ factor, data = ref.rdf), effect.type = "F")
  ## Interpretation: at least one group is significantly different from the others
  ## Yes these groups don't mean anything, but it's still illustrative.

# In reality would need to track down which groups are different from which (pairwise comparisons)
  ## but we'll leave that alone here

## --------------------------------------  ##
                  # NMS ####
## --------------------------------------  ##
# Actually do the non-metric multi-dimensional scaling
mds <- metaMDS(data[-c(1:3)], autotransform = F, expand = F, k = 2, try = 100)
## Where "resp" is the matrix of your community data (without grouping variables)

mds$stress
##  "Stress" is typically reported parenthetically for NMS ordinations,
## Similar to F statistics or p values
## Clarke et al. 1993 suggests stress ≤ 0.15 to be a good rule of thumb
### http://dx.doi.org/10.1111/j.1442-9993.1993.tb00438.x

## --------------------------------------  ##
         # Ordination Function ####
## --------------------------------------  ##
# Get our NMS function (works up to 10 groups)
nms_ord <- function(mod, groupcol, title = NA,
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
    plot(mod, display = 'sites', choice = c(1, 2), type = 'none',
         xlab = "NMS Axis 1", ylab = "NMS Axis 2", main = title)
    
    # Create a counter set to 1 (we'll need it in a moment)
    k <- 1
    
    # For each group, add points of a unique color and (up to 5 groups) unique shape (only 5 hollow shapes are available so they're recycled 2x each)
    for(level in unique(groupcol)){
      points(mod$points[groupcol == level, 1], mod$points[groupcol == level, 2],
             pch = shapes_actual[k], bg = colors_actual[k])
      
      # After each group's points are created, advance the counter by 1 to move the earlier part of the loop to a new color/shape
      k <- k + 1 }
    
    # With all of the points plotted, add ellipses of matched colors
    # This also allows for variation in line type if desired
    vegan::ordiellipse(mod, groupcol, col = colors_actual,
                       display = 'sites', kind = 'sd', lwd = 2,
                       lty = lines_actual, label = F)
    
    # Finally, add a legend
    legend(leg_pos, legend = leg_cont, bty = "n", 
           # The "title" of the legend will now be the stress of the NMS
           title = paste0("Stress = ", round(mod$stress, digits = 3)),
           pch = shapes_actual, cex = 1.15, pt.bg = colors_actual)
  }
}

# Use it for 2 levels
nms_ord(mod = mds, groupcol = data$factor_2lvl, title = '2-Level NMS',
        leg_pos = 'topright')

# Use it for 4 levels (but specify legend content)
nms_ord(mod = mds, groupcol = data$factor_4lvl, title = '4-Level NMS',
        leg_pos = 'topright', leg_cont = c('1', '2', '3', '4'))

# And use it for 6 (dropping title and some argument names)
nms_ord(mds, data$factor_6lvl, leg_pos = 'topright')

# Save two of them
## 4-level
jpeg(file = "./Test Plots/NMS_DummyOrd.jpg")
nms_ord(mod = mds, groupcol = data$factor_4lvl, title = '4-Level NMS',
        leg_pos = 'topright', leg_cont = c('1', '2', '3', '4'))
dev.off()

## 2-level
jpeg(file = "./Test Plots/NMS_DummyOrd2.jpg")
nms_ord(mod = mds, groupcol = data$factor_2lvl, title = '2-Level NMS', leg_pos = 'topright')
dev.off()

# END ####
