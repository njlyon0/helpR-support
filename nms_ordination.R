##  --------------------------------------------------------------------------------  ##
                            # NMS Function
##  --------------------------------------------------------------------------------  ##
# Code written by Nick Lyon
    ## refer questions to njlyon@iastate.edu if insoluble problems arise


# This function does an non-metric multidimensional scaling (NMS) ordination for you

# Press the arrow to the left of the function to get it to expand.
    ## Each bit has a comment explaining it and there is example syntax and descriptions of what the function needs within the function code

# Colors are colorblind safe and found here:
  # http://colorbrewer2.org/#type=sequential&scheme=GnBu&n=4

# Functions that plot NMS points with different colors for groups (only works for number of groups corresponding to name of fxn)
nms4plot <- function(mod, groupcol, g1, g2, g3, g4, legpos, legcont) {
  ## mod = object returned by metaMDS function 
  ## groupcol = group column FROM THE DF THAT CONTAINS THAT COLUMN
  ## g1 - 4 = grouping variables as written in dataframe (this MUST be written as it is in the dataframe)
  ## legpos = legend position, either numeric vector of x/y coords or shorthand accepted by "legend" function
  ## legcont = legend content, vector of labels for legend (this can be anything you want)
  
  ###############
  # EXAMPLE SYNTAX
  ###############
  #  nec1.mds <- metaMDS(dataframe, autotransform = F, expand = F, k = 2, try = 100)
      ## NOTE: This argument requires the dataframe that has no non-numeric columns but is otherwise identical to your 'full' df
  #  nms4plot(nec1.mds, as.factor(nec_r1$Fescue.Treatment), "Ref", "Con", "Spr", "SnS", "bottomleft", c("Ref", "Con", "Spr", "SnS"))
      ## In this case, "nec1.mds" is what was returned by the metaMDS
      ## But "nec_r1" is the dataframe that includes by grouping variables
  
  
  # Create plot
  plot(mod, display = 'sites', choice = c(1, 2), type = 'none', xlab = "", ylab = "")
  
  # Add points for each group with a different color per group
  points(mod$points[groupcol == g1, 1], mod$points[groupcol == g1, 2], pch = 21, bg = "#0868ac")
  points(mod$points[groupcol == g2, 1], mod$points[groupcol == g2, 2], pch = 21, bg = "#43a2ca")
  points(mod$points[groupcol == g3, 1], mod$points[groupcol == g3, 2], pch = 21, bg = "#7bccc4")
  points(mod$points[groupcol == g4, 1], mod$points[groupcol == g4, 2], pch = 21, bg = "#bae4bc")
  
  # Ordinate SD ellipses around the centroid
  ordiellipse(mod, groupcol, col = c("#0868ac", "#43a2ca", "#7bccc4", "#bae4bc"), display = "sites", kind = "sd", label = F)
  
  # Add legend
  legend(legpos, legend = legcont, bty = "n", fill = c("#0868ac", "#43a2ca", "#7bccc4", "#bae4bc"))
  
}


