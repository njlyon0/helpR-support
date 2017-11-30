##  ----------------------------------------------------------------------------------------------------------  ##
                  # Pairwise Comparisons with Critical Point Adjustment
##  ----------------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon
  ## Updated Nov. 21, 2017

# clear environment and set working directory
rm(list = ls())
setwd("~/Documents/School/Misc R/Custom Functions")
  ## "Session" menu at top of screen -> "Set Working Directory" -> "To Project Directory"

##  -------------------------------------  ##
      # Dummy Data Creation
##  -------------------------------------  ##
# Want to simulate data so that you have sufficient replicates
  ## While it doesn't really matter if the pairwise comparisons are significant,
  ## you don't want the function's efficacy to be confounded by low statistical power

# Simulate data that are normally distributed and have different means/variances
  ## Increase the odds of at least one compairson being signficant
group1 <- as.vector( rnorm(20, mean = 8, sd = 1) )
group2 <- as.vector( rnorm(20, mean = 7.75, sd = 1) )
group3 <- as.vector( rnorm(20, mean = 7, sd = 1) )
group4 <- as.vector( rnorm(20, mean = 6, sd = 1) )

# Get all that into a single column
response <- as.vector(c(group1, group2, group3, group4))

# Now you want a grouping variable
factor <- as.vector(c(rep.int("Gp1", (length(response)/4)), 
                      rep.int("Gp2", (length(response)/4)), 
                      rep.int("Gp3", (length(response)/4)), 
                      rep.int("Gp4", (length(response)/4))))

# Get all that into a dataframe
working.df <- as.data.frame(cbind(factor, response))
working.df$response <- as.numeric(as.character(working.df$response))

# To summarize:
  # You have 80 observations of some response
  # These are grouped into 20 observations from four levels of a treatment "factor"
    ## either 'a', 'b', 'c', or 'd'

# Check it out!
str(working.df)

##  -------------------------------------  ##
      # Pairwise Comparisons
##  -------------------------------------  ##
# First you analyze your data to see if your grouping variable is significant
aov.fit <- aov(response ~ factor, data = working.df)
summary(aov.fit)
  ## Quelle surprise: at least one factor is significantly different from the others!

# Now you want to know **which** levels are different from the others!

# PURPOSE:
  ## Conduct multiple pairwise comparisons and adjust for this by modifying the critical point
  ## This differs importantly from the "pairwise.t.test" function in the "stats" package
    ### As that function modifies the *p value* rather than the *critical point*,
    ### such that all interpretations can be made at alpha = 0.05
  ## While convenient, this would lead you to report both an incorrect critical point and p value

# Multiple comparison adjustment methods currently supported
    ## Sequential Bonferroni
    ## more to come! (hopefully)

# Load the function
pairstest <- function(dependent, indep, p.dig, crit.dig){
  ## dependent = response (or "dependent") variable
  ## indep = explanatory (or "independent") variable
  ## crit.dig = digits for critical point reporting
  ## p.dig = digits for p value reporting
  
  # Get the unadjusted p values for multiple comparisons
  pairs.unadj <- pairwise.t.test(dependent, indep, p.adj = "none")
  
  # Want a list of the pairwise comparisons from the matrix in "pairs.unadj"
  combinations <- expand.grid(rownames(pairs.unadj$p.value), colnames(pairs.unadj$p.value))
  
  # Now you want to make a results dataframe to hold adjusted  p values
  results <- NULL
  
  # List the actual pairwise combinations
  results$pairs <- paste0(combinations$Var1, "-", combinations$Var2) 
  
  # List unadjusted p values
  results$pvals <- as.vector( round(pairs.unadj$p.value, digits = p.dig) )
  
  # Get the set of pairwise comparisons and their associated p values into dataframe format
  results <- as.data.frame(results)
  
  # Ditch pairwise combinations for which there isn't a p value
  results <- results[complete.cases(results),]
      ## Comparisons of a group to itself or redundant comparisons (E.g. a to b = b to a, etc.)
  
  # START SEQUENTIAL BONFERRONI-SPECIFIC STUFF
  
  # For sequential Bonferroni you need to rank the pairs based on ascending p value
  results <- results[order(results$pvals),] # order the comparisons
  results$rank <- c(1:length(results$pairs)) # assign them a rank based on this order
  
  # Modify the critical point based on the rank of each sequential p value
  results$alpha <- round( with(results, ( (0.05 / (length(results$pairs) + 1 - rank)) ) ), digits = crit.dig)
    ## Sequential bonferroni is calculated as show above, but in plain English it is like this:
    ## Each comparison gets it's own, sequential, critical point
    ## This is determined by dividing the standard critical point (0.05) by
    ## the total number of comparisons plus 1, minus the "rank" of the p value
    ## where lower p values have a lower rank
    ## The final pairwise comparison will always have a critical point of 0.05 in this method
      ### E.g. 6 pairwise comparisons + 1 - 6 (for the sixth one) = 1
      ### And 0.05 / 1 = 0.05 (duh)
  
  # END SEQUENTIAL BONFERRONI-SPECIFIC STUFF
  
  # Also, you might want a significance assessment
  # First, creates a logical vector of T/F on whether the p value is less than the critical point
  results$sig <- with(results, (pvals < alpha))
  
  # Second, modifies that answer into "yes" or "no" for significance
  results$sig <- gsub("TRUE", "Yes!", results$sig); results$sig <- gsub("FALSE", "No", results$sig)
    ## Buyer beware: this does treat the critical point as a 'cliff' which some people disagree with
  
  # This is cosmetic; the re-ordering step makes the default row numbers non-sequential at this stage
  row.names(results) <- NULL
  
  return(results)
}

# Run the pairwise comprison test!
pairstest(dependent = working.df$response, indep = working.df$factor, p.dig = 5, crit.dig = 4)

# Plotting can help you better visualize these pairwise differences
plot(response ~ factor, xaxt = 'n', data = working.df)
axis(1, at = unique(working.df$factor), labels = parse(text = c("Gp1[A]", "Gp2[AB]", "Gp3[B]", "Gp4[C]")) )

# NOTE ON FXN MODIFICATION:
# As a reminder if you modify the function to do some different purpose:
  ## You chose the criteria for simulating the data!
  ## Therefor, if this function tells you comparisons are different when you made them not different
  ## something has gone horribly wrong

# ALTERNATE METHODS:
# If you would rather do Bonferroni (no sequential adjustment of critical point, so is more conservative)
  ## just compare all p values to the lowest critical point reported by this function.
  ## That is the traditional Bonferroni correction

# PROOF (of above statement):
  ## Bonferroni correction = 0.05 / number comparisons
  ## Sequential Bon = 0.05 / (number comparisons + 1 - rank of comparison i)

# So the first comparison (i.e. of the comparison with rank 1) will yield 0.05 / (X + 1 - 1)
# Identical to standard Bonferroni (0.05 / X)
    ## Where "X" is the number of comparisons
