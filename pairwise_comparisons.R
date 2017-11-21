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
group1 <- as.vector( rnorm(20, mean = 10, sd = 1) )
group2 <- as.vector( rnorm(20, mean = 5, sd = 1) )
group3 <- as.vector( rnorm(20, mean = 5, sd = 1) )
group4 <- as.vector( rnorm(20, mean = 10, sd = 1) )

# Get all that into a single column
response <- as.vector(c(group1, group2, group3, group4))

# Now you want a grouping variable
factor <- as.vector(c(rep.int("a", (length(response)/4)), 
                      rep.int("b", (length(response)/4)), 
                      rep.int("c", (length(response)/4)), 
                      rep.int("d", (length(response)/4))))

# Get all that into a dataframe
working.df <- as.data.frame(cbind(factor, response))
working.df$response <- as.numeric(as.character(working.df$response))

# To summarize:
  # You have 80 observations of some response
  # These are grouped into four levels of a treatment "factor"
    ## either 'a', 'b', 'c', or 'd'
  # And within each of these four levels you have one of two potential random effects "random"
    ## either 'X' or 'Y'
    ## Because this one was added entirely after the fact, it is unlikely to have any effect, but
      ## don't we frequently hope that our random effect isn't altering our treatment response?

# Check it out!
str(working.df)

##  -------------------------------------  ##
      # Pairwise Comparisons
##  -------------------------------------  ##
# First you analyze your data to see if your grouping variable is significant
aov.fit <- aov(response ~ factor, data = working.df)
summary(aov.fit)
  ## Quelle surprise, at least one factor is significantly different from the others!

# Now you want to know **which** levels are different from the others!

# PURPOSE:
  ## Conduct multiple pairwise comparisons and adjust for this by modifying the critical point
  ## This differs importantly from the "pairwise.t.test" function in the "stats" package
  ## As that function modifies the *p value* rather than the *critical point*,
  ## such that all interpretations can be made at alpha = 0.05
  ## While convenient, this would lead you to report an incorrect critical point in the body of your results

# Multiple comparison adjustment methods supported
    ## Sequential Bonferroni
    ## more to come!

# Load the function
pairstest <- function(dependent, indep, man.dig){
  ## dependent = response (or "dependent") variable
  ## indep = explanatory (or "independent") variable
  ## man.dig = manually set the digits you want the p value and critical point to be reported to
  
  # Get the unadjusted p values for multiple comparisons
  pairs.unadj <- pairwise.t.test(dependent, indep, p.adj = "none")
  
  # Want a list of the pairwise comparisons from the matrix in "pairs.unadj"
  combinations <- expand.grid(rownames(pairs.unadj$p.value), colnames(pairs.unadj$p.value))
  
  # Now you want to make a results dataframe to hold adjusted  p values
  results <- NULL
  
  # List the actual pairwise combinations
  results$pairs <- paste0(combinations$Var1, "-", combinations$Var2) 
  
  # List unadjusted p values
  results$pvals <- as.vector( round(pairs.unadj$p.value, digits = man.dig) )
  
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
  results$alpha <- round( with(results, ( (0.05 / (length(results$pairs) + 1 - rank)) ) ), digits = man.dig)
    ## The name of this method makes sense now right?
  
  # END SEQUENTIAL BONFERRONI-SPECIFIC STUFF
  
  # And just to make it painfully easy, this provides a logical where TRUE means p < alpha (i.e. significant)
  # and FALSE means p > alpha (i.e. non-significant)
  results$sig <- with(results, (pvals < alpha))
  
  return(results)
}

# Run the pairwise comprison test!
pairstest(dependent = working.df$response, indep = working.df$factor, man.dig = 4)

# As a reminder:
  ## When we simulated the data there was no difference between group A and group D
  ## And also no difference between groups B and C
  ## Therefor, if this function tells you those differences are significant, something has gone horribly wrong


