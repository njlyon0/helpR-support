##  ----------------------------------------------------------------------------------------------------------  ##
                      # Mixed-Effect Model P Value Determination
##  ----------------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon
  ## Updated Nov. 30, 2017

# Be sure that you have the two necessary libraries installed
#install.packages(c("lme4", "pbkrtest"))
# and load them
library(lme4); library(pbkrtest)

# Clear environment and set working directory
rm(list = ls())
setwd("~/Documents/School/Misc R/Custom Functions")
## "Session" menu at top of screen -> "Set Working Directory" -> "To Project Directory"

##  -------------------------------------  ##
      # Dummy Data Creation
##  -------------------------------------  ##
# Want to simulate data so that you have sufficient replicates
  ## While it doesn't really matter if anything is actually significant
  ## you don't want the function's efficacy to be confounded by low statistical power/no group differences

# Choose the mean values for 8 groups and their variances
means <- rep(c(10, 4, 5, 10, 10, 4, 5 ,10), rep(10, 8))
sds <- rep(c(1, 1, 1, 1, 3, 3, 3, 3), rep(10, 8))

# Once you have both you can easily generate a single object to hold all those different groups
response <- rnorm(80, means, sds)

# Now you want to add your grouping variable (group means vary by this value)
factor <- rep(c('a', 'b', 'c', 'd', 'a', 'b', 'c', 'd'),  rep(10, 8))

# And your random effect (wherer group 2 has much greater variance than group 1)
random <- rep(c('1', '1', '1', '1', '2', '2', '2', '2'),  rep(10, 8))

# Now make a dataframe to test the mixed effect function on
working.df <- data.frame(factor, random, response)
str(working.df)  # always good to check to see if you got what you thought you'd get


# Simulate data that are normally distributed and have different means/variances
  ## Increase the odds of at least one compairson being signficant
  ## Also, the two letters in the object name after "group" will make sense in a moment
groupax <- as.vector( rnorm(10, mean = 10, sd = 1) )
groupbx <- as.vector( rnorm(10, mean = 4, sd = 1) )
groupcx <- as.vector( rnorm(10, mean = 5, sd = 1) )
groupdx <- as.vector( rnorm(10, mean = 10, sd = 1) )
groupay <- as.vector( rnorm(10, mean = 10, sd = 3) )
groupby <- as.vector( rnorm(10, mean = 4, sd = 3) )
groupcy <- as.vector( rnorm(10, mean = 5, sd = 3) )
groupdy <- as.vector( rnorm(10, mean = 10, sd = 3) )

# Get all that into a single column
response <- as.vector(c(groupax, groupay, groupbx, groupby, groupcx, groupcy, groupdx, groupdy))

# Now you want a grouping variable
factor <- as.vector(c(rep.int("a", (length(response)/4)), 
                      rep.int("b", (length(response)/4)), 
                      rep.int("c", (length(response)/4)), 
                      rep.int("d", (length(response)/4))))

# And it might be valuable to have another factor variable to use as a random effect
ran <- c(rep.int("X", 10), rep.int("Y", 10))
random <- as.vector(rep(ran, (length(response)/4) ))

# Get all that into a dataframe
working.df <- data.frame(factor = factor, random = random, response = response)

# To summarize:
# You have 80 observations of some response
# These are grouped into four levels of a treatment "factor"
  ## either 'a', 'b', 'c', or 'd'
# And within each of these four levels you have one of two potential random effects "random"
  ## either 'X' or 'Y' where Y has a much greater variance (but the same mean as X for the same factor level)

# Check for yourself!
str(working.df)

##  -------------------------------------  ##
   # Mixed-Effect Model Fitting
##  -------------------------------------  ##
# Fit a mixed-effect model
mxef <- lmer(response ~ factor +(1|random), data = working.df)
summary(mxef)
  ## You get some summar statistics but no p values!

# PURPOSE:
  ## A critique I have heard of mixed-effect models is the lack of an easy 'here is the p value'-style output
  ## While thinking is encouraged, it is helpful to have a p value to aid in interpreting results
  ## Especially for those among us who are less statistically-inclined
  ## This function reports mixed-effect model significance via t statistic and p value
    ### Hence the name "memsig" (mixed-effect model = MEM + significance)

# Load the function
memsig <- function(model, p.dig = 4){
  ## model = object of mixed-effect model fitted by lme4::lmer
  ## p.dig = manual setting of the number of digits for p value reporting
  
  # Load in the table of summary results that comes with the model
  summary <- data.frame(coef(summary(model)))
  
  # For Kenward-Roger approximation of degrees of freedom
  require(pbkrtest)
  summary$df <- as.numeric(get_ddf_Lb(model, fixef(model)))
  
  # Calculate p value from degrees of freedom and t statistic
  pvalues <- 2 * (1 - stats::pt(abs(summary$t.value), summary$df))
  
  # Turn the p value into an interpretable four-digit code (for easy reporting without huge negative exponents)
  summary$pval <- round(pvalues, digits = p.dig)
  
  # Return the table of summary information
  return(summary)
}

memsig(model = mxef, p.dig = 6)







