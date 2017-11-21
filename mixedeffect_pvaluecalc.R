##  ----------------------------------------------------------------------------------------------------------  ##
                      # Mixed-Effect Model P Value Determination
##  ----------------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon
  ## Updated Nov. 21, 2017

# Clear environment and set working directory
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
group2 <- as.vector( rnorm(20, mean = 3, sd = 1) )
group3 <- as.vector( rnorm(20, mean = 5, sd = 1) )
group4 <- as.vector( rnorm(20, mean = 10, sd = 1) )

# Get all that into a single column
response <- as.vector(c(group1, group2, group3, group4))

# Now you want a grouping variable
factor <- as.vector(c(rep.int("a", (length(response)/4)), 
                      rep.int("b", (length(response)/4)), 
                      rep.int("c", (length(response)/4)), 
                      rep.int("d", (length(response)/4))))

# And it might be valuable to have another factor variable to use as a random effect
ran <- c(rep.int("X", 10), rep.int("Y", 10))
random <- as.vector(rep(ran, (length(response)/4) ))

# Get all that into a dataframe
working.df <- as.data.frame(cbind(factor, random, response))
working.df$response <- as.numeric(as.character(working.df$response))

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
# Need this library to fit a mixed-effect model
library(lme4)

# Fit a mixed-effect model
mxef <- lmer(response ~ factor +(1|random), data = working.df)
summary(mxef)

# PURPOSE:
  ## A critique I have heard of mixed-effect models is the lack of an easy 'here is the p value' output
  ## While thinking is encouraged, it is helpful to have a p value to aid in interpreting results
  ## Especially for those among us who are less statistically-inclined
  ## This function reports mixed-effect model significance via t statistic and p value
    ### Hence the name "memsig" (mixed-effect model = MEM + significance)

# Load the function
memsig <- function(model, man.dig){
  ## model = object of mixed-effect model fitted by lme4::lmer
  ## man.dig = manual setting of the number of digits for p value reporting
  
  model <- mxef
  man.dig <- 6
  # Load in the table of summary results that comes with the model
  summary <- data.frame(coef(summary(model)))
  
  # For Kenward-Roger approximation of degrees of freedom
  require(pbkrtest)
  summary$df <- as.numeric(get_ddf_Lb(model, fixef(model)))
  
  # Calculate p value from degrees of freedom and t statistic
  pvalues <- 2 * (1 - stats::pt(abs(summary$t.value), summary$df))
  
  # Turn the p value into an interpretable four-digit code (for easy reporting without huge negative exponents)
  summary$pval <- round(pvalues, digits = man.dig)
  
  # Return the table of summary information
  return(summary)
}

memsig(model = mxef, man.dig = 6)
