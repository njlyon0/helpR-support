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

##  -------------------------------------  ##
   # Mixed-Effect Model Fitting
##  -------------------------------------  ##
# Fit a mixed-effect model
mxef <- lme4::lmer(response ~ factor +(1|random), data = working.df)
summary(mxef)
  ## You get some summary statistics but no p values!

# PURPOSE:
  ## A critique I have heard of mixed-effect models is the lack of an easy 'here is the p value'-style output
  ## While thinking is encouraged, it is helpful to have a p value to aid in interpreting results
  ## Especially for those among us who are less statistically-inclined
  ## This function reports mixed-effect model significance via t statistic and p value
    ### Hence the name "memsig" (mixed-effect model = MEM + significance)

# Load the function
memsig <- function(model, df.dig = 4, p.dig = 4){
  ## model = object of mixed-effect model fitted by lme4::lmer
  ## df.dig = manual setting of the number of digits for degrees of freedom
  ## p.dig = manual setting of the number of digits for p value reporting
  
  # Load in the table of summary results that comes with the model
  summary <- data.frame(coef(summary(model)))
  
  # For Kenward-Roger approximation of degrees of freedom from each factor level
  require(pbkrtest)
  df1 <- get_ddf_Lb(model, c(1,0,0,0))  # to test 1 * intercept = 0
  df2 <- get_ddf_Lb(model, c(0,1,0,0))  # to test 1 * factorb = 0
  df3 <- get_ddf_Lb(model, c(0,0,1,0))  # to test 1 * factorc = 0
  df4 <- get_ddf_Lb(model, c(0,0,0,1))  # to test 1 * factord = 0
  
  # Get a single vector and round them to be more intelligible
  dfs <- as.vector(c(df1, df2, df3, df4))
  
  # Get them into a column as part of your summary table
  summary$df <- round(dfs, digits = df.dig)
  
  # Calculate p value from degrees of freedom and t statistic
  pvalues <- 2 * (1 - stats::pt(abs(summary$t.value), summary$df))
  
  # Turn the p value into an interpretable four-digit code (for easy reporting without huge negative exponents)
  summary$pval <- round(pvalues, digits = p.dig)
  
  # Return the table of summary information
  return(summary)
}

memsig(model = mxef, p.dig = 6)

# Alternately, there is an upgrade to the lmer function in the "lmerTest" package that does this by default
mxef2 <- lmerTest::lmer(response ~ factor +(1|random), data = working.df)
coef(summary(mxef2))

# So no real need for my custom function, though it does (hopefully) aid in understanding how p values
  ## are calculated for mixed effects models.
  ## Cheers!


