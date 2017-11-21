##  ----------------------------------------------------------------------------------------------------------  ##
                  # Pairwise Comparisons with Critical Point Adjustment
##  ----------------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon
  ## Updated Nov. 21, 2017

# PURPOSE:
  ## Conduct multiple pairwise comparisons and adjust for this by modifying the critical point
      ### So far this does sequential Bonferroni without other options, but more will be coming
  ## This differs importantly from the "pairwise.t.test" function in the "stats" package
  ## As that function modifies the *p value* rather than the *critical point*,
  ## such that all interpretations can be made at alpha = 0.05
  ## While convenient, this would lead you to report an incorrect critical point in the body of your results

# clear environment and set working directory
rm(list = ls())
setwd("~/Documents/School/Misc R/Custom Functions")
  ## "Session" menu at top of screen -> "Set Working Directory" -> "To Project Directory"

##  -------------------------------------  ##
      # Dummy Data Creation
##  -------------------------------------  ##
# Let's get some dummy data from the vegan package to test this
data("varespec")

# Only need one response column for pairwise comparisons so let's ditch the others
response <- varespec$Cladrang

# Need a faux-grouping variable too for pairwise comparisons among
factor <- as.vector(c(rep.int("a", (nrow(varespec)/4)), 
                      rep.int("b", (nrow(varespec)/4)), 
                      rep.int("c", (nrow(varespec)/4)), 
                      rep.int("d", (nrow(varespec)/4))))
  ## It doesn't matter if this is significant or not as we just want to test my function

# And just to make it a little more relevant to many people, let's give it a random effect for mixed-effect models
ran <- c("X", "X", "Y", "Y")
random <- as.vector(rep(ran, (nrow(varespec)/4) ))

# Get all that into a dataframe
working.df <- as.data.frame(cbind(factor, random, response))
working.df$response <- as.numeric(as.character(working.df$response))

##  -------------------------------------  ##
    # Mixed-Effect Model Fitting
##  -------------------------------------  ##


# Need this library to fit a mixed-effect model
library(lme4)

# Fit a mixed-effect model
mxef <- lmer(response ~ factor +(1|random), data = working.df)
summary(mxef)









#PAIRWISE TESTS OF LMER
library(pbkrtest)

#CREATE MEMSIG FUNCTION
memsig <- function(model){
  # For Kenward-Roger approximation of df, t-dist, and p-values
  require(pbkrtest)
  output <- NULL
  output <- data.frame(coef(summary(model)))
  df <- as.numeric(get_ddf_Lb(model, fixef(model)))
  output$pval <- 2 * (1 - pt(abs(output$t.value), df))
  return(output)
}


##  -------------------------------------  ##
      # Pairwise Comparisons
##  -------------------------------------  ##



#CREATE PAIRSTEST FUNCTION
pairstest <- function(response, explanatory){
  pairsfxn <- pairwise.t.test(response, explanatory, p.adj = "none")
  factors <- expand.grid(rownames(pairsfxn$p.value), colnames(pairsfxn$p.value))
  output <- NULL
  output$pairs <- paste0(factors$Var1, "-", factors$Var2) 
  output$pvals <- as.vector(pairsfxn$p.value)
  output <- as.data.frame(output)
  output <- output[complete.cases(output),]
  output <- output[order(output$pvals),]
  output$rank <- c(1:length(output$pairs))
  output$alpha <- with(output, ( (0.05 / (length(output$pairs) + 1 - rank)) ) )
  output$sig <- with(output, (pvals < alpha))
  
  return(output)
}

# ANALYSIS
memsig()
pairstest(dat$x, dat$x)



