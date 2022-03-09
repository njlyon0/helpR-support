##  --------------------------------------------------------------------  ##
                      # Numeric Check Function
##  --------------------------------------------------------------------  ##
# Written by Nick J Lyon
  ## Updated: Feb. 3, 2022

# Clear environment
rm(list = ls())

## --------------------------------------  ##
              # Data Prep ####
## --------------------------------------  ##
# Let's create some data that will be useful in demoing this function
fish <- data.frame('species' = c('salmon', 'bass', 'halibut', 'eel'),
                   'count' = c(1, ' 14', '_23', 12))
  ## Normally (of course) you would not purposefully introduce errors
  ## But we need non-numbers to show off this function's utility

# Look at data
fish
  ## With this small of a df we can see the errors but imagine that we couldn't

# Check structure
str(fish)

# If we wanted to analyze whether 'count' differed among 'species'
# we would need 'count' to be numeric

# However, if we simply coerce that column to numeric...
as.numeric(fish$count)
# ...we get a warning that some entries were coerced to NAs

# You could look through all of the entries in that column...
unique(fish$count)
# ...but you would have to sort through all of the non-issue entries too

# Here's where my function comes in

## --------------------------------------  ##
          # Numeric Check Function
## --------------------------------------  ##
# Load the function
num_chk <- function(dat, col) {
  ## dat = a dataframe
  ## col = the name of the column you want to check for issues (in quotes!)
  
  # Reduce the supplied dataframe to only those values that are nonNA
  notNA <- subset(dat, !is.na(dat[, col]))
  
  # Subset that dataframe to only those values that would be unhappy to be made numeric
  bad <- subset(notNA, is.na(suppressWarnings(as.numeric(notNA[, col]))))
    ## the 'suppressWarnings' bit prevents the warning we know `as.numeric()` causes
  
  # And return the values of our initial column that will be problematic
  unique(bad[, col])
  
}

# Now we can use it
num_chk(dat = fish, col = 'count')
# Only entries that would be coerced to NA are returned

# You can then use whichever flavor of typo fix you want to resolve this
fish2 <- fish
fish2$count_fix <- gsub("\\_23", "23", fish2$count)

# Then re-run the custom function to ensure the error is fixed
num_chk(dat = fish2, col = 'count_fix')
# "character(0)" means that there are no values that would be coerced to NA

# Note that the function won't identify non-numbers that R can guess
unique(fish2$count_fix)
  ## see the " 14" with a space to the left of the number

# But this is a non-issue because when you coerce the column to numeric
fish2$count_num <- as.numeric(fish2$count_fix)

# R correctly identifies the 'actual' number
unique(fish2$count_num)

# See the evolution of the 'count' column here
str(fish2)
fish2
 
# This is a shorter one than the others but my need for this function is simple
# Hope it helps!

# End ---------------------------------------------------------------

