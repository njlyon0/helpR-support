##  --------------------------------------------------------------------  ##
                      # Date Check Function
##  --------------------------------------------------------------------  ##
# Written by Nick J Lyon
  ## Updated: March 9, 2022

# Clear environment
rm(list = ls())

## --------------------------------------  ##
              # Data Prep ####
## --------------------------------------  ##
# Let's create some data that will be useful in demoing this function
sites <- data.frame('site' = c("LTR", "GIL", "PYN", "RIN"),
                    'visit' = c('2021-01-01', '2021-01-0w', '1990', '2020-10-xx'))
## Normally (of course) you would not purposefully introduce errors
## But we need non-numbers to show off this function's utility

# Look at data
sites
## With this small of a df we can see the errors but imagine there was a larger dataset we couldn't visually scan

# Check structure
str(sites)

# If you coerce that column to numeric it silently changes the typo ones to NA
sites$visit_coerce <- as.Date(sites$visit)

# Check it out
sites

# You could look through all of the entries in that column...
unique(sites$visit)
# ...but you would have to sort through all of the non-issue entries too

# Here's where my function comes in

## --------------------------------------  ##
          # Numeric Check Function
## --------------------------------------  ##
# Load the function
date_chk <- function(dat, col) {
  ## dat = a dataframe
  ## col = the name of the column you want to check for issues (in quotes!)
  
  # Reduce the supplied dataframe to only those values that are nonNA
  notNA <- subset(dat, !is.na(dat[, col]))
  
  # Subset that dataframe to only those values that would be unhappy to be made into true dates
  bad <- subset(notNA, is.na(as.Date(notNA[, col])))

  # And return the values of our initial column that will be problematic
  unique(bad[, col])
}

# Now we can use it
date_chk(dat = sites, col = 'visit')
# Only entries that would be coerced to NA are returned

# You can then use whichever flavor of typo fix you want to resolve this
sites2 <- sites
sites2$visit_fix <- gsub("2021-01-0w", "2021-01-02", sites2$visit)
sites2$visit_fix <- gsub("1990", "1990-01-01", sites2$visit_fix)
sites2$visit_fix <- gsub("2020-10-xx", "2020-10-31", sites2$visit_fix)
## Note you'd have to find the "right" dates from some external source or referring to your own records/raw data

# Then re-run the custom function to ensure the errors are fixed
date_chk(dat = sites2, col = 'visit_fix')
# "character(0)" means that there are no values that would be coerced to NA

# Note that the function won't identify non-dates that R can guess
unique(sites2$visit_fix)

# But this is a non-issue because when you coerce the column to numeric
sites2$visit_date <- as.Date(sites2$visit_fix)

# R correctly identifies the 'actual' dates
unique(sites2$visit_date)

# See the evolution of the 'visit' column here
str(sites2)
sites2

# This is a shorter one than the others but my need for this function is simple
# Hope it helps!

# End ---------------------------------------------------------------

