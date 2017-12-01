##  ----------------------------------------------------------------------------------------------------------  ##
                                  # How to Function
##  ----------------------------------------------------------------------------------------------------------  ##
# Code written by Nick J Lyon
  ## Updated Nov. 30, 2017

# SO:
# You've moved enough of your data management process into R that efficiency is now a consideration
  ## First off: congratulations!
  ## Secondly: It is time to automate some of your common  procedures and tools into functions!

# You've probably encountered some of these that are conveniently posted on Stack Overflow or Git (etc.)
  ## but it's extremely worthwhile to figure out how to write your own.
  ## You'll be able to become much speedier, and won't have to re-type cumbersome plotting/cleaning codes

# Fortunately, R provides a mechanism for writing your own functions
  ## While my purpose today will be to share some of the functions I've written
  ## it is important to understand the basics outside of that context


# Without further ado, let's examine some of the basics
rm(list = ls())
  # Clear your environment always before starting this sort of thing (eliminate many foolish mistakes)

# The way that functions...well...function is fairly straightforward
  ## Assign the operation(s) you want to conduct to an object using the argument "function"

fxn.syntax <- function(x, y, z){ }
  # The stuff within the parentheses ("x, y, z") are the things that you (or any user) will feed into the function
  # The content within the brackets (nothing at this point) are what you want the function to actually *do*

# Let's give it some actual task to perform yes?

fxn1 <- function(x, y, z){
  
  x.response <- sqrt(x) # take the square root of "x"
  y.response <- (y + 2) # add 2 to "y"
  z.response <- (z * 3) # multiply "z" by 3
  
  # Get one vector to return
  full.response <- c(x.response, y.response, z.response)
  
  return(full.response)
  
}

# Then you apply it in the same way you would with a pre-created function
fxn1(x = 4, y = 8, z = 3)

# Functions also work when you only provide the inputs
fxn1(4, 8, 3)

# but ORDER MATTERS
fxn1(8, 3, 4)

# Not extremely useful to perform unconnected operations, but you can also do sequential stuff
fxn2 <- function(x){
  
 x.resp1 <- sqrt(x) # perform first operation
  
 x.resp2 <- (x.resp1 + 4) # then do something else to the product of that first operation
 
 x.resp3 <- (x.resp2 * 2) # do a third (final) thing
 
 x.resp3 # and return the outcome
 
}

# Run your new function
fxn2(4)
fxn2(x = 4)
( sqrt(4) + 4 ) * 2 # same sequence of operations

# These functions are purposefully simple, but you can construct extremely complex functions if need be

# Some good resources for more tips on how to get started writing functions
# https://swcarpentry.github.io/r-novice-inflammation/02-func-R/

# Or the super basic explanation from R on the bare bones of writing functions
# https://www.statmethods.net/management/userfunctions.html

# Let's switch gears and look at some functions I've written to perform different tasks
# Learning by doing (or examining in an applied context) is likely to prove helpful
