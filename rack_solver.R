install.packages("permute")
library(permute)
install.packages("tidyverse")
library(tidyverse)

## now we want to solve all the arrangements we made
## where "solve" means use a combination of pairwise switches
## and 120-degree triangle rotations to match this template

target <- c(               "solid",
                       "stripe","solid",
                     "solid","eight","stripe",
                 "stripe","solid","stripe","solid",
               "solid","stripe","stripe","solid","stripe")

## first we read in the arrangements we made in the rack_generator file
arrangements <- read.csv("~/538_riddler_141/arrangements.csv") %>% 
  dplyr::select(-ID2,-X) %>% rename("eight"=`eight.x`)

## then turn each arrangement into an ordered list
## (solid, solid, eight, stripe, solid, etc)
## so that we have objects we can easily manipulate

# initialize a new column in the data frame
arrangements$placement <- "AAA"

# generate string lists describing each arrangement
# takes about 13 minutes on my potato laptop
for (i in 1:51480) {
  starting_rack <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  starting_rack[arrangements$eight[i]] <- "eight"
  starting_rack[as.integer(arrangements[i,2:8])] <- "solid"
  starting_rack[which(starting_rack==0)] <- "stripe"
  arrangements$placement[i] <- list(starting_rack)
}

# We want to create three functions that we can use to manipulate
# the arrangements: two rotation functions (left and right), and
# a pairwise switching function

rotate_left <- function (rack) {
  if (length(rack)!=15) {
    return(NA)
  } else {
    new_rack <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    new_rack[1] <- rack[15]
    new_rack[2] <- rack[10]
    new_rack[3] <- rack[14]
    new_rack[4] <- rack[6]
    new_rack[5] <- rack[9]
    new_rack[6] <- rack[13]
    new_rack[7] <- rack[3]
    new_rack[8] <- rack[5]
    new_rack[9] <- rack[8]
    new_rack[10] <- rack[12]
    new_rack[11] <- rack[1]
    new_rack[12] <- rack[2]
    new_rack[13] <- rack[4]
    new_rack[14] <- rack[7]
    new_rack[15] <- rack[11]
    return(new_rack)
  }
}

# rotating right is equivalent to rotating left twice
# so we can save some space here by defining rotate_right
# in terms of rotate_left
rotate_right <- function (rack) {
  new_rack <- rotate_left(rotate_left(rack))
  return(new_rack)
}

# this function will switch two balls in the specified positions, i and j
swtch <- function(rack,i,j) {
  rack[c(i,j)] <- rack[c(j,i)] 
  return(rack)
} 

# Now that we have 51480 arrangements and the functions for solving them,
# we need to find solutions using the fewest moves. So let's try to characterize
# the most efficient solutions. First, we should never rotate more than once.
# Two rotations left is equivalent to one rotation right. Three rotations left is equivalent
# to no rotations. So multiple rotations means wasted moves. Second, we have to pay special
# attention to the 8-ball. If it doesn't start in the correct place (position 5), we need to
# rotate or switch it into place. Once the 8-ball is in place, there must be an even number (2n)
# of out-of-place balls--for every stripe out of place, there must be a solid out of place.
# So after the 8-ball is positioned, we'll have have n pairwise switches to make.

# Rather than trying to figure out which rotation (or none) is most efficient, this loop
# solves each arrangement three ways:
# 1) don't rotate, position the 8-ball (if necessary), then make pairwise switches
# 2) rotate left, position the 8-ball (if necessary), pairwise switches
# 3) rotate right, position the 8-ball (if necessary), pairwise switches

# One of these three must be the most efficient solution, so the for loop below calculates and stores
# results for all of them. Then, at the end, we take the fastest solution of the 3 and store that number of
# moves as the minimum for that arrangement.

#initialize some new columns
arrangements$moves_a <- 1000
arrangements$moves_b <- 1000
arrangements$moves_c <- 1000

#then this for loop does all the heavy lifting
#takes about 3 minutes on my potato
for (i in 1:51480) {
  ############# NO ROTATE ############
  r <- arrangements$placement[[i]]
  moves_a <- 0
  if (which(r=="eight")!=5) {
    r <- swtch(r,5,which(r=="eight"))
    moves_a <- 1}
  moves_a <- moves_a + sum(r!=target)/2
  ######## LEFT ROTATE ##############
  r <- rotate_left(arrangements$placement[[i]])
  moves_b <- 1
  if (which(r=="eight")!=5) {
    r <- swtch(r,5,which(r=="eight"))
    moves_b <- 2}
  moves_b <- moves_b + sum(r!=target)/2
  ######## RIGHT ROTATE ##############
  r <- rotate_right(arrangements$placement[[i]])
  moves_c <- 1
  if (which(r=="eight")!=5) {
    r <- swtch(r,5,which(r=="eight"))
    moves_c <- 2}
  moves_c <- moves_c + sum(r!=target)/2
  
  ###### store the results
  arrangements$moves_a[i] <- moves_a
  arrangements$moves_b[i] <- moves_b
  arrangements$moves_c[i] <- moves_c
  
}

# store the most efficient number of moves as "min_moves" in the data frame
arrangements <- arrangements %>% mutate(min_moves = 
                                          ifelse(moves_a<=moves_b&moves_a<=moves_c,moves_a,
                                                 ifelse(moves_b<=moves_c,moves_b,moves_c)))

# Now the results:

#### create a data table of the arrangements with the highest
#### minimum moves to solve
worst <- arrangements %>% filter(min_moves==max(min_moves))
# there are 140 starting racks that require 6 moves

#### plot a histogram of min_move frequency
hist(arrangements$min_moves)

# and the average number of moves is 4.017
summary(arrangements$min_moves)


