install.packages("permute")
library(permute)
install.packages("tidyverse")
library(tidyverse)


## Numbering the positions
#           1
#         2  3
#       4  5  6
#     7  8  9  10
#    11 12 13 14 15


# Generate a list of all possible arrangements of 7 solids, 7 stripes and the 8-ball
# within the template above.
# First place the 7 solids in any 7 of the 15 positions--C(15,7)=6435 possibilities.

arrangements <- t(combn(1:15,7)) %>% as.data.frame()
names(arrangements) <- c("solid1","solid2","solid3","solid4","solid5","solid6","solid7")
arrangements$ID <- seq.int(nrow(arrangements))

# now that the solids are in place, 
# the 8-ball has 8 remaining positions it can take in each arrangement
# but I'm lazy so I'm going to put it in all 15 positions and then delete the impossible
# arrangements (ie the ones where the 8 ball is in an alread-occupied position)
# this will give us C(15,7)*8=51480 combinations

arrangements$eight <- rep(1:15)
extra_eights <- arrangements %>% expand(ID,eight)
extra_eights <- left_join(extra_eights,arrangements,by="ID")
extra_eights$ID2 <- seq.int(nrow(extra_eights))
extra_eights <- extra_eights %>% dplyr::select(-ID,-`eight.y`)

# this for loop deletes the extraneous arrangements
for (i in 1:nrow(extra_eights)) {
  if (extra_eights$`eight.x`[which(extra_eights$ID2==i)] %in% c(extra_eights[which(extra_eights$ID2==i),2:8])) {
    extra_eights <- extra_eights %>%  filter(ID2!=i)
  }
}

# the remaining 7 positions must be occupied by stripes,
# and there are no additional arrangements to be made by 
# shuffling around the identical stripes, so we have our 51480 total arrangements

#save the results as a csv
write.csv(extra_eights,"~/538_riddler_141/arrangements.csv")
