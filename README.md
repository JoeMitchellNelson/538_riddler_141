# 538_riddler_141
Solution to 538's robo-racker riddler

See https://fivethirtyeight.com/features/the-robot-invasion-has-come-for-our-pool-halls/
for the puzzle.

# Set up the problem
rack_generator.R creates a data frame describing the 51480 possible starting positions
for the balls.

# Solve the arrangements
rack_solver.R parses each row of the data frame created in rack_generator into
an ordered list of balls (solid, eight, stripe, stripe,...) and then determines the
minimum number of allowable moves required to match the starting arrangement to 
a target arrangement (see the blog for a picture).

Three functions are defined to rotate the arrangement left and right, and to make pairwise
switches. The loop that solves each arrangement doesn't try to guess whether a rack should
be rotated left, right or not at all. Rather, it solves each rack 3 times, once for each 
rotation possibility. Then it takes the most efficient of the 3 at the end.

# Results
Most starting arrangements require 4 moves. The most moves any arrangement requires
is 6, and there are 140 such starting arrangements. These arrangements are listed in the
data frame "worst," generated at the end of rack_solver.R. The average number of moves
required is 4.017.
