#day 3
#http://adventofcode.com/2017/day/3
# 
# You come across an experimental new kind of memory stored on an infinite two-dimensional grid.
# 
# Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward. For example, the first few squares are allocated like this:
#   
#   17  16  15  14  13
# 18   5   4   3  12
# 19   6   1   2  11
# 20   7   8   9  10
# 21  22  23---> ...
# While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the location of the only access port for this memory system) by programs that can only move up, down, left, or right. They always take the shortest path: the Manhattan Distance between the location of the data and square 1.
# 
# For example:
#   
#   Data from square 1 is carried 0 steps, since it's at the access port.
# Data from square 12 is carried 3 steps, such as: down, left, left.
# Data from square 23 is carried only 2 steps: up twice.
# Data from square 1024 must be carried 31 steps.
# How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?


##https://rosettacode.org/wiki/Spiral_matrix#R
spiral_matrix <- function(n) {
  spiralv <- function(v) {
    n <- sqrt(length(v))
    if (n != floor(n))
      stop("length of v should be a square of an integer")
    if (n == 0)
      stop("v should be of positive length")
    if (n == 1)
      m <- matrix(v, 1, 1)
    else
      m <- rbind(v[1:n], cbind(spiralv(v[(2 * n):(n^2)])[(n - 1):1, (n - 1):1], 
                               v[(n + 1):(2 * n - 1)]))
    return(m)
  }
  
  spiralv2 <- function(v) {
    n <- sqrt(length(v))
    if (n == 1)
      m <- matrix(v, 1, 1)
    else
      m <- rbind(cbind(v[(n^2-2*n+2):(n^2-n)],
                       spiralv(v[(n-1)^2]:1)
                       ),
                 v[(n^2-n+1):n^2]
                 )
    return(m)
  }
  spiralv2(1:(n^2))
}

newmatrix <- spiral_matrix(32)

#find coordinates
square <- 1024
numcoord <- which(newmatrix==square,arr.ind = TRUE)
basecoord <- which(newmatrix==1,arr.ind = TRUE)

sum(abs(numcoord - basecoord))

#Your puzzle input is 347991.
#need to change options because so many nested vars
options(expressions = 10000)
newmatrix <- spiralv2(1:625^2)

#find coordinates
square <- 347991
numcoord <- which(newmatrix==square,arr.ind = TRUE)
basecoord <- which(newmatrix==1,arr.ind = TRUE)

sum(abs(numcoord - basecoord))

#part 2
#As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1. Then, in the same allocation order as shown above, they store the sum of the values in all adjacent squares, including diagonals.
# 
# So, the first few squares' values are chosen as follows:
# 
# Square 1 starts with the value 1.
# Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
# Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
# Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
# Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
# Once a square is written, its value does not change. Therefore, the first few squares would receive the following values:

#What is the first value written that is larger than your puzzle input?



