spiral_matrix <- function(n) {
  #stopifnot(is.numeric(n))
  #stopifnot(n > 0)
  steps <- c(1, n, -1, -n)
 # steps <- c(n, 1, -1, -n)
  reps <- n - seq_len(n * 2 - 1L) %/% 2
  indicies <- rep(rep_len(steps, length(reps)), reps)
  indicies <- cumsum(indicies)
  values <- integer(length(indicies))
  values[indicies] <- seq_along(indicies)
  matrix(values, n, n, byrow = TRUE)
}
spiral_matrix(2)

matrix(seq(1,3**2),3,3,byrow = TRUE)


seq(1,9)
 