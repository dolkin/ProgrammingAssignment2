## To finish the assignment, made two functions below.
## The first function, makeCacheMatrix is to make a inverse of a matrix following the guidance from the assignment.
## The secount function, cacheSolve is to print out the inverse of the matrix generated from makeCacheMatrix function.

## Below is the first function, makeCacheMatrix, to make a inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- matrix(nrow = nrow(x), ncol = ncol(x))
  set <- function(y) {
    x <<- y
    m <<- matrix(nrow = nrow(x), ncol = ncol(x))
  }
  get <- function() x
  setsolve <- function(inv) m <<- inv
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Below is the second function, cacheSolve, to print out the inverse of the matrix.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.na(m[1,1])) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}


## Below is the example with upper two functions.

set.seed(3)
mat <- matrix(sample(4, 4, replace = FALSE), nrow = 2, ncol = 2)  # make a 3 by 3 matrix randomly.

inv <- makeCacheMatrix(mat)  # put the matrix in the first function to make the inverse of the matrix.
cacheSolve(inv)  # print it out.
mat %*% cacheSolve(inv)  # check out.