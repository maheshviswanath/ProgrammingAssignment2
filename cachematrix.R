## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## variable to store inverse
  set <- function(temp = matrix()){ ## function to set variables
    x <<- temp
    inv <<- NULL
  }
  get <- function() x ## function to return original matrix
  setInverse <- function(solveMatrix = matrix()) inv <<- solve(solveMatrix) ## function to create inverse
  getInverse <- function() inv ## function to return inverse of matrix
  list(set = set, get = get, ## return list of functions
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached inverse")
    return(inverseMatrix)
  }
  origMatrix <- x$get()
  inverseMatrix <- solve(origMatrix)
  x$setInverse(inverseMatrix)
  inverseMatrix
}

## a <- c(1,2,3,0,1,4,5,6,0)
## l <- c(7,0,-3,2,3,4,1,-1,-2)
## source("cachematrix.R")
## b <- matrix(a,ncol = 3, nrow = 3)