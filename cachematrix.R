## Coursera course "R Programming 2016"
## Programming assignment 2 lexical scoping
## Name author: Lianne de Vries

## Functions makeCacheMatrix and cacheSolve
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. The pair of functions below create a special "matrix" object that can cache its inverse.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
      x <<- y
      invmat <<- NULL
  }

  get <- function() x
  setmatrix <- function(solve) invmat <<- solve
  getmatrix <- function() invmat
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getmatrix()
  if(!is.null(invmat)){
    message("getting cached data")
    return(invmat)
  }
  normatrix <- x$get()
  invmat <- solve(normatrix, ...)
  x$setmatrix(invmat)
  invmat
}

