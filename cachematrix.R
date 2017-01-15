## Functions for computing the inverse of a matrix 
## and storing its value in a special "matrix" object (a list) 
## not to recompute the inverse when called again for the same matrix.


## makeCacheMatrix creates a special "matrix" object in the form of a list
## that initiates a matrix and stores its inverse.It provides getters and setters
## for the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## cacheSolve returns the inverse of the matrix passed as its argument
## (an object of type makeCacheMatrix) from cache if already computed
## or computes the inverse and stores it 
## to cache in the makeCacheMatrix object passed as argument

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
