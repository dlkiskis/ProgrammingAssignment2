## This is my response for Programming Assignment 2.
## The functions in this file will create an object that can cache
## a matrix and its inverse.

## example usage: (using the hilbert function from the man page for solve())
## > hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## > h8 <- hilbert(8);
## > my_matrix <- makeCacheMatrix(h8)
## > cacheSolve(my_matrix)

## The first call should return the inverse.  Subsequent calls will print
## "getting cached data" before returning the inverse

## makeCachematrix is the "constructor" for the object.  Pass in a square matrix
## to be stored.  use $get and $set to change the value.  $getinv and $setinv to 
## get and set the inverse

makeCacheMatrix <- function(x = matrix()) {
  # cache for the inverse of the matrix
  inv <- NULL
  # set and get for the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  # set and get for the inverse
  setinv <- function(loc_inverse) inv <<- loc_inverse
  getinv <- function() inv
  # return the list of functions from this make function
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calls solve() to get the inverse of a matrix that was saved using
## makeCacheMatrix(). 
## after the first time the function is called, the inverse will be cached so it 
## is not computed in subsequent calls.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # get the cached value, could be NULL if not yet computed and cached
  inv <- x$getinv()
  # if not null, i.e., cached, return that value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # previous value was not calculated, so calculate it using solve()
  data <- x$get()
  inv <- solve(data, ...)
  
  # cache the calculated value back in x
  x$setinv(inv)
  # return the inverse matrix
  inv
}
