## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
