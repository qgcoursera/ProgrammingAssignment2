## Put comments here that give an overall description of what your
## functions do

## c <- makeCacheMatrix(x) creates a 'cacheable' object 'c' of a given matrix 'x',
## to be passed as a parameter to cacheResolve(m)

makeCacheMatrix <- function(x = matrix()) {
  ## Returned cache object of matrix x
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## i <- cacheSolve(c) returns the solve() result 'i' of a cacheable matrix object 'c' 
## (see makeCacheMatrix to create 'c')

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached solve of matrix data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
