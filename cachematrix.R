## These functions (makeCacheMatrix and cachesolve) cache the potentially
## time consuming computation of a matrix inverse. If the contents of the
## matrix are not changing, we can cache the value of the inverse, so that
## when we need it again, it can be retrieved from the cache rather than 
## recomputing it.  

## Function makeCacheMatrix creates a special matrix object 
## that can cache its inverse. The function returns a list
## of functions.

makeCacheMatrix <- function(x=matrix) {
  m <- NULL
  
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  
  get <- function() x      
  
  setinverse <- function(solve) m <<- solve(x)
  
  getinverse <- function() m
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the special matrix
## returned by makeCacheMatrix above. If the inverse has already been
## calculated and the matrix has not changed, then the cachesolve function
## should retrieve the inverse from the cache and return a message.

cacheSolve <- function(x, ...) {
 
  m <- x$getinverse()
  if(!is.null(m)) {
    message ("getting cached data")
    return(m)
  }
  
  mdata <- x$get()
  m <- solve(mdata, ...)
  x$setinverse(m)
  m
}

