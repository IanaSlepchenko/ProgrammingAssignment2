## Part 1 - makeCacheMatrix:

makeCacheMatrix <- function(x = matrix()) {
  p <- NULL
  set <- function(y) {
    x <<- y
    p <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) p <<- inverse
  getInverse <- function() p
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##Part 2 - cacheSolve: 

cacheSolve <- function(x, ...) {
    p <- x$getInverse()
  if (!is.null(p)) {
    message("getting cached data")
    return(p)
  }
  g <- x$get()
  p <- solve(g, ...)
  x$setInverse(p)
  p
}
