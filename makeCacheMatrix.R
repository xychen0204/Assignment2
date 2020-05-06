makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## initialize
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x  ## set the function and return the matrix
  setinverse <- function(solve) m <<- solve  ## get the inverse matrix
  getinverse <- function() m
  ## Return the list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheinverse <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get() ## get the matrix
        m <- solve(data, ...) ## get the inverse result
        x$setinverse(m)
        m  ## return the matrix
}
