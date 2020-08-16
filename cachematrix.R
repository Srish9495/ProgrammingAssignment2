## The function makeCacheMatrix creates the matrix object and gets/sets the inverse of the matrix object.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m 
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
