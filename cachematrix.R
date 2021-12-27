## The interaction of makeCacheMatrix and cacheSolve makes it possible to cache the inverse of 
## a matrix thereby avoiding having to inverse the same matrix multiple times. 


## makeCacheMatrix creates an object of the makeCacheMatrix type that holds functions to set and get
## the matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
      x <<- y
      im <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) im <<- inverse
    getinverse <- function() im
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## cacheSolve checks if the inverse for the matrix (held in the makeCacheMatrix object) has 
## already been calculated. If not it calculates it, stores it in cache and returns it.

cacheSolve <- function(x, ...) {
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im 
}
