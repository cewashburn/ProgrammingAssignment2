## Creates and caches the inverse of a matrix. makeChacheMatrix creates a matrix
##and cacheSolve calculates and caches the inverse or else reports the cached value

## Creates a matrix capable of caching its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


##checks if the inverse of x has been cached. if not, calculates the inverse and caches it to be called later.

cacheSolve <- function(x, ...) {
        inv <-x$getinverse()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
