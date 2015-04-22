## the two functions cache the inverse of a matrix, which 
## save the time of calcualtion 

## This function does the following things: set the value of a 
## matrix, get the value of a matrix, set the value of the inverse
## of the matrix, and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set  <- function(y) {
    x <<- y
    I <<- matrix()
  }
  get <- function() x
  setInverse <- function(inverse) I <<- inverse 
  getInverse <- function() I
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## based on the obove function, this function tells whether the inverse
## of a matrix has been calculated or not, if the inverse has been calculated,
## it will return the cached inverse, otherwise it will calculate the inverse
## of the matrix

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setInverse(i)
  i

}
