## Creating functions that cache the inverse of a matrix

## function that creates matrix object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## function that computes the inverse of the matrix

cacheSolve <- function(x, ...) {
       inv <- x$getInverse()
       if(!is.null(inv)){
         message("getting inverse")
         return(inv)
       }
       mat <- x$get()
       inv <- solve(mat,...)
       x$setInverse(inv)
       inv
}
