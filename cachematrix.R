## makeCacheMatrix has 4 functions
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse matrix
## 4.get the value of the inverse  matrix



makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
 setMatrix <- function(y){
   X <<- y
   Inv <<- NULL 
 } 
getMatrix <- function() x
setInverse <- function(invrs) Inv <<- invrs
getInverse <- function() Inv
list(setMatrix = setMatrix, getMatrix = getMatrix, 
     setInverse = setInverse, getInverse = getInverse)
}


## Below function solves /finds the inverse of a matrix. 
## If inverse is available from cache, return it from cache
## if not, solve the matrix
cacheSolve <- function(x, ...) {
  ## get inverse
  inv <- x$getInverse()
  ## if already in cache, return value from cache
  if (!is.null(inv)) {
    message("Getting from Cache")
     return (inv)
  }
  ## solve the matrix because cache is null
  mtrx <- x$getMatrix()
  inv <- solve(mtrx)
  x$setInverse(inv)
  return(inv)
  
}
