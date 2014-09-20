## A pair of functions that cache the inverse fo a matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function( matrix ){
    x <<- matrix
    i <<- NULL
  }
  get <- function() {
    x
  }
  set_inverse <- function(inverse){
    i <<- inverse
  }
  get_inverse <- function(){
    i
  }
  list(set = set, get = get, set_inverse = set_inverse, 
       get_inverse = get_inverse)
  
}


## The function below computes the inverse of the special matrix returned
## by makeCacheMatrix above. If the inverse has already been calculated 
## and the matrix has not changed, then the cachesolve shoudl retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  if(!is.null(m)){
    return(m)
  }
  new_m <- x$get()
  m <- solve(new_m) %*% new_m
  x$set_inverse(m)
}
