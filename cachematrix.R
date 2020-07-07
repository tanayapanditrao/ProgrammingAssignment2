## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix will generate the special matrix. 

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <-function(y){
    x <<- y
    minv <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse)
  {
    minv <<- inverse
  }
  getInverse <- function(){
    minv
  }
  list(set = set, get = get,
       setInverse = setInverse , getInverse = getInverse)
  

}


## Write a short comment describing this function
## cachesolve() function will compute the inverse of the matrix
##returned by makeCacheMatrix. If inverse is already calculated,
##it returns the inverse from cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getInverse()
  if(!is.null(minv)){
    message("getting cached data")
    return(minv)
  }
  cdata <- x$get()
  minv <- solve(cdata, ...)
  x$setInverse(minv)
  minv
}
