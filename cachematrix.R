## The following functions sets the parameter values of a special matrix and its inverse
## in the cache and re-evaluates the same if the input matrix changes

## The said function initializes the conditions and prepares the overall sub function set

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     ##sub function to set the values of parameters
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     
     ##sub function to get the cached data
     get <- function() x
     
     ##sub function to set the value on inverse to cache
     setinv <- function(minv) inv <<- minv
     
     ##sub function to get the cached inverse value
     getinv <- function() inv
     
     ##return list highlighting the various sub functions
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The said function gets the value of inverse from cache if no conditions have change
## else reevaluates the same

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getinv()
     if(!is.null(inv)){
          message("Reading from cache")
          return(inv)
     }
     mat <- x$get()
     inv <- solve(mat)
     x$setinv(inv)
     inv
}
