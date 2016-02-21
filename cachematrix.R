## These functions use the <<- operator to assign a value to an object 
##in an environment that is different from the
##The firstirst: cache creates allows you to put a matric in the parent ##environment
## And the second checks if inverse matrix has already been cached and ##retrieve, if not, then create

## This function caches x = a matrix, sets inverseOfX to be NULL 

makeCacheMatrix <- function(x = matrix()) {
    inverseOfX <- NULL
    set <- function(y) {
      x <<- y
      inverseOfX <<- NULL
    }
    get <- function() x
    ## when setsolve is called, it will set inverseOfX in the parent
    ## environment (aka the inverseOfX field declared at line 9)
    ## to whatever is passed in. Hopefully you pass in the inverse of X!
    setsolve <- function(solveValue) inverseOfX <<- solveValue
    getsolve <- function() inverseOfX
    ##I added the return even though it isnt necessary, but it is clearer
    ##the makeCacehMatrix will return the following list:
    return(list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve))

}


## This function computes the inverse of the special "matrix" 
##returned by `makeCacheMatrix` above. If the inverse has already been 
##calculated (and the matrix has not changed), then `cacheSolve` should 
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseOfX <- x$getsolve()
  if(!is.null(inverseOfX)) {
    message("getting cached data")
    # return ends execution, so once we return, the other
    # stuff won't get computed. This effectively makes
    # data <- x$get(), etc, an "else" clause -- once we go into
    # this if, we can't do anything but return.
    return(inverseOfX)
  }
  data <- x$get()
  inverseOfX <- solve(data, ...)
  x$setsolve(inverseOfX)
  
  inverseOfX
}
