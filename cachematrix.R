## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function(y) {
        x <<- y;
        i <<- NULL;
    }
    get <- function() x;
    setinv <- function(inv) i <<- inv;
    getinv <- function() i;
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
        inv <- x$getinv()
        if(!is.null(i)) {
            message("Getting cached data...")
            return(i)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv

        
        
}
