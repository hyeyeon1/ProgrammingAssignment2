## these functions are used to assign a value to an object in an environment that is different from the current environment.
## it is useful to create a special object that stores a numeric vector and cache's its mean.

## this function makeVector creates a special "vector".

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
        mat <<- y
        inv <<- NULL
        }
        
        get <- function() mat
        setInverse <- function(solve) inv <<- solve
        getInverse <- function() inv
        list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## the following function calculates the mean of the special "vector" created with the above function.
## the special thing of this function is that it checks to see if the mean has already been calculated at first. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
        if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
