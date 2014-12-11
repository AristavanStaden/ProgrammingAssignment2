## Functions as below are used to calculate the inverse of a matrix. 
## If the inverse has already been found, it is returned from cache & not calculated again.


### This function creates a list of functions that caches the inverse of the matrix. 


makeCacheMatrix <- function(x=matrix()) {
        m <- NULL
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m   
        list(get=get, 
             set=set,
             setInverse=setInverse, 
             getInverse=getInverse)
}

### Calculates the inverse of the matrix returned from the first function, unless it has already
### been calculated, in which case it'll return the data from the cache.

cacheSolve <- function(x) {
        m <- x$getInverse()
        if(!is.null(m)){
                message("Returning cached data")
                return(m)
        }
        else {
                message("Calculating inverse matrix...")
                data <- x$get() 
                m <- solve(data) 
                x$setInverse(m) 
                return(m)
        }
}