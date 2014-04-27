##This code calculates and stores the inverse of a square matrix. If the matrix is stored in the cache it gets its value instead of doing the calculation. 

##Returns a list of functinos for storing and retrieving tha inverse value of a matrix
makeCacheMatrix <- function(x = matrix()) {
        ##Set the matrix as null
        m <- NULL        
        set <- function(y) {
                ##Save 'y' as 'x'. and store both 'x' and 'm' in another environment. 
                x <<- y
                m <<- NULL
        }
        ## Read 'x'
        get <- function() x
        ##  Calculate the inverse
        setInverse <- function(solve) m <<- solve
        ## Retrieve 'm'
        getInverse <- function() m
        ## List of functions to get or set the value, or to calculate and set it if not stored.
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

##This function gets or calculates the inverse of a matrix comparing to a saved value for time saving
cacheSolve <- function(x, ...) {
        ##Get the inverse stored value
        m <- x$getInverse()
        ##If the stored value is not NULL, then use it as the result and exit
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##If the stored value is NULL, get 'x', calculate the inverse 'm', store its value and return it.
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}