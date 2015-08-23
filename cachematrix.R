makeCacheMatrix <- function(b = matrix()) {
## Function returns a list with functions that set and get the matrix, set and get the inverse
## This is an input to cacheSolve
        
        invrs = NULL
        setval = function(a) {
                # assign value
                b <<- a
                invrs <<- NULL
        }
        getval = function() b
        setinvval = function(inverse) invrs <<- inverse 
        getinvval = function() invrs
        list(setval=setval, getval=getval, setinvval=setinvval, getinvval=getinvval)
}

cacheSolve <- function(b, ...) {
        ## returns the inverse of the original matrix
        
        invrs = b$getinvval()
        
        # retrieve the inverse if already calculated
        if (!is.null(invrs)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(invrs)
        }
        
        # calculate inverse 
        mat.data = b$getval()
        invrs = solve(mat.data, ...)
        
        # sets inverse in cache
        b$setinvval(invrs)
        
        # return value
        return(invrs)
}
