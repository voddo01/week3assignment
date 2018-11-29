## This function creates a list of functions that enable you to set the value
## of the matrix, get its value, set and get the inverse of the matrix.  

makeCacheMatrix <- function(x = matrix()) { 
                inv <- NULL
                set <- function(y) {                  ##The set function allows us to set
                        x <<- y                       ##new parameters of our matrix, and
                        inv <<- NULL                  ##clears the "inv" object when doing so
                }

                get <- function() {                   ##The get function lets us see the matrix
                                x
                }
                setinverse <- function(solve) {       ##The setinverse function sets the "inv"
                        inv <<-  solve                ##when we use the cacheSolve function
                }
                getinverse <- function() {            ##The getinverse function lets us see 
                                inv                   ##the inverse once it has been calculated
                }
                list(set = set, get = get,            ##This list lets us call each individual
                        setinverse = setinverse,      ##function with the '$' operator
                        getinverse = getinverse)
}


## This function calculates the inversion of matrix defined in the previous function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {                        ##First the function checks to see if
                message("getting cached data")     ##the matrix inversion has already been
                return(inv)                        ##calculated.  
        }
        data <- x$get()                 ##if it has not, we use the solve function
        inv <- solve(data)              ##to find the inverse of the matrix and 
        x$setinverse(inv)               ##cache the result in our makeCachematrix function.
        return(inv)                             
}
