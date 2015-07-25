## These two functions will allow you to input a matrix, calculate its inverse, and cache this
## answer for later use.

## The first function creates four nested functions which allow you to set the matrix, get it,
## set a matrix inverse and get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrixInv <- function(solve) m <<- solve
        getMatrixInv <- function() m
        list(set = set, get = get,
             setMatrixInv = setMatrixInv,
             getMatrixInv = getMatrixInv)
}


## If the inverse of the matrix has not been changed from the one set in the makeCacheMatrix function
## then the cacheSolve function will get the cached data from the above function and state it is doing
## so.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrixInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrixInv(m)
        m
}
