
## Calculate the inverse matrix and store it in memory in an environment 
## outside the current environment. This way it is cached in memory.
## If the inverse matrix has already been previously calculated and stored, 
## it obtains the result of the cache memory avoiding to make the calculation again.


## Fuction makeMatrix
## Builder to create a matrix object and functions to interact with it in cache memory.

makeCacheMatrix <- function(x = matrix()) {
        mInv <- NULL
        set <- function(y) {
                x <<- y
                mInv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) mInv <<- solve
        getinv <- function() mInv 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Function cacheSolve 
## Return the inverse matrix of another matrix passed as an argument calling the Buider above.
## It checks whether the inverse matrix has been calculated previously, in which case it obtains it from memory. 
## Otherwise perform the calculations and store the result in memory

cacheSolve <- function(x, ...) {
        mInv <- x$getinv()
        if(!is.null(mInv)) {
                message("getting cached data")
                return(mInv)
        }
        data <- x$get()
        mInv <- solve(data, ...)
        x$setinv(mInv)
        mInv 
}
