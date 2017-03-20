## Function makeCacheMatrix() incorporates the following functionalities for a matrix
## 		a) get() function - Gets the matrix value
## 		b) set() function - Assign the passed matrix value to the parent matrix variable
##	 						and reset the parent matrix inverse variable to NULL
## 		c) getinverse() function - Gets the inverse matrix value
##		c) setinverse() function - Assign the passed matrix value to the parent matrix 
##								   inverse variable

makeCacheMatrix <- function(x = matrix()) {
        matrixinverse <- NULL
        set <- function(y) {
                x <<- y
                matrixinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(minverse) matrixinverse <<- minverse
        getinverse <- function() matrixinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function returns the inverse of a matrix 'x'. The function assumes, that input matrix is always invertible.
## Initially, the function tries to retrieve the inverse value from the cache using the getinverse() function.
## If the inverse is already computed, it skips the inverse computation, and retrives the inverse values from the cache.
## If the inverse is not available, it calculates the inverse of the matrix and assigns the value of the inverse to the 
## cache via the setinverse() function.

cacheSolve <- function(x, ...) {
		matrixinverse <- x$getinverse()
        if(!is.null(matrixinverse)) {
                message("getting cached data")
                return(matrixinverse)
        }
        data <- x$get()
        matrixinverse <- solve(data, ...)
        x$setinverse(matrixinverse)
        matrixinverse
}

## Sample Run
## > m <- matrix(11:14,2,2)
## > mx <- makeCacheMatrix(m)
## > mx$get()
## [,1] [,2]
## [1,]   11   13
## [2,]   12   14
## > 
## First run - No Cache data available
## > cacheSolve(mx)
## [,1] [,2]
## [1,]   -7  6.5
## [2,]    6 -5.5
## > 
##
## 2nd Run - Invserse retrieved from cache itself
## > cacheSolve(mx)
## getting cached data
## [,1] [,2]
## [1,]   -7  6.5
## [2,]    6 -5.5
## > 

