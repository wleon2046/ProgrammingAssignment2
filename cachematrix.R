## This is my assignment to write a pair of functions(FUNCTION1&FUNCTION2) 
## that cache the inverse of a matrix.

## FUNCTION1：
## This function creates a special "matrix" object that can cache its inverse,
## For example: MATR=matrix(1:4, 2, 2)     
##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4

makeCacheMatrix <- function(x = matrix()) {    ## 'x' is formal parameter,'MATR' is actual parameters. 
    m <- NULL
        set <- function(y) {                   ## 'set' is executed by default.
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## FUNCTION2：
## This function computes the inverse of the special 'matrix' returned by 
## makeCacheMatrix above. If the inverse has already been calculated,
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {               ## this 'x' is formal parameter,
					       ## actual value is 'makeCacheMatrix(MATR)' above.
## Return a matrix that is the inverse of 'makeCacheMatrix(MATR)'
 m <- x$getsolve ()
        if(!is.null(m)) {
                message("getting cached data") ## if 'm' is not 'null',then print
				               ## 'getting cached data' before output.
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)                  ## if 'm' is 'null', then calculate
					       ## the inverse of 'MATR' to 'm'
        x$setsolve(m)
        m
}
-----------------------------------------------------------------------------------------
## Below is the process executed process, and the result is CORRECT!
##
## > MATR=matrix(1:4, 2, 2)
## > MATR
##        [,1] [,2]
##  [1,]    1    3
##  [2,]    2    4
## > 
## > makeCacheMatrix(MATR)
## $set
## function (y) 
## {
##     x <<- y
##     m <<- NULL
## }
## <environment: 0x0000000016bf65a8>
## 
## $get
## function () 
## x
## <environment: 0x0000000016bf65a8>
## 
## $setsolve
## function (solve) 
## m <<- solve
## <environment: 0x0000000016bf65a8>
## 
## $getsolve
## function () 
## m
## <environment: 0x0000000016bf65a8>
## 
## > cacheSolve(makeCacheMatrix(MATR))
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 
